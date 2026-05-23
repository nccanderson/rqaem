#' Run RQA on each group of a data frame
#'
#' Tidy-eval entry point for the common case of a long-format fixation
#' data frame with one row per fixation and one or more grouping
#' columns (trial, participant, image, ...). Returns a single tibble
#' with one row per group and the scalar RQA metrics as columns. The
#' recurrence matrices themselves are not materialised in the output —
#' call [rqa()] on a single group when you need the matrix.
#'
#' Supply either `x` and `y` (for xy fixation coordinates) **or**
#' `category` (for categorical RQA), but not both.
#'
#' @param data A data frame.
#' @param x,y Tidy-eval column references for the xy coordinates.
#'   Mutually exclusive with `category`.
#' @param category Tidy-eval column reference for a categorical
#'   sequence (e.g. region-of-interest codes). Mutually exclusive with
#'   `x` / `y`.
#' @param duration Optional tidy-eval column reference for fixation
#'   durations. When non-`NULL`, the duration-weighted variant of RQA
#'   is used.
#' @param by Character vector of column names to group by.
#' @param radius Threshold radius for the recurrence matrix.
#' @param line_length Minimum line length for diagonal / vertical line
#'   measures. Defaults to `2L`.
#' @param min_cluster Minimum component size (in cells) for the
#'   `clusters` metric. Defaults to `8L`.
#'
#' @return A tibble: the grouping columns followed by the 13 scalar
#'   metrics (`n`, `nrec`, `rec`, `det`, `revdet`, `meanline`,
#'   `maxline`, `ent`, `relent`, `lam`, `tt`, `corm`, `clusters`),
#'   one row per group.
#'
#' @examples
#' set.seed(1)
#' eyedat <- data.frame(
#'   trial = rep(1:3, each = 20),
#'   x     = runif(60, 0, 1000),
#'   y     = runif(60, 0, 1000)
#' )
#' rqa_by(eyedat, x = x, y = y, by = "trial", radius = 100)
#'
#' @export
rqa_by <- function(data, x = NULL, y = NULL, category = NULL,
                   duration = NULL, by, radius,
                   line_length = 2L, min_cluster = 8L) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (missing(by) || !is.character(by) || length(by) == 0L) {
    stop("`by` must be a non-empty character vector of column names.",
         call. = FALSE)
  }
  if (missing(radius)) {
    stop("`radius` is required.", call. = FALSE)
  }
  missing_by <- setdiff(by, names(data))
  if (length(missing_by) > 0L) {
    stop("Grouping columns not in `data`: ",
         paste(missing_by, collapse = ", "), call. = FALSE)
  }

  x_q        <- rlang::enquo(x)
  y_q        <- rlang::enquo(y)
  category_q <- rlang::enquo(category)
  duration_q <- rlang::enquo(duration)

  x_vec        <- .resolve_data_column(x_q,        data, "x")
  y_vec        <- .resolve_data_column(y_q,        data, "y")
  category_vec <- .resolve_data_column(category_q, data, "category")
  duration_vec <- .resolve_data_column(duration_q, data, "duration")

  has_xy  <- !is.null(x_vec) && !is.null(y_vec)
  has_cat <- !is.null(category_vec)
  if (has_xy == has_cat) {
    stop("Supply either `x` and `y`, or `category`, but not both / neither.",
         call. = FALSE)
  }
  if (!is.null(x_vec) && is.null(y_vec)) {
    stop("`y` is required when `x` is supplied.", call. = FALSE)
  }
  if (is.null(x_vec) && !is.null(y_vec)) {
    stop("`x` is required when `y` is supplied.", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    return(.empty_rqa_by_tibble(data, by))
  }

  group_keys <- data[, by, drop = FALSE]
  group_id   <- do.call(paste, c(lapply(group_keys, as.character),
                                 sep = "\r"))
  # Preserve the order in which groups first appear.
  group_id <- factor(group_id, levels = unique(group_id))
  group_idx <- split(seq_len(nrow(data)), group_id)

  rows <- lapply(group_idx, function(idx) {
    fix <- if (has_xy) {
      cbind(x_vec[idx], y_vec[idx])
    } else {
      category_vec[idx]
    }
    dur <- if (!is.null(duration_vec)) duration_vec[idx] else NULL
    res <- rqa(fix,
               radius      = radius,
               line_length = line_length,
               min_cluster = min_cluster,
               duration    = dur)
    cbind(
      group_keys[idx[1L], , drop = FALSE],
      as.data.frame(res)
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  tibble::as_tibble(out)
}

# Helpers --------------------------------------------------------------

# Resolve a tidy-eval column quosure to a vector of values from `data`,
# or return NULL if the quosure was the default `NULL`.
.resolve_data_column <- function(quo, data, arg_name) {
  if (rlang::quo_is_null(quo)) {
    return(NULL)
  }
  expr <- rlang::quo_get_expr(quo)
  if (is.character(expr) && length(expr) == 1L) {
    name <- expr
  } else if (is.name(expr) || is.symbol(expr)) {
    name <- rlang::as_name(quo)
  } else {
    stop(sprintf("`%s` must be a bare column name or a string.", arg_name),
         call. = FALSE)
  }
  if (!name %in% names(data)) {
    stop(sprintf("Column `%s` (passed as `%s`) not found in `data`.",
                 name, arg_name),
         call. = FALSE)
  }
  data[[name]]
}

# Build an empty tibble with the right columns when `data` has no rows.
.empty_rqa_by_tibble <- function(data, by) {
  group_part <- data[integer(0L), by, drop = FALSE]
  metric_part <- tibble::tibble(
    n        = integer(0L),
    nrec     = numeric(0L),
    rec      = numeric(0L),
    det      = numeric(0L),
    revdet   = numeric(0L),
    meanline = numeric(0L),
    maxline  = numeric(0L),
    ent      = numeric(0L),
    relent   = numeric(0L),
    lam      = numeric(0L),
    tt       = numeric(0L),
    corm     = numeric(0L),
    clusters = numeric(0L)
  )
  tibble::as_tibble(cbind(group_part, metric_part))
}
