#' Bootstrap null distributions for RQA metrics
#'
#' Generates a null distribution by repeatedly resampling the fixation
#' sequence and re-running [rqa()] on each resample, then returns the
#' resulting scalar metrics in long tibble form. Two null types are
#' supported, matching the strategies used in the paper:
#'
#' * `type = "shuffle"` permutes the fixation order while keeping the
#'   set of positions (and durations) fixed. Preserves the spatial
#'   distribution; breaks temporal structure.
#' * `type = "uniform"` draws each resample's fixations independently
#'   from a uniform distribution on `screen = c(width, height)`.
#'   Preserves only the screen extents; breaks both spatial clustering
#'   and temporal structure.
#'
#' @param fixations Numeric matrix / data.frame with two columns
#'   (`x`, `y`), or a numeric / integer vector of categorical codes.
#' @param radius Threshold radius forwarded to [rqa()].
#' @param line_length,min_cluster Forwarded to [rqa()].
#' @param duration Optional duration vector forwarded to [rqa()].
#'   Under `type = "shuffle"`, durations are permuted alongside the
#'   fixations they were paired with.
#' @param n Number of resamples. Defaults to `999`.
#' @param type One of `"shuffle"` (default) or `"uniform"`.
#' @param screen Numeric vector `c(width, height)` defining the
#'   sampling region for `type = "uniform"`. Required for that type
#'   and ignored otherwise.
#' @param seed Optional integer seed. When supplied, the resamples are
#'   reproducible across machines and R sessions; the global RNG state
#'   is restored on exit.
#' @param metrics Character vector of scalar RQA metrics to record per
#'   resample. Defaults to the six emphasised in the paper.
#'
#' @return A tibble of class `rqa_bootstrap`: one row per resample,
#'   one column per requested metric plus a `resample` index column.
#'   The empirical (observed) metrics are stored as the `"observed"`
#'   attribute (a one-row tibble), and the call parameters as
#'   `"params"`.
#'
#' @examples
#' set.seed(1)
#' fix <- cbind(runif(30, 0, 1000), runif(30, 0, 1000))
#' boot <- rqa_bootstrap(fix, radius = 80, n = 19, seed = 42)
#' summary(boot)
#'
#' @export
rqa_bootstrap <- function(fixations, radius,
                          line_length = 2L, min_cluster = 8L,
                          duration = NULL,
                          n = 999L,
                          type = c("shuffle", "uniform"),
                          screen = NULL,
                          seed = NULL,
                          metrics = c("rec", "det", "lam", "tt",
                                      "corm", "ent")) {
  type <- match.arg(type)
  if (!is.numeric(n) || length(n) != 1L || n < 1L) {
    stop("`n` must be a positive integer.", call. = FALSE)
  }
  n <- as.integer(n)
  bad_metrics <- setdiff(metrics, .rqa_metric_names)
  if (length(bad_metrics)) {
    stop("Unknown metrics requested: ",
         paste(bad_metrics, collapse = ", "), call. = FALSE)
  }

  fx <- .coerce_fixations(fixations)
  n_fix <- nrow(fx)
  if (n_fix <= 1L) {
    stop("Need at least 2 fixations to bootstrap.", call. = FALSE)
  }
  is_xy <- ncol(fx) == 2L

  if (type == "uniform") {
    if (is.null(screen) || !is.numeric(screen) || length(screen) != 2L) {
      stop("`screen = c(width, height)` is required when `type = \"uniform\"`.",
           call. = FALSE)
    }
    if (!is_xy) {
      stop("`type = \"uniform\"` requires 2-column xy fixations.",
           call. = FALSE)
    }
  }

  if (!is.null(duration) && length(duration) != n_fix) {
    stop("`duration` must have one entry per fixation.", call. = FALSE)
  }

  if (!is.null(seed)) {
    .restore_rng <- .install_rng_guard()
    on.exit(.restore_rng(), add = TRUE)
    set.seed(seed,
             kind = "Mersenne-Twister",
             normal.kind = "Inversion",
             sample.kind = "Rejection")
  }

  observed <- rqa(fixations, radius = radius,
                  line_length = line_length, min_cluster = min_cluster,
                  duration = duration)

  rows <- matrix(NA_real_, nrow = n, ncol = length(metrics))
  colnames(rows) <- metrics
  for (i in seq_len(n)) {
    if (type == "shuffle") {
      idx <- sample.int(n_fix)
      new_fix <- if (is_xy) fx[idx, , drop = FALSE] else as.vector(fx[idx, 1L])
      new_dur <- if (!is.null(duration)) duration[idx] else NULL
    } else {
      new_fix <- cbind(
        stats::runif(n_fix, 0, screen[1L]),
        stats::runif(n_fix, 0, screen[2L])
      )
      new_dur <- duration
    }
    res <- rqa(new_fix, radius = radius,
               line_length = line_length, min_cluster = min_cluster,
               duration = new_dur)
    for (m in metrics) rows[i, m] <- as.numeric(res[[m]])
  }

  observed_row <- tibble::as_tibble(
    as.list(vapply(metrics, function(m) as.numeric(observed[[m]]),
                   numeric(1L)))
  )

  out <- tibble::tibble(resample = seq_len(n))
  for (m in metrics) out[[m]] <- rows[, m]

  structure(
    out,
    observed = observed_row,
    params = list(
      radius = radius, line_length = line_length,
      min_cluster = min_cluster, duration = duration,
      n = n, type = type, screen = screen, seed = seed,
      metrics = metrics
    ),
    class = c("rqa_bootstrap", class(out))
  )
}

#' Print an RQA bootstrap result
#'
#' Prepends a one-line header with the bootstrap type and resample
#' count, then defers to the inherited tibble print.
#'
#' @param x An `rqa_bootstrap` from [rqa_bootstrap()].
#' @param ... Forwarded to the next method.
#'
#' @return `x`, invisibly.
#' @export
print.rqa_bootstrap <- function(x, ...) {
  p <- attr(x, "params")
  cat(sprintf("<rqa_bootstrap> (%s, n = %d, seed = %s)\n",
              p$type, p$n,
              if (is.null(p$seed)) "NULL" else format(p$seed)))
  NextMethod()
}

#' Summarise an RQA bootstrap result
#'
#' Reports the observed value, an empirical one-sided right-tail
#' p-value, and a percentile-bootstrap confidence interval at the
#' requested level for each metric. The p-value is computed with the
#' Davison & Hinkley correction:
#'
#' \deqn{p = (\#\{boot \ge observed\} + 1) / (n + 1).}
#'
#' @param object An `rqa_bootstrap` from [rqa_bootstrap()].
#' @param level Confidence level for the percentile interval.
#'   Defaults to `0.95`.
#' @param digits Number of digits for the printed table.
#' @param ... Ignored.
#'
#' @return A tibble (invisibly) with one row per metric and columns
#'   `metric`, `observed`, `p_value`, `lo`, `hi`.
#' @export
summary.rqa_bootstrap <- function(object, level = 0.95, digits = 4, ...) {
  if (!is.numeric(level) || level <= 0 || level >= 1) {
    stop("`level` must be in (0, 1).", call. = FALSE)
  }
  obs <- attr(object, "observed")
  params <- attr(object, "params")
  metrics <- params$metrics
  alpha <- 1 - level

  rows <- lapply(metrics, function(m) {
    boot <- object[[m]]
    obs_v <- obs[[m]]
    n_valid <- sum(!is.na(boot))
    p_value <- if (is.na(obs_v) || n_valid == 0L) {
      NA_real_
    } else {
      (sum(boot >= obs_v, na.rm = TRUE) + 1) / (n_valid + 1)
    }
    qs <- if (n_valid > 0L) {
      stats::quantile(boot, c(alpha / 2, 1 - alpha / 2),
                      na.rm = TRUE, names = FALSE)
    } else {
      c(NA_real_, NA_real_)
    }
    tibble::tibble(
      metric   = m,
      observed = obs_v,
      p_value  = p_value,
      lo       = qs[1L],
      hi       = qs[2L]
    )
  })
  table <- do.call(rbind, rows)

  cat(sprintf("<rqa_bootstrap> summary (%s, n = %d, %.0f%% CI)\n",
              params$type, params$n, level * 100))
  print(as.data.frame(table), digits = digits, row.names = FALSE)
  invisible(table)
}

# Install an on-exit guard that snapshots the current .Random.seed and
# restores it when invoked. The RNG kind is encoded in
# .Random.seed[1] so a single assign() restores both the algorithm and
# the state.
.install_rng_guard <- function() {
  saved_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) {
    get(".Random.seed", envir = .GlobalEnv)
  } else NULL
  function() {
    if (is.null(saved_seed)) {
      if (exists(".Random.seed", envir = .GlobalEnv)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    } else {
      assign(".Random.seed", saved_seed, envir = .GlobalEnv)
    }
  }
}
