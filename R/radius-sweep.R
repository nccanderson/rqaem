#' Sweep RQA metrics across a grid of candidate radii
#'
#' Runs [rqa()] at every radius in `radii` and returns a long tibble
#' with one row per (radius, metric) pair. Optionally runs a
#' [rqa_bootstrap()] at each radius to attach a null-baseline column
#' (mean) and a percentile-bootstrap interval (`boot_lo`, `boot_hi`)
#' alongside the observed value.
#'
#' Reproduces the input for Fig. 7 of Anderson et al. (2013), which
#' plots `rec` against radius with a bootstrap ribbon as a way to
#' choose a working radius.
#'
#' @param fixations Numeric matrix / data.frame with two columns
#'   (`x`, `y`), or a numeric / integer vector of categorical codes.
#' @param radii Numeric vector of candidate radii to sweep across.
#' @param line_length,min_cluster Forwarded to [rqa()].
#' @param duration Optional duration vector forwarded to [rqa()].
#' @param bootstrap_n If `> 0`, runs [rqa_bootstrap()] with this many
#'   resamples at each radius and attaches `boot_mean` / `boot_lo` /
#'   `boot_hi` columns. Defaults to `0` (no bootstrap).
#' @param bootstrap_type Forwarded to [rqa_bootstrap()] when
#'   `bootstrap_n > 0`. Defaults to `"shuffle"`.
#' @param screen Required when `bootstrap_type = "uniform"`.
#' @param metrics Character vector of scalar RQA metrics to record.
#'   Defaults to the four most commonly swept (`rec`, `det`, `lam`,
#'   `corm`).
#' @param seed Optional integer seed; affects the bootstrap path only.
#'   When supplied, the global RNG state is restored on exit so the
#'   sweep is idempotent across calls.
#' @param level Confidence level for the percentile interval when a
#'   bootstrap is run. Defaults to `0.95`.
#'
#' @return A tibble of class `radius_sweep` with one row per
#'   (radius, metric) pair: `radius`, `metric`, `value`. When
#'   `bootstrap_n > 0`, three more columns are appended:
#'   `boot_mean`, `boot_lo`, `boot_hi`.
#'
#' @examples
#' set.seed(1)
#' fix <- cbind(runif(40, 0, 800), runif(40, 0, 600))
#' radius_sweep(fix, radii = seq(40, 200, by = 40))
#'
#' @export
radius_sweep <- function(fixations, radii,
                         line_length = 2L, min_cluster = 8L,
                         duration = NULL,
                         bootstrap_n = 0L,
                         bootstrap_type = c("shuffle", "uniform"),
                         screen = NULL,
                         metrics = c("rec", "det", "lam", "corm"),
                         seed = NULL,
                         level = 0.95) {
  bootstrap_type <- match.arg(bootstrap_type)
  if (!is.numeric(radii) || length(radii) == 0L) {
    stop("`radii` must be a non-empty numeric vector.", call. = FALSE)
  }
  if (any(radii <= 0)) {
    stop("All entries in `radii` must be positive.", call. = FALSE)
  }
  bad <- setdiff(metrics, .rqa_metric_names)
  if (length(bad)) {
    stop("Unknown metrics requested: ",
         paste(bad, collapse = ", "), call. = FALSE)
  }
  if (!is.numeric(bootstrap_n) || length(bootstrap_n) != 1L ||
      bootstrap_n < 0L) {
    stop("`bootstrap_n` must be a non-negative integer.", call. = FALSE)
  }
  bootstrap_n <- as.integer(bootstrap_n)
  if (!is.numeric(level) || level <= 0 || level >= 1) {
    stop("`level` must be in (0, 1).", call. = FALSE)
  }

  if (!is.null(seed)) {
    .restore_rng <- .install_rng_guard()
    on.exit(.restore_rng(), add = TRUE)
    set.seed(seed,
             kind = "Mersenne-Twister",
             normal.kind = "Inversion",
             sample.kind = "Rejection")
  }

  alpha <- 1 - level

  rows <- vector("list", length(radii))
  for (k in seq_along(radii)) {
    r <- radii[k]
    obs <- rqa(fixations, radius = r,
               line_length = line_length, min_cluster = min_cluster,
               duration = duration)
    obs_vals <- unname(vapply(metrics, function(m) as.numeric(obs[[m]]),
                              numeric(1L)))

    if (bootstrap_n > 0L) {
      boot <- rqa_bootstrap(fixations, radius = r,
                            line_length = line_length,
                            min_cluster = min_cluster,
                            duration = duration,
                            n = bootstrap_n,
                            type = bootstrap_type,
                            screen = screen,
                            seed = NULL,
                            metrics = metrics)
      boot_mean <- unname(vapply(metrics, function(m) {
        v <- boot[[m]]
        if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE)
      }, numeric(1L)))
      boot_qs <- unname(vapply(metrics, function(m) {
        v <- boot[[m]]
        if (all(is.na(v))) c(NA_real_, NA_real_) else {
          unname(stats::quantile(v, c(alpha / 2, 1 - alpha / 2),
                                 na.rm = TRUE))
        }
      }, numeric(2L)))
      rows[[k]] <- tibble::tibble(
        radius    = r,
        metric    = metrics,
        value     = obs_vals,
        boot_mean = boot_mean,
        boot_lo   = boot_qs[1L, ],
        boot_hi   = boot_qs[2L, ]
      )
    } else {
      rows[[k]] <- tibble::tibble(
        radius = r,
        metric = metrics,
        value  = obs_vals
      )
    }
  }

  out <- do.call(rbind, rows)
  out$metric <- factor(out$metric, levels = metrics)

  structure(
    out,
    params = list(
      radii = radii, line_length = line_length, min_cluster = min_cluster,
      duration = duration, bootstrap_n = bootstrap_n,
      bootstrap_type = bootstrap_type, screen = screen,
      metrics = metrics, seed = seed, level = level
    ),
    class = c("radius_sweep", class(out))
  )
}

#' Print a radius sweep result
#'
#' Prepends a one-line header describing the sweep, then defers to
#' the inherited tibble print.
#'
#' @param x A `radius_sweep` from [radius_sweep()].
#' @param ... Forwarded to the next method.
#'
#' @return `x`, invisibly.
#' @export
print.radius_sweep <- function(x, ...) {
  p <- attr(x, "params")
  cat(sprintf(
    "<radius_sweep> (%d radii x %d metrics%s)\n",
    length(p$radii), length(p$metrics),
    if (p$bootstrap_n > 0L) {
      sprintf(", bootstrap = %s/n=%d", p$bootstrap_type, p$bootstrap_n)
    } else ""
  ))
  NextMethod()
}
