# Names and labels for the scalar RQA metrics, in display order.
.rqa_metric_names <- c(
  "n", "nrec", "rec", "det", "revdet", "meanline", "maxline",
  "ent", "relent", "lam", "tt", "corm", "clusters"
)

#' Print an RQA result
#'
#' Compact one-screen summary of an `rqa_result` object. The
#' recurrence matrix itself is not printed; access it via `x$recmat`.
#'
#' @param x An `rqa_result` returned by [rqa()].
#' @param digits Number of significant digits for the scalar metrics.
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#' @export
print.rqa_result <- function(x, digits = 4, ...) {
  type <- x$params$type %||% "rqa"
  cat(sprintf("<rqa_result> (%s)\n", type))
  cat(sprintf(
    "  n = %s, radius = %s, line_length = %s, min_cluster = %s\n",
    format(x$n),
    format(x$params$radius),
    format(x$params$line_length),
    format(x$params$min_cluster)
  ))
  cat("  Metrics:\n")
  metric_keys <- setdiff(.rqa_metric_names, "n")
  for (k in metric_keys) {
    cat(sprintf("    %-9s %s\n", k, format(x[[k]], digits = digits)))
  }
  invisible(x)
}

#' Summarise an RQA result
#'
#' Like [print.rqa_result()] but also lists any metrics that are `NA`
#' because they were not computable (e.g. `det` when there are no
#' diagonal-line runs of the required length).
#'
#' @param object An `rqa_result` returned by [rqa()].
#' @param digits Number of significant digits for the scalar metrics.
#' @param ... Ignored.
#'
#' @return `object`, invisibly.
#' @export
summary.rqa_result <- function(object, digits = 4, ...) {
  print(object, digits = digits)
  uncomputable <- vapply(
    setdiff(.rqa_metric_names, "n"),
    function(k) is.na(object[[k]]),
    logical(1L)
  )
  bad <- names(uncomputable)[uncomputable]
  if (length(bad)) {
    cat("  Not computable (NA): ", paste(bad, collapse = ", "), "\n", sep = "")
  }
  invisible(object)
}

#' Coerce an RQA result to a one-row tibble of scalar metrics
#'
#' The recurrence matrix is dropped; only the 13 scalar metrics are
#' returned, in the canonical order. Useful for combining results
#' from many trials via [rqa_by()] or `do.call(rbind, ...)`.
#'
#' @param x An `rqa_result` returned by [rqa()].
#' @param row.names,optional,... Ignored (present for S3 compatibility).
#'
#' @return A tibble with one row and 13 columns (the scalar metrics).
#' @export
as.data.frame.rqa_result <- function(x, row.names = NULL, optional = FALSE, ...) {
  tibble::tibble(
    n        = x$n,
    nrec     = x$nrec,
    rec      = x$rec,
    det      = x$det,
    revdet   = x$revdet,
    meanline = x$meanline,
    maxline  = x$maxline,
    ent      = x$ent,
    relent   = x$relent,
    lam      = x$lam,
    tt       = x$tt,
    corm     = x$corm,
    clusters = x$clusters
  )
}
