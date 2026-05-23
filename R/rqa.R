#' Recurrence quantification analysis of a fixation sequence
#'
#' Computes the recurrence matrix and the scalar RQA metrics described
#' in Anderson, Bischof, Laidlaw, Risko and Kingstone (2013):
#' `rec`, `det`, `revdet`, `lam`, `tt`, `corm`, `meanline`, `maxline`,
#' `ent`, `relent`, and the cluster fraction `clusters`. Supports both
#' the binary recurrence formulation (default) and the duration-
#' weighted variant when a `duration` vector is supplied.
#'
#' The implementation is a direct port of the reference Python
#' `Rqa.py` / `RqaDur.py` and is verified against them at bit-for-bit
#' parity on a curated set of test fixtures.
#'
#' @param fixations Numeric matrix / data.frame with two columns
#'   (`x`, `y`), or a numeric / integer vector of categorical codes.
#' @param radius Threshold distance for the recurrence matrix, in the
#'   same units as `fixations`. For categories use a sub-unit value
#'   such as `0.1`.
#' @param line_length Minimum line length to count toward determinism,
#'   laminarity, entropy, and the diagonal/vertical line summaries.
#'   Defaults to `2L`, matching the originals.
#' @param min_cluster Minimum component size (in cells) included in the
#'   `clusters` metric. Defaults to `8L`, matching the originals.
#' @param duration Optional numeric vector with one duration per
#'   fixation. When supplied, switches to the duration-weighted variant
#'   (the recurrence matrix entries become `dur[i] + dur[j]` instead
#'   of `1`, and line measures sum durations instead of cell counts).
#'   Defaults to `NULL` (binary RQA).
#'
#' @return A list of class `c("rqa_result", "list")`:
#'   `n`, `nrec`, `rec`, `det`, `revdet`, `meanline`, `maxline`,
#'   `ent`, `relent`, `lam`, `tt`, `corm`, `clusters`, plus the
#'   `recmat` recurrence matrix and a `params` list echoing the call.
#'
#' @details
#' The duration-weighted variant follows `RqaDur.py` exactly. One
#' consequence worth noting: the originals compute `corm` *outside*
#' the early-return guard, so when the upper triangle is empty
#' (`nrec == 0`) `corm` evaluates to `NaN` (0 / 0). This package
#' preserves that behaviour for parity.
#'
#' @examples
#' fix <- rbind(c(0, 0), c(1, 1), c(100, 100), c(0.5, 0.5))
#' rqa(fix, radius = 5)$rec
#'
#' # Duration-weighted variant
#' rqa(fix, radius = 5, duration = c(200, 150, 300, 220))$rec
#'
#' @export
rqa <- function(fixations, radius, line_length = 2L, min_cluster = 8L,
                duration = NULL) {
  if (is.null(duration)) {
    .rqa_binary(fixations, radius, line_length, min_cluster)
  } else {
    .rqa_durations(fixations, radius, line_length, min_cluster, duration)
  }
}

# Binary RQA — direct port of Rqa.py.
.rqa_binary <- function(fixations, radius, line_length, min_cluster) {
  res <- .rqa_default_result()
  res$params <- list(
    radius = radius,
    line_length = line_length,
    min_cluster = min_cluster,
    type = "rqa"
  )
  fx <- .coerce_fixations(fixations)
  n <- nrow(fx)
  res$n <- n
  if (n <= 1L) {
    return(res)
  }

  recmat <- recurrence_matrix(fx, radius)
  res$recmat <- recmat
  rm <- as.matrix(recmat)
  storage.mode(rm) <- "integer"
  ntriangle <- n * (n - 1L) / 2

  partial <- rm
  partial[lower.tri(partial, diag = TRUE)] <- 0L
  nrec <- sum(partial)
  res$nrec <- nrec
  res$rec  <- 100 * nrec / ntriangle

  diagonals <- lapply(seq_len(n - 1L), function(k) {
    rm[cbind(seq_len(n - k), seq.int(1L + k, n))]
  })

  diag_lens <- integer(0L)
  for (dv in diagonals) {
    diag_lens <- c(diag_lens, .extract_runs(dv, line_length)$count)
  }

  if (length(diag_lens) > 0L) {
    res$det      <- 100 * sum(diag_lens) / nrec
    res$meanline <- mean(diag_lens)
    res$maxline  <- max(diag_lens)
    ent <- .compute_entropy(diag_lens, line_length, res$maxline)
    res$ent    <- ent$ent
    res$relent <- ent$relent
    corm <- 0
    for (i in seq_along(diagonals)) {
      corm <- corm + sum(diagonals[[i]]) * i
    }
    res$corm <- 100 * corm / ((n - 1L) * nrec)
  }

  flipped <- partial[rev(seq_len(n)), , drop = FALSE]
  rev_diag_lens <- integer(0L)
  for (d in seq.int(-(n - 1L), n - 1L)) {
    if (d >= 0L) {
      idx <- cbind(seq_len(n - d), seq.int(1L + d, n))
    } else {
      idx <- cbind(seq.int(1L - d, n), seq_len(n + d))
    }
    if (nrow(idx) == 0L) next
    dv <- flipped[idx]
    rev_diag_lens <- c(rev_diag_lens, .extract_runs(dv, line_length)$count)
  }
  if (length(rev_diag_lens) > 0L) {
    res$revdet <- 100 * sum(rev_diag_lens) / nrec
  }

  rm_v <- rm
  diag(rm_v) <- 0L
  vert_lens <- integer(0L)
  for (i in seq_len(n)) {
    vert_lens <- c(vert_lens, .extract_runs(rm_v[i, ], line_length)$count)
  }
  if (length(vert_lens) > 0L) {
    res$lam <- 100 * sum(vert_lens) / sum(rm)
    res$tt  <- mean(vert_lens)
  }

  res$clusters <- .recurrence_clusters(rm, min_cluster, ntriangle)
  res
}

# Duration-weighted RQA — direct port of RqaDur.py.
#
# The recurrence matrix entries are `(dist <= radius) * (dur[i] + dur[j])`
# rather than `(dist <= radius)`. Line measures sum durations within
# each run rather than counting cells. Note that `corm` is computed
# unconditionally here (the originals compute it outside the "are
# there any diagonal runs?" guard); this means `nrec == 0` yields
# `corm = NaN` (0 / 0).
.rqa_durations <- function(fixations, radius, line_length, min_cluster,
                           duration) {
  res <- .rqa_default_result()
  res$params <- list(
    radius = radius,
    line_length = line_length,
    min_cluster = min_cluster,
    type = "rqa_dur"
  )
  fx <- .coerce_fixations(fixations)
  n <- nrow(fx)
  res$n <- n
  if (n <= 1L) {
    return(res)
  }
  if (!is.numeric(duration) || length(duration) != n) {
    stop("`duration` must be a numeric vector with one entry per fixation.",
         call. = FALSE)
  }

  base <- as.matrix(recurrence_matrix(fx, radius))
  storage.mode(base) <- "integer"
  rm <- base * outer(duration, duration, FUN = "+")
  res$recmat <- if (n >= 64L) Matrix::Matrix(rm, sparse = TRUE) else rm

  ntriangle <- n * (n - 1L) / 2
  partial <- rm
  partial[lower.tri(partial, diag = TRUE)] <- 0
  sum_r <- sum(partial)
  sum_t <- sum(duration)
  res$nrec <- sum_r
  res$rec  <- 100 * sum_r / ((n - 1L) * sum_t)

  diagonals <- lapply(seq_len(n - 1L), function(k) {
    rm[cbind(seq_len(n - k), seq.int(1L + k, n))]
  })

  diag_durs <- numeric(0L)
  for (dv in diagonals) {
    diag_durs <- c(diag_durs, .extract_runs(dv, line_length)$sum)
  }

  if (length(diag_durs) > 0L) {
    res$det      <- 100 * sum(diag_durs) / sum_r
    res$meanline <- mean(diag_durs)
    res$maxline  <- max(diag_durs)
    ent <- .compute_entropy(diag_durs, line_length, res$maxline)
    res$ent    <- ent$ent
    res$relent <- ent$relent
  }

  # corm is unconditional in RqaDur.py — preserved here for parity.
  corm <- 0
  for (i in seq_along(diagonals)) {
    corm <- corm + sum(diagonals[[i]]) * i
  }
  res$corm <- 100 * corm / ((n - 1L) * sum_r)

  flipped <- partial[rev(seq_len(n)), , drop = FALSE]
  rev_diag_durs <- numeric(0L)
  for (d in seq.int(-(n - 1L), n - 1L)) {
    if (d >= 0L) {
      idx <- cbind(seq_len(n - d), seq.int(1L + d, n))
    } else {
      idx <- cbind(seq.int(1L - d, n), seq_len(n + d))
    }
    if (nrow(idx) == 0L) next
    dv <- flipped[idx]
    rev_diag_durs <- c(rev_diag_durs, .extract_runs(dv, line_length)$sum)
  }
  if (length(rev_diag_durs) > 0L) {
    res$revdet <- 100 * sum(rev_diag_durs) / sum_r
  }

  rm_v <- rm
  diag(rm_v) <- 0
  vert_durs <- numeric(0L)
  for (i in seq_len(n)) {
    vert_durs <- c(vert_durs, .extract_runs(rm_v[i, ], line_length)$sum)
  }
  if (length(vert_durs) > 0L) {
    res$lam <- 100 * sum(vert_durs) / sum(rm)
    res$tt  <- mean(vert_durs)
  }

  res$clusters <- .recurrence_clusters(rm, min_cluster, ntriangle)
  res
}

# Default-NA shell so any early return (n <= 1) leaves the rest as NA,
# matching the originals.
.rqa_default_result <- function() {
  structure(
    list(
      n        = NA_integer_,
      recmat   = NULL,
      nrec     = NA_real_,
      rec      = NA_real_,
      det      = NA_real_,
      revdet   = NA_real_,
      meanline = NA_real_,
      maxline  = NA_real_,
      ent      = NA_real_,
      relent   = NA_real_,
      lam      = NA_real_,
      tt       = NA_real_,
      corm     = NA_real_,
      clusters = NA_real_,
      params   = NULL
    ),
    class = c("rqa_result", "list")
  )
}
