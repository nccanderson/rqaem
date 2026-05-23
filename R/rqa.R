#' Recurrence quantification analysis of a fixation sequence
#'
#' Computes the recurrence matrix and the scalar RQA metrics described
#' in Anderson, Bischof, Laidlaw, Risko and Kingstone (2013):
#' `rec`, `det`, `revdet`, `lam`, `tt`, `corm`, `meanline`, `maxline`,
#' `ent`, `relent`, and the cluster fraction `clusters`. The
#' implementation is a direct port of the reference Python `Rqa.py`
#' and is verified against it at bit-for-bit parity on a curated set
#' of test fixtures.
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
#'
#' @return A list of class `c("rqa_result", "list")`:
#'   `n`, `nrec`, `rec`, `det`, `revdet`, `meanline`, `maxline`,
#'   `ent`, `relent`, `lam`, `tt`, `corm`, `clusters`, plus the
#'   `recmat` recurrence matrix and a `params` list echoing the call.
#'
#' @examples
#' fix <- rbind(c(0, 0), c(1, 1), c(100, 100), c(0.5, 0.5))
#' res <- rqa(fix, radius = 5)
#' res$rec
#'
#' @export
rqa <- function(fixations, radius, line_length = 2L, min_cluster = 8L) {
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

  # Upper-triangle recurrence count.
  partial <- rm
  partial[lower.tri(partial, diag = TRUE)] <- 0L
  nrec <- sum(partial)
  res$nrec <- nrec
  res$rec  <- 100 * nrec / ntriangle

  # Diagonals 1..n-1 above the main diagonal (un-thresholded).
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
    # CORM is the recurrence-mass-weighted mean offset, computed over
    # every diagonal (un-thresholded), not only those with kept runs.
    corm <- 0
    for (i in seq_along(diagonals)) {
      corm <- corm + sum(diagonals[[i]]) * i
    }
    res$corm <- 100 * corm / ((n - 1L) * nrec)
  }

  # Reverse determinism: flip the upper triangle vertically and walk
  # every anti-diagonal from -(n-1) to (n-1).
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

  # Vertical line measures. Per Rqa.py: zero the diagonal and treat
  # each row as a vertical scan; the result is the union of vertical
  # and horizontal lines in the upper triangle (symmetry trick).
  rm_v <- rm
  diag(rm_v) <- 0L
  vert_lens <- integer(0L)
  for (i in seq_len(n)) {
    vert_lens <- c(vert_lens, .extract_runs(rm_v[i, ], line_length)$count)
  }
  if (length(vert_lens) > 0L) {
    # `lam`'s denominator is the WHOLE recurrence matrix sum
    # (including the diagonal) — matches the originals.
    res$lam <- 100 * sum(vert_lens) / sum(rm)
    res$tt  <- mean(vert_lens)
  }

  res$clusters <- .recurrence_clusters(rm, min_cluster, ntriangle)
  res
}

# Default-NA shell, used as the starting result so any early return
# (n <= 1) leaves the rest as NA, matching the originals' behaviour.
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
