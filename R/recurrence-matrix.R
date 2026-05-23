#' Build a recurrence matrix from a fixation sequence
#'
#' Computes pairwise Euclidean distances and thresholds them at `radius`
#' to produce a binary recurrence matrix. Two fixations recur if they
#' fall within `radius` of each other.
#'
#' Accepts either:
#' * a two-column numeric matrix / data.frame of (x, y) coordinates, or
#' * a one-dimensional numeric or integer vector of category codes
#'   (use a sub-unit radius like `0.1` so equal codes recur).
#'
#' @param fixations Numeric matrix / data.frame with two columns (xy),
#'   or a numeric / integer vector of category codes.
#' @param radius Threshold distance in the same units as `fixations`.
#'
#' @return An `n` x `n` recurrence matrix with `1` where
#'   `distance <= radius` and `0` elsewhere. The diagonal is always `1`
#'   (each fixation recurs with itself). Storage is sparse
#'   (`Matrix::dgCMatrix`) when `n >= 64`, and a dense base `matrix`
#'   below that.
#'
#' @examples
#' fix <- rbind(c(0, 0), c(1, 1), c(100, 100))
#' recurrence_matrix(fix, radius = 5)
#'
#' @export
recurrence_matrix <- function(fixations, radius) {
  if (missing(radius) || !is.numeric(radius) || length(radius) != 1L) {
    stop("`radius` must be a single numeric value.", call. = FALSE)
  }
  fx <- .coerce_fixations(fixations)
  n <- nrow(fx)
  if (n == 0L) {
    return(matrix(integer(0L), nrow = 0L, ncol = 0L))
  }
  if (n == 1L) {
    return(matrix(1L, nrow = 1L, ncol = 1L))
  }
  d <- as.matrix(stats::dist(fx))
  r <- (d <= radius) * 1L
  if (n >= 64L) {
    Matrix::Matrix(r, sparse = TRUE)
  } else {
    r
  }
}
