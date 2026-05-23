#' Default value for `NULL`
#'
#' Returns `b` if `a` is `NULL`, otherwise `a`.
#'
#' @param a,b R objects.
#' @return `a` if not `NULL`, otherwise `b`.
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a

# Coerce a fixation input to an n-by-k numeric matrix.
# A 1-D vector becomes an n-by-1 matrix (the categorical case). A
# data.frame becomes the corresponding matrix.
.coerce_fixations <- function(x) {
  if (is.null(dim(x))) {
    matrix(as.numeric(x), ncol = 1L)
  } else if (is.data.frame(x)) {
    as.matrix(x)
  } else {
    x
  }
}
