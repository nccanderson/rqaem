# Iterative 8-connected component labelling on a binary matrix.
#
# Replaces `skimage.measure.label(m, connectivity=2)` from the
# reference Python without the heavy CV dependency. Each connected
# region (8-neighbour) gets a distinct positive integer label;
# background cells are 0.
#
# @param m Numeric / logical matrix. Cells with `> 0` are foreground.
#
# @return Integer matrix of the same dimensions as `m`.
# @keywords internal
# @noRd
.label_components_8 <- function(m) {
  m <- as.matrix(m) > 0
  nr <- nrow(m)
  nc <- ncol(m)
  labels <- matrix(0L, nr, nc)
  if (nr == 0L || nc == 0L) {
    return(labels)
  }
  cap <- nr * nc
  stack_r <- integer(cap)
  stack_c <- integer(cap)
  next_label <- 0L
  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      if (!m[i, j] || labels[i, j] != 0L) next
      next_label <- next_label + 1L
      stack_r[1L] <- i
      stack_c[1L] <- j
      labels[i, j] <- next_label
      top <- 1L
      while (top > 0L) {
        r <- stack_r[top]
        c <- stack_c[top]
        top <- top - 1L
        for (dr in -1L:1L) {
          rr <- r + dr
          if (rr < 1L || rr > nr) next
          for (dc in -1L:1L) {
            if (dr == 0L && dc == 0L) next
            cc <- c + dc
            if (cc < 1L || cc > nc) next
            if (m[rr, cc] && labels[rr, cc] == 0L) {
              labels[rr, cc] <- next_label
              top <- top + 1L
              stack_r[top] <- rr
              stack_c[top] <- cc
            }
          }
        }
      }
    }
  }
  labels
}

# Compute the `clusters` recurrence metric: the area (as a percentage
# of `n*(n-1)/2`) covered by 8-connected components in the strict
# upper triangle of the recurrence matrix that meet `threshold`.
#
# @param recmat Recurrence matrix (dense or sparse).
# @param threshold Minimum component size to count, in cells.
# @param ntriangle Normaliser, typically `n * (n - 1) / 2`.
#
# @return Numeric scalar; `NA_real_` when the recurrence matrix is too
#   small to contain a strict upper triangle.
# @keywords internal
# @noRd
.recurrence_clusters <- function(recmat, threshold, ntriangle) {
  m <- as.matrix(recmat)
  n <- nrow(m)
  if (n < 2L) {
    return(NA_real_)
  }
  m[lower.tri(m, diag = TRUE)] <- 0
  labels <- .label_components_8(m)
  if (max(labels) == 0L) {
    return(0)
  }
  sizes <- tabulate(labels[labels > 0L])
  total <- sum(sizes[sizes >= threshold])
  100 * total / ntriangle
}
