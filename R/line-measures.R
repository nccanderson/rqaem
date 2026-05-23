# Extract contiguous nonzero runs from a vector.
#
# Pads the input with zeros at both ends, scans for transitions, and
# returns the count of cells and the sum of values for each run whose
# count meets `min_length`. The same algorithm handles both binary
# (count-based) line measures and duration-weighted (sum-based) line
# measures.
#
# @param v Numeric vector. Nonzero values are treated as in-run.
# @param min_length Integer; runs shorter than this are discarded.
#
# @return A list with two equal-length numeric vectors:
#   - `count`: number of cells in each kept run.
#   - `sum`:   sum of `v` within each kept run.
#   When no runs meet `min_length`, both vectors are zero-length.
#
# @keywords internal
# @noRd
.extract_runs <- function(v, min_length) {
  if (length(v) == 0L) {
    return(list(count = integer(0L), sum = numeric(0L)))
  }
  vp <- c(0, v, 0)
  in_run <- as.integer(vp > 0)
  d <- diff(in_run)
  starts <- which(d == 1L)
  ends   <- which(d == -1L)
  counts <- ends - starts
  keep <- counts >= min_length
  if (!any(keep)) {
    return(list(count = integer(0L), sum = numeric(0L)))
  }
  starts <- starts[keep]
  ends   <- ends[keep]
  counts <- counts[keep]
  sums <- vapply(
    seq_along(starts),
    function(k) sum(vp[(starts[k] + 1L):ends[k]]),
    numeric(1L)
  )
  list(count = counts, sum = sums)
}
