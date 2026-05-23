# Shannon entropy (base 2) and relative entropy of a length / duration
# vector, mirroring the originals' `compute_entropy()`.
#
# Computed as `H = -sum(p * log2(p))` over the empirical distribution
# of unique values in `a`. Relative entropy normalises by the maximum
# achievable entropy across `max_length - min_length + 1` distinct
# values; it is `NaN` when only one distinct length is observed (so
# `min_length == max_length`), matching the Python and MATLAB
# originals.
#
# @param a Numeric vector of observed line lengths (or durations).
# @param min_length Threshold line length used for the run extractor.
# @param max_length Largest observed line length in `a`.
#
# @return A list with `ent` and `relent` (both numeric scalars). When
#   `a` is empty, both are `NA_real_`.
#
# @keywords internal
# @noRd
.compute_entropy <- function(a, min_length, max_length) {
  if (length(a) == 0L) {
    return(list(ent = NA_real_, relent = NA_real_))
  }
  counts <- tabulate(match(a, unique(a)))
  p <- counts / sum(counts)
  ent <- -sum(p * log2(p))
  relent <- if (min_length == max_length) {
    NaN
  } else {
    ent / log2(max_length - min_length + 1)
  }
  list(ent = ent, relent = relent)
}
