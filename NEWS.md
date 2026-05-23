# rqaem 0.1.0

Initial release.

## Core

* `rqa()` computes the recurrence matrix and the 13 scalar metrics
  defined in Anderson, Bischof, Laidlaw, Risko and Kingstone (2013):
  `rec`, `det`, `revdet`, `lam`, `tt`, `corm`, `meanline`, `maxline`,
  `ent`, `relent`, `clusters`, plus `n` and `nrec`.
* Accepts either 2-column xy fixations or a vector of categorical
  codes; the categorical path dispatches on input shape.
* Supports the duration-weighted variant via the `duration` argument
  (mirrors the reference `RqaDur.py`).
* The recurrence matrix uses sparse `Matrix::dgCMatrix` storage from
  `n = 64` upward and dense base `matrix` below that.

## Tidy multi-trial workflow

* `rqa_by()` consumes a long-format fixation data frame and returns
  one row per group with all 13 scalar metrics. Tidy-eval on
  `x` / `y` / `category` / `duration`; the `by` argument is a
  character vector of grouping column names. Multi-column grouping is
  supported and the original group order is preserved.
* S3 methods `print.rqa_result()`, `summary.rqa_result()`, and
  `as.data.frame.rqa_result()` on the result object.

## Significance testing and radius selection

* `rqa_bootstrap()` produces null distributions with two strategies:
  `"shuffle"` (permute fixation order) and `"uniform"` (draw from a
  uniform-on-screen distribution). Reproducible via the `seed`
  argument; the global RNG state is restored on exit.
* `summary.rqa_bootstrap()` reports observed value, an empirical
  right-tail p-value (Davison & Hinkley correction), and a percentile
  confidence interval per metric.
* `radius_sweep()` runs `rqa()` over a grid of radii; with
  `bootstrap_n > 0` it also attaches a percentile-bootstrap ribbon at
  each radius.

## Plotting

* `plot_recurrence()`, `plot_fixations()`, and `plot_fixations_grid()`
  return `ggplot` objects. Background images are loaded lazily via
  `magick` (Suggests).
* `autoplot()` methods for `rqa_result` and `radius_sweep`. The
  radius-sweep autoplot reproduces Fig. 7 of the 2013 paper when a
  bootstrap was run.

## Validation

* Bit-for-bit parity with the reference Python `Rqa.py` and
  `RqaDur.py` at `tolerance = 1e-8` on 12 curated test cases (the
  four originals' examples, seven edge cases, and one seeded
  200-point random-walk stress case).

## Documented deviation

* `RqaDur.py` computes `corm` outside the early-return guard that
  protects the other metrics, so `nrec == 0` yields `corm = NaN`
  (`0 / 0`). This package preserves that behaviour for parity and
  notes it in the `rqa()` `@details`.

## License and citation

* Released under the MIT License.
* The reference Python and MATLAB implementations were written by
  Walter F. Bischof; see `inst/CITATION`.
