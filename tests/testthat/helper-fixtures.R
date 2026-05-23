# Helper used by parity tests. Loads python_fixtures.json (regenerated
# by data-raw/make_python_fixtures.py) and reshapes each case into
# R-native objects:
#
#   * `input$fixations` -> numeric matrix (xy) or numeric vector
#     (categorical).
#   * `input$duration`  -> numeric vector or NULL.
#   * `result` scalars  -> JSON `null` (NaN in Python) becomes NA_real_.
#   * `result$recmat`   -> integer matrix, or NULL when n <= 1.
load_python_fixtures <- function(path = NULL) {
  if (is.null(path)) {
    path <- system.file("extdata", "python_fixtures.json",
                        package = "rqaem")
  }
  if (!nzchar(path) || !file.exists(path)) {
    skip("python_fixtures.json not available")
  }
  raw <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  lapply(raw$cases, normalize_fixture_case)
}

normalize_fixture_case <- function(cs) {
  fix_raw <- cs$input$fixations
  cs$input$fixations <- if (length(fix_raw) == 0L) {
    numeric(0L)
  } else if (is.list(fix_raw[[1L]])) {
    do.call(rbind, lapply(fix_raw, unlist))
  } else {
    unlist(fix_raw)
  }
  if (!is.null(cs$input$duration)) {
    cs$input$duration <- unlist(cs$input$duration)
  }

  recmat_raw <- cs$result$recmat
  cs$result$recmat <- if (is.null(recmat_raw) || length(recmat_raw) == 0L) {
    NULL
  } else {
    # Keep numeric — duration-weighted recmats are not integer.
    do.call(rbind, lapply(recmat_raw, unlist))
  }
  scalar_keys <- setdiff(names(cs$result), "recmat")
  for (k in scalar_keys) {
    if (is.null(cs$result[[k]])) cs$result[[k]] <- NA_real_
  }
  cs
}
