skip_if_not_installed("jsonlite")

rqa_metric_keys <- c(
  "n", "nrec", "rec", "det", "revdet", "meanline", "maxline",
  "ent", "relent", "lam", "tt", "corm", "clusters"
)

test_that("rqa() matches Python reference at bit-for-bit parity", {
  cases <- load_python_fixtures()
  rqa_cases <- Filter(function(cs) cs$kind == "rqa", cases)
  expect_gt(length(rqa_cases), 0L)

  for (cs in rqa_cases) {
    p <- cs$params
    got <- rqa(
      cs$input$fixations,
      radius      = p$radius,
      line_length = p$line_length,
      min_cluster = p$min_cluster
    )
    for (key in rqa_metric_keys) {
      expect_equal(
        got[[key]], cs$result[[key]],
        tolerance = 1e-8,
        info = sprintf("case=%s metric=%s", cs$name, key)
      )
    }
    if (!is.null(cs$result$recmat)) {
      got_recmat <- unname(as.matrix(got$recmat))
      storage.mode(got_recmat) <- "integer"
      expect_identical(
        got_recmat,
        cs$result$recmat,
        info = sprintf("case=%s recmat", cs$name)
      )
    }
  }
})
