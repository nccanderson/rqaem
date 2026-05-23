skip_if_not_installed("jsonlite")

rqa_dur_metric_keys <- c(
  "n", "nrec", "rec", "det", "revdet", "meanline", "maxline",
  "ent", "relent", "lam", "tt", "corm", "clusters"
)

test_that("rqa(duration=) matches RqaDur Python reference at bit-for-bit parity", {
  cases <- load_python_fixtures()
  dur_cases <- Filter(function(cs) cs$kind == "rqa_dur", cases)
  expect_gt(length(dur_cases), 0L)

  for (cs in dur_cases) {
    p <- cs$params
    got <- rqa(
      cs$input$fixations,
      radius      = p$radius,
      line_length = p$line_length,
      min_cluster = p$min_cluster,
      duration    = cs$input$duration
    )
    for (key in rqa_dur_metric_keys) {
      expect_equal(
        got[[key]], cs$result[[key]],
        tolerance = 1e-8,
        info = sprintf("case=%s metric=%s", cs$name, key)
      )
    }
    if (!is.null(cs$result$recmat)) {
      expect_equal(
        unname(as.matrix(got$recmat)),
        unname(cs$result$recmat),
        tolerance = 1e-8,
        info = sprintf("case=%s recmat", cs$name)
      )
    }
  }
})

test_that("rqa(duration=) errors when duration length doesn't match n", {
  fix <- rbind(c(0, 0), c(1, 1), c(100, 100))
  expect_error(
    rqa(fix, radius = 5, duration = c(1, 2)),
    "duration"
  )
})

test_that("rqa(duration=) with constant durations matches binary RQA up to a scale", {
  # Per the originals' Example 2 in TestRqaDur: with duration = 0.5
  # everywhere, RqaDur and Rqa give the same scalar metrics for the
  # measures that are scale-invariant. Headline check: rec and det
  # should agree exactly.
  set.seed(42)
  fix <- cbind(runif(20, 0, 100), runif(20, 0, 100))
  binary <- rqa(fix, radius = 30)
  dur    <- rqa(fix, radius = 30, duration = rep(0.5, 20))
  expect_equal(dur$rec, binary$rec, tolerance = 1e-10)
  expect_equal(dur$det, binary$det, tolerance = 1e-10)
})
