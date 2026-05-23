test_that("print.rqa_result() produces a compact one-screen summary", {
  res <- rqa(rbind(c(0, 0), c(1, 1), c(100, 100)), radius = 5)
  out <- capture.output(print(res))
  expect_true(any(grepl("<rqa_result>", out)))
  expect_true(any(grepl("rec", out)))
  expect_true(any(grepl("clusters", out)))
  # No recurrence matrix dump.
  expect_false(any(grepl("recmat", out, fixed = TRUE)))
})

test_that("summary.rqa_result() flags uncomputable metrics", {
  # n=1: every scalar metric is NA, so summary should list all of them.
  res <- rqa(matrix(c(0, 0), nrow = 1), radius = 5)
  out <- capture.output(summary(res))
  expect_true(any(grepl("Not computable", out)))
  expect_true(any(grepl("det", out)))
  expect_true(any(grepl("lam", out)))
})

test_that("summary.rqa_result() omits the warning when nothing is NA", {
  set.seed(7)
  fix <- cbind(runif(40, 0, 100), runif(40, 0, 100))
  res <- rqa(fix, radius = 50)
  out <- capture.output(summary(res))
  expect_false(any(grepl("Not computable", out)))
})

test_that("as.data.frame.rqa_result() returns a one-row tibble of scalars", {
  res <- rqa(rbind(c(0, 0), c(1, 1), c(100, 100)), radius = 5)
  df <- as.data.frame(res)
  expect_s3_class(df, "tbl_df")
  expect_identical(nrow(df), 1L)
  expect_identical(
    names(df),
    c("n", "nrec", "rec", "det", "revdet", "meanline", "maxline",
      "ent", "relent", "lam", "tt", "corm", "clusters")
  )
  # No recmat column.
  expect_false("recmat" %in% names(df))
})
