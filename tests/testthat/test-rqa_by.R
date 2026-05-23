make_eyedat <- function(seed = 1L, n_per_trial = 40L, n_trials = 3L) {
  set.seed(seed)
  data.frame(
    trial    = rep(seq_len(n_trials), each = n_per_trial),
    x        = runif(n_per_trial * n_trials, 0, 1000),
    y        = runif(n_per_trial * n_trials, 0, 1000),
    duration = sample(100:500, n_per_trial * n_trials, replace = TRUE)
  )
}

test_that("rqa_by() produces one row per group with all 13 scalar metrics", {
  eyedat <- make_eyedat()
  out <- rqa_by(eyedat, x = x, y = y, by = "trial", radius = 100)
  expect_s3_class(out, "tbl_df")
  expect_identical(nrow(out), 3L)
  expect_true(all(c("trial", "n", "nrec", "rec", "det", "clusters") %in% names(out)))
  expect_identical(out$n, rep(40L, 3L))
})

test_that("rqa_by() values agree with running rqa() on each subset", {
  eyedat <- make_eyedat()
  out <- rqa_by(eyedat, x = x, y = y, by = "trial", radius = 100)
  for (tr in unique(eyedat$trial)) {
    sub <- eyedat[eyedat$trial == tr, ]
    direct <- rqa(cbind(sub$x, sub$y), radius = 100)
    row <- out[out$trial == tr, ]
    expect_equal(row$rec, direct$rec)
    expect_equal(row$det, direct$det)
    expect_equal(row$clusters, direct$clusters)
  }
})

test_that("rqa_by() forwards duration through to the weighted variant", {
  eyedat <- make_eyedat()
  with_dur <- rqa_by(eyedat, x = x, y = y, duration = duration,
                     by = "trial", radius = 100)
  no_dur   <- rqa_by(eyedat, x = x, y = y, by = "trial", radius = 100)
  # `rec` differs because the duration variant has a different denominator.
  expect_false(isTRUE(all.equal(with_dur$rec, no_dur$rec)))
})

test_that("rqa_by() handles multiple grouping columns", {
  set.seed(2)
  eyedat <- data.frame(
    trial       = rep(1:2, each = 30),
    participant = rep(c("A", "B"), each = 15, times = 2),
    x = runif(60, 0, 1000),
    y = runif(60, 0, 1000)
  )
  out <- rqa_by(eyedat, x = x, y = y, by = c("trial", "participant"),
                radius = 100)
  expect_identical(nrow(out), 4L)
  expect_true(all(c("trial", "participant") %in% names(out)))
})

test_that("rqa_by() accepts categorical input", {
  set.seed(3)
  catdat <- data.frame(
    trial    = rep(1:2, each = 20),
    category = sample(1:5, 40, replace = TRUE)
  )
  out <- rqa_by(catdat, category = category, by = "trial",
                radius = 0.1)
  expect_identical(nrow(out), 2L)
  expect_true(all(out$n == 20L))
})

test_that("rqa_by() rejects bad inputs", {
  eyedat <- make_eyedat()
  expect_error(rqa_by(eyedat, x = x, y = y, by = "trial"),
               "radius")
  expect_error(rqa_by(eyedat, x = x, y = y, by = "nonexistent",
                      radius = 100),
               "not in")
  # Both xy and category.
  expect_error(
    rqa_by(eyedat, x = x, y = y, category = trial,
           by = "trial", radius = 100),
    "either"
  )
  # Neither.
  expect_error(rqa_by(eyedat, by = "trial", radius = 100),
               "either")
  # Just x, no y.
  expect_error(rqa_by(eyedat, x = x, by = "trial", radius = 100),
               "either")
})

test_that("rqa_by() preserves the original group ordering", {
  eyedat <- data.frame(
    trial = rep(c(3L, 1L, 2L), each = 10),
    x = runif(30, 0, 1000),
    y = runif(30, 0, 1000)
  )
  out <- rqa_by(eyedat, x = x, y = y, by = "trial", radius = 100)
  expect_identical(out$trial, c(3L, 1L, 2L))
})

test_that("rqa_by() returns an empty tibble for empty data", {
  eyedat <- data.frame(trial = integer(0), x = numeric(0), y = numeric(0))
  out <- rqa_by(eyedat, x = x, y = y, by = "trial", radius = 100)
  expect_s3_class(out, "tbl_df")
  expect_identical(nrow(out), 0L)
  expect_true("trial" %in% names(out))
  expect_true("rec" %in% names(out))
})
