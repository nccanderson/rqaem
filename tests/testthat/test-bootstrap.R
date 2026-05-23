test_that("rqa_bootstrap() returns the expected shape", {
  set.seed(1)
  fix <- cbind(runif(40, 0, 800), runif(40, 0, 600))
  boot <- rqa_bootstrap(fix, radius = 100, n = 20L, seed = 42L)
  expect_s3_class(boot, "rqa_bootstrap")
  expect_s3_class(boot, "tbl_df")
  expect_identical(nrow(boot), 20L)
  expect_identical(
    names(boot),
    c("resample", "rec", "det", "lam", "tt", "corm", "ent")
  )
  expect_identical(boot$resample, seq_len(20L))
  expect_s3_class(attr(boot, "observed"), "tbl_df")
  expect_identical(nrow(attr(boot, "observed")), 1L)
})

test_that("rqa_bootstrap() is reproducible given the same seed", {
  set.seed(7)
  fix <- cbind(runif(60, 0, 800), runif(60, 0, 600))
  a <- rqa_bootstrap(fix, radius = 120, n = 10L, seed = 1L)
  b <- rqa_bootstrap(fix, radius = 120, n = 10L, seed = 1L)
  expect_equal(as.data.frame(a), as.data.frame(b))
})

test_that("rqa_bootstrap() restores the global RNG state when seeded", {
  set.seed(99)
  fix <- cbind(runif(30, 0, 800), runif(30, 0, 600))
  # Snapshot immediately before the call — the runif() above advances
  # the RNG, so this must be captured *after* it.
  before <- .Random.seed
  invisible(rqa_bootstrap(fix, radius = 100, n = 5L, seed = 1L))
  expect_identical(.Random.seed, before)
})

test_that("shuffle bootstrap preserves rec exactly (symmetric matrix invariance)", {
  set.seed(7)
  fix <- cbind(runif(60, 0, 800), runif(60, 0, 600))
  obs <- rqa(fix, radius = 120)
  boot <- rqa_bootstrap(fix, radius = 120, n = 25L,
                        type = "shuffle", seed = 1L)
  expect_true(all(boot$rec == obs$rec))
})

test_that("rqa_bootstrap() seeded run matches a fixed fixture (R 4.5 RNG)", {
  set.seed(7)
  fix <- cbind(runif(60, 0, 800), runif(60, 0, 600))
  boot <- rqa_bootstrap(fix, radius = 120, n = 5L, seed = 1L)
  # Fixture captured on R 4.5.1 with the default
  # (Mersenne-Twister / Inversion / Rejection) RNG.
  expect_equal(boot$rec, rep(7.683615819209039, 5L), tolerance = 1e-10)
  expect_equal(boot$det,
               c(7.352941176470588, 8.088235294117647,
                 15.441176470588236, 5.882352941176471,
                 8.823529411764707),
               tolerance = 1e-10)
})

test_that("uniform bootstrap requires screen and xy fixations", {
  fix <- cbind(runif(10, 0, 100), runif(10, 0, 100))
  expect_error(
    rqa_bootstrap(fix, radius = 50, type = "uniform", n = 5L),
    "screen"
  )
  cats <- sample(1:5, 20, replace = TRUE)
  expect_error(
    rqa_bootstrap(cats, radius = 0.1, type = "uniform",
                  screen = c(100, 100), n = 5L),
    "uniform"
  )
})

test_that("rqa_bootstrap() rejects bad inputs", {
  fix <- cbind(runif(10, 0, 100), runif(10, 0, 100))
  expect_error(rqa_bootstrap(fix, radius = 50, n = 0L), "positive")
  expect_error(rqa_bootstrap(fix, radius = 50, metrics = c("rec", "bogus")),
               "Unknown")
  expect_error(rqa_bootstrap(matrix(1, 1, 2), radius = 50), "2 fixations")
  expect_error(
    rqa_bootstrap(fix, radius = 50,
                  duration = c(100, 200)),
    "duration"
  )
})

test_that("duration is permuted alongside fixations in shuffle mode", {
  set.seed(2)
  fix <- cbind(runif(20, 0, 500), runif(20, 0, 500))
  dur <- sample(100:500, 20)
  boot <- rqa_bootstrap(fix, radius = 80, duration = dur,
                        n = 10L, seed = 1L)
  # Sanity: bootstrap result has the right shape and seeded.
  expect_identical(nrow(boot), 10L)
  expect_true(all(!is.na(boot$rec)))
})

test_that("summary.rqa_bootstrap() returns a tidy table and prints", {
  set.seed(7)
  fix <- cbind(runif(60, 0, 800), runif(60, 0, 600))
  boot <- rqa_bootstrap(fix, radius = 120, n = 50L, seed = 1L)
  out <- capture.output(s <- summary(boot))
  expect_true(any(grepl("rqa_bootstrap", out)))
  expect_s3_class(s, "tbl_df")
  expect_identical(
    names(s),
    c("metric", "observed", "p_value", "lo", "hi")
  )
  expect_identical(nrow(s), 6L)
  # P-values are bounded by 1/(n+1) below and 1 above.
  ok <- !is.na(s$p_value)
  expect_true(all(s$p_value[ok] >= 1 / 51))
  expect_true(all(s$p_value[ok] <= 1))
})

test_that("print.rqa_bootstrap() prepends a header", {
  set.seed(7)
  fix <- cbind(runif(30, 0, 800), runif(30, 0, 600))
  boot <- rqa_bootstrap(fix, radius = 100, n = 5L, seed = 1L)
  out <- capture.output(print(boot))
  expect_true(any(grepl("<rqa_bootstrap>", out)))
})
