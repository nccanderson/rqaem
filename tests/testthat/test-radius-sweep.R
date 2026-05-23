test_that("radius_sweep() returns one row per (radius, metric) in long form", {
  set.seed(1)
  fix <- cbind(runif(40, 0, 800), runif(40, 0, 600))
  radii <- c(40, 80, 120)
  out <- radius_sweep(fix, radii = radii)
  expect_s3_class(out, "radius_sweep")
  expect_s3_class(out, "tbl_df")
  expect_identical(nrow(out), 3L * 4L) # 3 radii x 4 default metrics
  expect_identical(names(out), c("radius", "metric", "value"))
  expect_s3_class(out$metric, "factor")
  # Metric ordering preserved.
  expect_identical(levels(out$metric), c("rec", "det", "lam", "corm"))
  # Every radius appears exactly once per metric.
  expect_identical(sort(unique(out$radius)), sort(radii))
})

test_that("radius_sweep() values agree with calling rqa() at each radius", {
  set.seed(2)
  fix <- cbind(runif(40, 0, 800), runif(40, 0, 600))
  radii <- c(50, 100, 150)
  out <- radius_sweep(fix, radii = radii)
  for (r in radii) {
    direct <- rqa(fix, radius = r)
    sub <- out[out$radius == r, ]
    expect_equal(sub$value[sub$metric == "rec"],  direct$rec)
    expect_equal(sub$value[sub$metric == "det"],  direct$det)
    expect_equal(sub$value[sub$metric == "lam"],  direct$lam)
    expect_equal(sub$value[sub$metric == "corm"], direct$corm)
  }
})

test_that("radius_sweep() with bootstrap_n attaches boot_mean / boot_lo / boot_hi", {
  set.seed(3)
  fix <- cbind(runif(40, 0, 800), runif(40, 0, 600))
  out <- radius_sweep(fix, radii = c(60, 120), bootstrap_n = 20L,
                      seed = 1L)
  expect_identical(
    names(out),
    c("radius", "metric", "value", "boot_mean", "boot_lo", "boot_hi")
  )
  expect_true(all(out$boot_lo <= out$boot_mean, na.rm = TRUE))
  expect_true(all(out$boot_mean <= out$boot_hi, na.rm = TRUE))
})

test_that("radius_sweep() with seed is reproducible", {
  set.seed(7)
  fix <- cbind(runif(40, 0, 800), runif(40, 0, 600))
  a <- radius_sweep(fix, radii = c(80, 160), bootstrap_n = 25L,
                    seed = 1L)
  b <- radius_sweep(fix, radii = c(80, 160), bootstrap_n = 25L,
                    seed = 1L)
  expect_equal(as.data.frame(a), as.data.frame(b))
})

test_that("radius_sweep() restores the global RNG state when seeded", {
  set.seed(9)
  fix <- cbind(runif(30, 0, 800), runif(30, 0, 600))
  before <- .Random.seed
  invisible(
    radius_sweep(fix, radii = c(60, 120), bootstrap_n = 10L, seed = 1L)
  )
  expect_identical(.Random.seed, before)
})

test_that("radius_sweep() supports custom metrics", {
  set.seed(11)
  fix <- cbind(runif(40, 0, 800), runif(40, 0, 600))
  out <- radius_sweep(fix, radii = c(80, 160),
                      metrics = c("rec", "nrec", "clusters"))
  expect_identical(levels(out$metric), c("rec", "nrec", "clusters"))
  expect_identical(nrow(out), 2L * 3L)
})

test_that("radius_sweep() rejects bad inputs", {
  fix <- cbind(runif(10, 0, 100), runif(10, 0, 100))
  expect_error(radius_sweep(fix, radii = numeric(0)),
               "non-empty")
  expect_error(radius_sweep(fix, radii = c(-1, 1)),
               "positive")
  expect_error(radius_sweep(fix, radii = c(50),
                            metrics = c("rec", "bogus")),
               "Unknown")
  expect_error(radius_sweep(fix, radii = c(50), bootstrap_n = -1L),
               "non-negative")
  expect_error(radius_sweep(fix, radii = c(50), level = 1.5),
               "level")
})

test_that("print.radius_sweep() prepends a header", {
  set.seed(13)
  fix <- cbind(runif(30, 0, 800), runif(30, 0, 600))
  out <- radius_sweep(fix, radii = c(60, 120))
  text <- capture.output(print(out))
  expect_true(any(grepl("<radius_sweep>", text)))
  out2 <- radius_sweep(fix, radii = c(60), bootstrap_n = 5L, seed = 1L)
  text2 <- capture.output(print(out2))
  expect_true(any(grepl("bootstrap = shuffle", text2)))
})
