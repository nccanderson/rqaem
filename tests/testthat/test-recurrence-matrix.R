test_that("recurrence_matrix() builds the expected matrix for xy fixations", {
  fix <- rbind(c(0, 0), c(1, 1), c(100, 100))
  expected <- matrix(c(
    1L, 1L, 0L,
    1L, 1L, 0L,
    0L, 0L, 1L
  ), nrow = 3L, byrow = TRUE)
  expect_identical(unname(recurrence_matrix(fix, radius = 5)), expected)
})

test_that("recurrence_matrix() handles categorical (1D) input", {
  cats <- c(1L, 2L, 1L, 3L)
  expected <- matrix(c(
    1L, 0L, 1L, 0L,
    0L, 1L, 0L, 0L,
    1L, 0L, 1L, 0L,
    0L, 0L, 0L, 1L
  ), nrow = 4L, byrow = TRUE)
  expect_identical(unname(recurrence_matrix(cats, radius = 0.1)), expected)
})

test_that("recurrence_matrix() degenerate sizes", {
  expect_identical(dim(recurrence_matrix(numeric(0), radius = 1)), c(0L, 0L))
  expect_identical(recurrence_matrix(c(7), radius = 1), matrix(1L, 1L, 1L))
})

test_that("recurrence_matrix() switches to sparse storage at n >= 64", {
  small <- recurrence_matrix(seq_len(63L), radius = 0.5)
  big   <- recurrence_matrix(seq_len(64L), radius = 0.5)
  expect_true(is.matrix(small) && !inherits(small, "Matrix"))
  expect_s4_class(big, "Matrix")
  # The diagonal-only pattern has nrec == n in the upper triangle being 0
  expect_identical(sum(as.matrix(big)), 64)
})

test_that("recurrence_matrix() radius threshold is inclusive", {
  fix <- rbind(c(0, 0), c(3, 4))   # distance 5
  expect_equal(sum(recurrence_matrix(fix, radius = 5)), 4)   # both off-diag set
  expect_equal(sum(recurrence_matrix(fix, radius = 4.99)), 2) # only diagonal
})

test_that("recurrence_matrix() rejects invalid radius", {
  expect_error(recurrence_matrix(c(1, 2, 3)), "radius")
  expect_error(recurrence_matrix(c(1, 2, 3), radius = c(1, 2)), "radius")
  expect_error(recurrence_matrix(c(1, 2, 3), radius = "x"), "radius")
})

test_that("recurrence_matrix() accepts data.frame input", {
  df <- data.frame(x = c(0, 1, 100), y = c(0, 1, 100))
  expect_identical(
    unname(recurrence_matrix(df, radius = 5)),
    unname(recurrence_matrix(as.matrix(df), radius = 5))
  )
})
