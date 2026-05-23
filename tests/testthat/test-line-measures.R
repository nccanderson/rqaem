test_that(".extract_runs() returns empty for trivial inputs", {
  expect_identical(
    rqaem:::.extract_runs(numeric(0), min_length = 2),
    list(count = integer(0L), sum = numeric(0L))
  )
  expect_identical(
    rqaem:::.extract_runs(c(0, 0, 0, 0), min_length = 2),
    list(count = integer(0L), sum = numeric(0L))
  )
})

test_that(".extract_runs() captures a single run of binary 1s", {
  out <- rqaem:::.extract_runs(c(1, 1, 1), min_length = 2)
  expect_identical(out$count, 3L)
  expect_identical(out$sum, 3)
})

test_that(".extract_runs() drops runs shorter than min_length", {
  out <- rqaem:::.extract_runs(c(1, 1), min_length = 3)
  expect_identical(out$count, integer(0L))
  expect_identical(out$sum, numeric(0L))
})

test_that(".extract_runs() handles multiple binary runs separated by zeros", {
  out <- rqaem:::.extract_runs(c(1, 1, 0, 1, 1, 1, 0, 1), min_length = 2)
  expect_identical(out$count, c(2L, 3L))
  expect_identical(out$sum, c(2, 3))
})

test_that(".extract_runs() sums values in weighted runs", {
  out <- rqaem:::.extract_runs(c(2, 3, 0, 4, 0, 5, 5, 5), min_length = 2)
  expect_identical(out$count, c(2L, 3L))
  expect_identical(out$sum, c(5, 15))
})

test_that(".extract_runs() matches the originals' padding/edge rules", {
  # Run touching either boundary should be captured.
  out <- rqaem:::.extract_runs(c(1, 1, 0, 0, 1, 1), min_length = 2)
  expect_identical(out$count, c(2L, 2L))
  expect_identical(out$sum, c(2, 2))
})
