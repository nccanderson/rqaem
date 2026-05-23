test_that(".label_components_8() returns all zeros for empty input", {
  out <- rqaem:::.label_components_8(matrix(0, 3, 3))
  expect_identical(out, matrix(0L, 3, 3))
})

test_that(".label_components_8() labels a single isolated cell", {
  m <- matrix(0L, 3, 3)
  m[2, 2] <- 1L
  out <- rqaem:::.label_components_8(m)
  expect_identical(max(out), 1L)
  expect_identical(out[2, 2], 1L)
  expect_equal(sum(out > 0), 1)
})

test_that(".label_components_8() merges diagonal neighbours (8-connectivity)", {
  m <- matrix(c(
    1, 0, 0,
    0, 1, 0,
    0, 0, 1
  ), nrow = 3, byrow = TRUE)
  out <- rqaem:::.label_components_8(m)
  expect_identical(max(out), 1L)              # one component along the diagonal
  expect_equal(sum(out > 0), 3)
})

test_that(".label_components_8() separates components by background gaps", {
  m <- matrix(c(
    1, 0, 1,
    0, 0, 0,
    1, 0, 1
  ), nrow = 3, byrow = TRUE)
  out <- rqaem:::.label_components_8(m)
  expect_identical(max(out), 4L)              # four corner cells, no 8-contact
})

test_that(".label_components_8() handles L-shape and adjacent components", {
  m <- matrix(c(
    1, 1, 0, 1,
    1, 0, 0, 1,
    1, 0, 0, 0
  ), nrow = 3, byrow = TRUE)
  out <- rqaem:::.label_components_8(m)
  expect_identical(max(out), 2L)
  expect_equal(sum(out == 1L) + sum(out == 2L), sum(m == 1))
})

test_that(".recurrence_clusters() respects the threshold and upper-triangle restriction", {
  # 3x3 binary recurrence-style matrix with two off-diagonal recurrences
  # forming a size-2 cluster in the upper triangle.
  rm <- matrix(c(
    1, 1, 1,
    1, 1, 0,
    1, 0, 1
  ), nrow = 3, byrow = TRUE)
  # ntriangle = n*(n-1)/2 = 3 here; the two upper-triangle 1s touch
  # diagonally (positions (1,2) and (1,3) are 8-connected) so one
  # cluster of size 2.
  expect_equal(rqaem:::.recurrence_clusters(rm, threshold = 2, ntriangle = 3),
               100 * 2 / 3)
  # Bumping the threshold above the cluster size yields zero.
  expect_equal(rqaem:::.recurrence_clusters(rm, threshold = 3, ntriangle = 3), 0)
})

test_that(".recurrence_clusters() returns NA for trivially small matrices", {
  expect_identical(
    rqaem:::.recurrence_clusters(matrix(integer(0), 0, 0),
                                 threshold = 1, ntriangle = 0),
    NA_real_
  )
  expect_identical(
    rqaem:::.recurrence_clusters(matrix(1L, 1, 1),
                                 threshold = 1, ntriangle = 0),
    NA_real_
  )
})
