test_that(".compute_entropy() returns NA for empty input", {
  out <- rqaem:::.compute_entropy(integer(0), min_length = 2, max_length = 2)
  expect_identical(out$ent, NA_real_)
  expect_identical(out$relent, NA_real_)
})

test_that(".compute_entropy() is zero when all lengths are equal, relent is NaN", {
  out <- rqaem:::.compute_entropy(c(2, 2, 2), min_length = 2, max_length = 2)
  expect_equal(out$ent, 0)
  expect_identical(out$relent, NaN)
})

test_that(".compute_entropy() computes Shannon entropy in base 2", {
  out <- rqaem:::.compute_entropy(c(2, 3), min_length = 2, max_length = 3)
  expect_equal(out$ent, 1)        # H(0.5, 0.5) = 1 bit
  expect_equal(out$relent, 1)     # ent / log2(2) = 1
})

test_that(".compute_entropy() normalises by log2(max-min+1) for relent", {
  # Four distinct equally-frequent lengths span min=2 to max=5.
  out <- rqaem:::.compute_entropy(c(2, 3, 4, 5), min_length = 2, max_length = 5)
  expect_equal(out$ent, 2)        # log2(4) = 2 bits
  expect_equal(out$relent, 1)     # 2 / log2(4) = 1 -> fully spread
})

test_that(".compute_entropy() ignores ordering and counts unique values", {
  out_a <- rqaem:::.compute_entropy(c(2, 2, 3), min_length = 2, max_length = 3)
  out_b <- rqaem:::.compute_entropy(c(3, 2, 2), min_length = 2, max_length = 3)
  expect_equal(out_a$ent,    out_b$ent)
  expect_equal(out_a$relent, out_b$relent)
  # H(2/3, 1/3) = -2/3*log2(2/3) - 1/3*log2(1/3) ≈ 0.9182958
  expect_equal(out_a$ent, -(2/3) * log2(2/3) - (1/3) * log2(1/3))
})
