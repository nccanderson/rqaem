test_that("plot_recurrence() returns a ggplot", {
  res <- rqa(rbind(c(0, 0), c(1, 1), c(100, 100)), radius = 5)
  p <- plot_recurrence(res)
  expect_s3_class(p, "ggplot")
  # Accepts a bare matrix too.
  p2 <- plot_recurrence(as.matrix(res$recmat))
  expect_s3_class(p2, "ggplot")
})

test_that("plot_recurrence() uses viridis for weighted matrices", {
  fix <- cbind(c(0, 1, 2, 1), c(0, 1, 2, 1))
  weighted <- rqa(fix, radius = 0.6, duration = c(100, 200, 300, 400))
  p <- plot_recurrence(weighted)
  # The viridis scale appears in `scales` for weighted; binary has no
  # fill scale beyond the manual color.
  scales_text <- vapply(p$scales$scales, function(s) class(s)[1L],
                        character(1L))
  expect_true("ScaleContinuous" %in% scales_text)
})

test_that("plot_recurrence() errors on a NULL or empty recurrence matrix", {
  empty <- rqa(matrix(c(0, 0), nrow = 1), radius = 5) # n == 1: NULL recmat
  expect_error(plot_recurrence(empty), "NULL")
})

test_that("plot_fixations() returns a ggplot and adds circles when radius is set", {
  set.seed(1)
  fix <- cbind(runif(10, 0, 800), runif(10, 0, 600))
  p_no_r <- plot_fixations(fix)
  p_r    <- plot_fixations(fix, radius = 50)
  expect_s3_class(p_no_r, "ggplot")
  expect_s3_class(p_r, "ggplot")
  # Adding a radius means one extra layer (the circles).
  expect_gt(length(p_r$layers), length(p_no_r$layers))
})

test_that("plot_fixations() rejects 1-D inputs", {
  expect_error(plot_fixations(c(1, 2, 3, 4)),
               "2-D")
})

test_that("plot_fixations_grid() returns a ggplot and validates grid_size", {
  set.seed(2)
  fix <- cbind(runif(10, 0, 800), runif(10, 0, 600))
  p <- plot_fixations_grid(fix, grid_size = 100)
  expect_s3_class(p, "ggplot")
  expect_error(plot_fixations_grid(fix, grid_size = -1),
               "positive")
  expect_error(plot_fixations_grid(fix, grid_size = c(50, 100)),
               "positive")
})

test_that("autoplot.rqa_result() dispatches to plot_recurrence()", {
  res <- rqa(rbind(c(0, 0), c(1, 1), c(100, 100)), radius = 5)
  p <- ggplot2::autoplot(res)
  expect_s3_class(p, "ggplot")
})

test_that("autoplot.radius_sweep() builds a faceted sweep curve", {
  set.seed(3)
  fix <- cbind(runif(40, 0, 800), runif(40, 0, 600))
  sweep <- radius_sweep(fix, radii = c(40, 80, 120))
  p <- ggplot2::autoplot(sweep)
  expect_s3_class(p, "ggplot")
  # No ribbon layer when there's no bootstrap.
  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1L],
                          character(1L))
  expect_false("GeomRibbon" %in% layer_classes)
})

test_that("autoplot.radius_sweep() adds a ribbon when bootstrap columns exist", {
  set.seed(4)
  fix <- cbind(runif(30, 0, 800), runif(30, 0, 600))
  sweep <- radius_sweep(fix, radii = c(60, 120),
                        bootstrap_n = 10L, seed = 1L)
  p <- ggplot2::autoplot(sweep)
  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1L],
                          character(1L))
  expect_true("GeomRibbon" %in% layer_classes)
})

test_that("plot_fixations() errors clearly when `magick` is not available", {
  # If magick is installed we can't simulate the failure; skip then.
  skip_if(requireNamespace("magick", quietly = TRUE),
          "magick available; cannot test missing-dep path")
  set.seed(5)
  fix <- cbind(runif(5, 0, 100), runif(5, 0, 100))
  expect_error(plot_fixations(fix, image = "nonexistent.png"),
               "magick")
})

# Optional vdiffr snapshots — covered only when vdiffr is installed.
test_that("vdiffr snapshots are stable when vdiffr is available", {
  skip_if_not_installed("vdiffr")
  set.seed(42)
  fix <- cbind(runif(30, 0, 800), runif(30, 0, 600))
  res <- rqa(fix, radius = 100)
  vdiffr::expect_doppelganger("recurrence-binary", plot_recurrence(res))

  weighted <- rqa(fix, radius = 100, duration = sample(100:500, 30))
  vdiffr::expect_doppelganger("recurrence-weighted",
                              plot_recurrence(weighted))

  vdiffr::expect_doppelganger("fixations-with-radius",
                              plot_fixations(fix, radius = 80))

  vdiffr::expect_doppelganger("fixations-grid",
                              plot_fixations_grid(fix, grid_size = 100))

  sweep <- radius_sweep(fix, radii = c(40, 80, 120, 160),
                        bootstrap_n = 25L, seed = 1L)
  vdiffr::expect_doppelganger("radius-sweep-with-ribbon",
                              ggplot2::autoplot(sweep))
})
