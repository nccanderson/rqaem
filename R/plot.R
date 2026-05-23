#' Recurrence plot from an RQA result or matrix
#'
#' Renders the recurrence matrix as a tile plot with `i` decreasing
#' from top to bottom so the visual layout matches the matrix
#' representation in the paper. Binary matrices use a single black
#' fill; duration-weighted matrices use a viridis scale.
#'
#' @param x An `rqa_result` from [rqa()], or a recurrence matrix
#'   directly (base `matrix` or `Matrix::dgCMatrix`).
#' @param ... Ignored (present for S3 compatibility).
#'
#' @return A `ggplot` object.
#'
#' @examples
#' fix <- cbind(c(0, 1, 2, 1, 0), c(0, 1, 2, 1, 0))
#' plot_recurrence(rqa(fix, radius = 0.6))
#'
#' @export
plot_recurrence <- function(x, ...) {
  recmat <- .extract_recmat(x)
  n <- nrow(recmat)
  nz <- which(recmat != 0, arr.ind = TRUE)
  df <- if (nrow(nz) == 0L) {
    tibble::tibble(i = integer(0L), j = integer(0L), value = numeric(0L))
  } else {
    tibble::tibble(
      i = nz[, 1L],
      j = nz[, 2L],
      value = as.numeric(recmat[nz])
    )
  }

  binary <- nrow(df) == 0L || isTRUE(all(df$value == 1))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$j, y = .data$i))
  p <- if (binary) {
    p + ggplot2::geom_tile(fill = "black")
  } else {
    p + ggplot2::geom_tile(ggplot2::aes(fill = .data$value)) +
      ggplot2::scale_fill_viridis_c(name = "weight")
  }
  p +
    ggplot2::coord_equal(
      xlim   = c(0.5, n + 0.5),
      ylim   = c(n + 0.5, 0.5),
      expand = FALSE
    ) +
    ggplot2::labs(x = "j", y = "i") +
    ggplot2::theme_minimal()
}

#' Plot a fixation sequence with optional radius circles
#'
#' Scatter and path plot of a fixation sequence, with optional
#' circles of radius `radius` drawn around each fixation to visualise
#' the recurrence threshold, and an optional background image.
#'
#' @param fixations Numeric matrix / data.frame with two columns
#'   (`x`, `y`).
#' @param radius Optional numeric scalar. When supplied, circles of
#'   the given radius are drawn around each fixation.
#' @param image Optional file path to a background image. Requires
#'   the `magick` package (Suggests).
#'
#' @return A `ggplot` object.
#'
#' @examples
#' fix <- cbind(runif(20, 0, 800), runif(20, 0, 600))
#' plot_fixations(fix, radius = 80)
#'
#' @export
plot_fixations <- function(fixations, radius = NULL, image = NULL) {
  fx <- .coerce_fixations(fixations)
  if (ncol(fx) != 2L) {
    stop("`plot_fixations()` requires 2-D fixation coordinates.",
         call. = FALSE)
  }
  df <- tibble::tibble(
    x   = fx[, 1L],
    y   = fx[, 2L],
    idx = seq_len(nrow(fx))
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$y))
  p <- .layer_image(p, image)

  if (!is.null(radius)) {
    circles <- .circle_paths(df$x, df$y, radius)
    p <- p + ggplot2::geom_path(
      data = circles,
      mapping = ggplot2::aes(x = .data$x, y = .data$y,
                             group = .data$id),
      color = "steelblue", alpha = 0.4,
      inherit.aes = FALSE
    )
  }
  p +
    ggplot2::geom_path(color = "grey60", alpha = 0.5) +
    ggplot2::geom_point(color = "black") +
    ggplot2::geom_text(ggplot2::aes(label = .data$idx),
                       vjust = -1, size = 3) +
    ggplot2::coord_equal() +
    ggplot2::scale_y_reverse() +
    ggplot2::theme_minimal()
}

#' Plot a fixation sequence over a square grid
#'
#' Like [plot_fixations()] but overlays a regular grid spaced at
#' `grid_size` units. Useful for visualising the grid-coarsening
#' step that turns xy fixations into categorical sequences.
#'
#' @param fixations Numeric matrix / data.frame with two columns
#'   (`x`, `y`).
#' @param grid_size Side length of one grid cell, in the same units
#'   as the fixations.
#' @param image Optional file path to a background image. Requires
#'   the `magick` package (Suggests).
#'
#' @return A `ggplot` object.
#'
#' @examples
#' fix <- cbind(runif(20, 0, 800), runif(20, 0, 600))
#' plot_fixations_grid(fix, grid_size = 100)
#'
#' @export
plot_fixations_grid <- function(fixations, grid_size, image = NULL) {
  fx <- .coerce_fixations(fixations)
  if (ncol(fx) != 2L) {
    stop("`plot_fixations_grid()` requires 2-D fixation coordinates.",
         call. = FALSE)
  }
  if (!is.numeric(grid_size) || length(grid_size) != 1L ||
      grid_size <= 0) {
    stop("`grid_size` must be a positive numeric scalar.", call. = FALSE)
  }
  df <- tibble::tibble(
    x   = fx[, 1L],
    y   = fx[, 2L],
    idx = seq_len(nrow(fx))
  )

  if (!is.null(image) && requireNamespace("magick", quietly = TRUE)) {
    info <- magick::image_info(magick::image_read(image))
    xb <- c(0, info$width)
    yb <- c(0, info$height)
  } else {
    xb <- range(df$x)
    yb <- range(df$y)
  }
  vgrid <- seq(xb[1L], xb[2L], by = grid_size)
  hgrid <- seq(yb[1L], yb[2L], by = grid_size)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$y))
  p <- .layer_image(p, image)
  p +
    ggplot2::geom_vline(xintercept = vgrid, color = "grey80") +
    ggplot2::geom_hline(yintercept = hgrid, color = "grey80") +
    ggplot2::geom_path(color = "grey60", alpha = 0.5) +
    ggplot2::geom_point(color = "black") +
    ggplot2::geom_text(ggplot2::aes(label = .data$idx),
                       vjust = -1, size = 3) +
    ggplot2::coord_equal() +
    ggplot2::scale_y_reverse() +
    ggplot2::theme_minimal()
}

#' Recurrence plot for an `rqa_result`
#'
#' S3 method for the `ggplot2::autoplot()` generic. Equivalent to
#' calling [plot_recurrence()] on the same object.
#'
#' @param object An `rqa_result` from [rqa()].
#' @param ... Forwarded to [plot_recurrence()].
#'
#' @return A `ggplot` object.
#' @export
autoplot.rqa_result <- function(object, ...) {
  plot_recurrence(object, ...)
}

#' Sweep curve for a `radius_sweep`
#'
#' Faceted line plot of each metric against radius, with the
#' bootstrap-baseline ribbon overlaid when `bootstrap_n > 0` produced
#' `boot_lo` / `boot_hi` columns. Reproduces the layout of Fig. 7 of
#' Anderson et al. (2013).
#'
#' @param object A `radius_sweep` from [radius_sweep()].
#' @param ... Ignored.
#'
#' @return A `ggplot` object.
#' @export
autoplot.radius_sweep <- function(object, ...) {
  has_boot <- all(c("boot_mean", "boot_lo", "boot_hi") %in% names(object))
  p <- ggplot2::ggplot(
    object,
    ggplot2::aes(x = .data$radius, y = .data$value)
  )
  if (has_boot) {
    p <- p +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data$boot_lo, ymax = .data$boot_hi),
        fill = "steelblue", alpha = 0.2
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = .data$boot_mean),
        color = "steelblue", linetype = "dashed"
      )
  }
  p +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~ metric, scales = "free_y") +
    ggplot2::labs(y = "value") +
    ggplot2::theme_minimal()
}

# Helpers --------------------------------------------------------------

# Extract a dense recurrence matrix from either an rqa_result or a
# bare matrix-like input.
.extract_recmat <- function(x) {
  rm <- if (inherits(x, "rqa_result")) x$recmat else x
  if (is.null(rm)) {
    stop("Recurrence matrix is NULL (was n <= 1?).", call. = FALSE)
  }
  rm <- as.matrix(rm)
  if (nrow(rm) == 0L) {
    stop("Recurrence matrix is empty (n == 0?).", call. = FALSE)
  }
  rm
}

# Build a long tibble of approximated circle outlines for geom_path.
.circle_paths <- function(cx, cy, r, n_segments = 64L) {
  theta <- seq(0, 2 * pi, length.out = n_segments + 1L)
  ct <- cos(theta)
  st <- sin(theta)
  out <- lapply(seq_along(cx), function(i) {
    tibble::tibble(
      id = i,
      x = cx[i] + r * ct,
      y = cy[i] + r * st
    )
  })
  do.call(rbind, out)
}

# Add a background image as an annotation_raster layer, if requested.
.layer_image <- function(p, image) {
  if (is.null(image)) return(p)
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package `magick` is required for image overlays. ",
         "Install it or omit the `image` argument.",
         call. = FALSE)
  }
  img <- magick::image_read(image)
  info <- magick::image_info(img)
  p + ggplot2::annotation_raster(
    grDevices::as.raster(img),
    xmin = 0, xmax = info$width,
    ymin = 0, ymax = info$height
  )
}
