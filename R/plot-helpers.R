#' Hijacks the venneuler plot method
#'
#' Hijacks the venneuler plot method to plot using grid instead of
#' base graphics, provide appropriate limits and label positions.
#'
#' @param x An object of class 'VennDiagram'
#' @param col Color postions on the gradient
#' @param col.fn Color function to produce colors with
#' @param alpha Opacity
#' @param edges Ignored
#' @param border Border color
#' @param col.txt ignored
#' @param ... Passed on to [plot.euler]
#'
#' @return Plots a venneuler diagram using eulerr methods.
#' @export
plot_venneuler <- function(x,
                           col,
                           col.fn = function(col) grDevices::hcl(col*360, 130, 60),
                           alpha = 0.3,
                           edges = 200,
                           border = NA,
                           col.txt = 1,
                           ...) {
  obj <- structure(list(), class = "euler")
  obj$coefficients <- cbind(x$centers, r = x$diameters/2)
  colnames(obj$coefficients) <- c("h", "k", "r")
  obj$fitted.values <- rep(1, length.out = 2^nrow(x$centers) - 1)
  obj$original.values <- rep(1, length.out = 2^nrow(x$centers) - 1)

  graphics::plot(obj, fill = col.fn(x$colors), fill_alpha = alpha, font = 1,
                 border = border, ...)
}

#' Plot venn.js fit
#'
#' @param obj An object of class 'vennjs'
#' @param col color
#' @param alpha Opacity
#' @param edges Ignored
#' @param border Border color
#' @param col.txt ignored
#' @param fill color for fills
#' @param ... Passed on to [plot.euler]
#'
#' @return Plot a diagram from venn.js
#' @export
plot_vennjs <- function(obj,
                        fill = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
                                "#9467bd", "#8c564b", "#e377c2", "#7f7f7f",
                                "#bcbd22", "#17becf"),
                        col = fill,
                        alpha = 0.3,
                        edges = 200,
                        border = NA,
                        col.txt = 1,
                        ...) {
  x <- unlist(lapply(obj, "[", "x"))
  y <- unlist(lapply(obj, "[", "y"))
  r <- unlist(lapply(obj, "[", "radius"))

  names(x) <- names(y) <- names(r) <- names(obj)

  obj <- structure(list(), class = "euler")
  obj$coefficients <- cbind(x = x, y = y, r = r)
  colnames(obj$coefficients) <- c("h", "k", "r")
  obj$fitted.values <- rep(1, length.out = 2^length(x) - 1)
  obj$original.values <- rep(1, length.out = 2^length(x) - 1)

  graphics::plot(obj, fill = fill, col = col, fill_alpha = alpha, font = 1,
                 border = border, ...)
}
