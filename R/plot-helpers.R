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
                           col.fn = function(col) hcl(col * 360, 130, 60),
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

  plot(obj, fill = col.fn(x$colors), fill_alpha = alpha, font = 1,
       border = border, ...)
}
