#' A set relationship visualized with circles and ellipses
#'
#' @return Plots two plots side-by-side.
#' @export
fig_impossible <- function() {
  p1 <- plot(
    eulerr::euler(c(A = 2, B = 2, C = 2, "A&B" = 1, "A&C" = 1, "B&C" = 1)))
  p2 <- plot(
    eulerr::euler(c(A = 2, B = 2, C = 2, "A&B" = 1, "A&C" = 1, "B&C" = 1),
                  shape = "ellipse"))

  gridExtra::grid.arrange(p1, p2, ncol = 2)
}

#' Example of overlap computation for ellipses
#'
#' @return Plot a showcase of overlap area computations.
#' @export
fig_polyarea <- function() {
  x <- c(0, -0.3, 0.2)
  y <- c(0, 0.1, 0.3)
  ra <- a <- c(0.3, 0.5, 0.4)
  rb <- b <- c(0.3, 0.3, 0.6)
  phi <- c(-pi/6, 2, -2)

  ee <- data.frame(x, y, ra, rb, phi)

  tx <- atan2(-b*tan(phi), a)
  ty <- atan2(b*tan(pi/2L - phi), a)

  xlim <- range(x + a*cos(tx)*cos(phi) - b*sin(tx)*sin(phi),
                x + a*cos(tx + pi)*cos(phi) - b*sin(tx + pi)*sin(phi))
  ylim <- range(y + b*sin(ty)*cos(phi) + a*cos(ty)*sin(phi),
                y + b*sin(ty + pi)*cos(phi) + a*cos(ty + pi)*sin(phi))

  pp <- matrix(NA, ncol = 4, nrow = 0)

  for (i in 1:2) {
    for (j in (i + 1):3) {
      e1 <-
        RConics::ellipseToConicMatrix(c(ra[i], rb[i]), c(x[i], y[i]), phi[i])
      e2 <-
        RConics::ellipseToConicMatrix(c(ra[j], rb[j]), c(x[j], y[j]), phi[j])
      pp <-
        rbind(pp, cbind(t(RConics::intersectConicConic(e1, e2)[1:2, ]), i, j))
    }
  }

  sel <- logical(nrow(pp))
  for (k in 1:nrow(pp)) {
    in_which <- ((pp[k, 1] - x)*cos(phi) + (pp[k, 2] - y)*sin(phi))^2/ra^2 +
      ((pp[k, 1] - x)*sin(phi) - (pp[k, 2] - y)*cos(phi))^2/rb^2 <= 1 + 0.1
    sel[k] <- all(in_which)
  }

  pp <- pp[sel, ]
  mid <- cbind(mean(pp[, 1]), mean(pp[, 2]))
  seglines <- matrix(NA, ncol = 2, nrow = 0)

  ang <- atan2(pp[, 1] - mid[1], pp[, 2] - mid[2])
  ord <- order(ang)

  pp <- pp[ord, ]

  j <- nrow(pp)
  for (i in 1:nrow(pp)) {
    k <- intersect(pp[i, 3:4], pp[j, 3:4])
    start <- atan2(pp[j, 2] - y[k], pp[j, 1] - x[k])
    stop <- atan2(pp[i, 2] - y[k], pp[i, 1] - x[k])
    arc <- ellipse_arc(c(a[k], b[k]), c(x[k], y[k]), theta = phi[k],
                       rng = c(start, stop))
    seglines <- rbind(seglines, arc)
    j <- i
  }

  lattice::xyplot(
    y ~ x, data = ee, asp = "iso",
    xlim = grDevices::extendrange(xlim), ylim = grDevices::extendrange(ylim),
    scales = list(draw = FALSE), xlab = NULL, ylab = NULL,
    par.settings = list(axis.line = list(col = "transparent")),
    panel = function(x, y, ...) {
      eulerr::panel.euler.ellipses(x, y, ra, rb, phi, ...)
      lattice::panel.polygon(seglines, col = "slategray2")
      lattice::panel.polygon(pp[, 1:2], col = "grey90")
      lattice::panel.points(mid, pch = 4, col = 1)
      lattice::panel.points(pp[, 1:2, drop = FALSE], col = 1, pch = 16)
    }
  )
}

#' The process of intersecting two ellipses
#'
#' @return Plot three diagrams side-by-side showing the process of intersecting
#'   three ellipses.
#' @export
fig_intersection <- function() {
  C1 <- RConics::ellipseToConicMatrix(c(8, 2), c(0, 0), -pi/3)
  C2 <- RConics::ellipseToConicMatrix(c(5, 2), c(1, -2), pi/5)
  ll <- Re(degenerate_split_conics(C1, C2))

  ellipses <- data.frame(rbind(ellipse(c(8, 2), c(0, 0), -pi/3),
                               ellipse(c(5, 2), c(1, -2), pi/5)))
  colnames(ellipses) <- c("x", "y")
  ellipses$fac <- rep(c("A", "B"), each = 201)
  pp <- RConics::intersectConicConic(C1, C2)

  p1 <- lattice::xyplot(
    y ~ x, data = ellipses, type = "l", groups = fac, asp = 1,
    xlab = NULL, ylab = NULL,
    scales = list(draw = FALSE, axes = FALSE),
    par.settings = list(axis.line = list(col = "transparent")),
    panel = function(x, y, ...) {
      lattice::panel.xyplot(x, y, ..., col = c("black", "steelblue3"))
    })
  p2 <- lattice::xyplot(y ~ x, data = ellipses, type = "l", groups = fac, asp = 1,
    xlab = NULL, ylab = NULL,
    scales = list(draw = FALSE, axes = FALSE),
    par.settings = list(axis.line = list(col = "transparent")),
    panel = function(x, y, ...) {
      lattice::panel.xyplot(x, y, ..., col = c("transparent", "steelblue3"))
      lattice::panel.abline(a = -ll[3, 1]/ll[2, 1], b = -ll[1, 1]/ll[2, 1])
      lattice::panel.abline(a = -ll[3, 2]/ll[2, 2], b = -ll[1, 2]/ll[2, 2])
    })
  p3 <- lattice::xyplot(
    y ~ x, data = ellipses, type = "l", groups = fac, asp = 1,
    xlab = NULL, ylab = NULL,
    scales = list(draw = FALSE, axes = FALSE),
    par.settings = list(axis.line = list(col = "transparent")),
    panel = function(x, y, ...) {
      lattice::panel.xyplot(x, y, ..., col = c("transparent", "steelblue3"))
      lattice::panel.abline(a = -ll[3, 1]/ll[2, 1], b = -ll[1, 1]/ll[2, 1])
      lattice::panel.abline(a = -ll[3, 2]/ll[2, 2], b = -ll[1, 2]/ll[2, 2])
      lattice::panel.points(t(pp[1:2, ]), col = 1)
    })
  gridExtra::grid.arrange(p1, p2, p3, ncol = 3)
}
