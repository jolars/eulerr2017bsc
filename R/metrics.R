#' Goodness of fit tests for euler diagram
#'
#' stress and diagError
#'
#' @param obj A fit euler diagram
#'
#' @return Returns a list of goodness-of-fit tests
#' @export
gof <- function(obj, orig) {
  UseMethod("gof", obj)
}

#' @rdname gof
#' @export
gof.VennDrawing <- function(obj, orig) {
  # Goodness of fit tests for vennerable diagram
  r <- c()
  center <- matrix(NA, ncol = 2, nrow = 0)

  for (i in seq_along(obj@edgeList)) {
    if (.hasSlot(obj@edgeList[[i]], "radius")) {
      r <- c(r, slot(obj@edgeList[[i]], "radius"))
      center <- rbind(center, as.numeric(obj@edgeList[[i]]@centre))
    }
  }

  r <- r[!duplicated(center)]
  center <- center[!duplicated(center), ]

  x <- center[, 1]
  y <- center[, 2]

  pars <- as.vector(matrix(c(x, y, r), nrow = 3, byrow = TRUE))
  fit <- as.vector(eulerr:::intersect_ellipses(pars, circles = TRUE))

  stress <- eulerr:::venneuler_stress(orig, fit)
  diag_error <- max(abs(fit/sum(fit) - orig/sum(orig)))

  list(stress = stress, diag_error = diag_error)
}

#' @rdname gof
#' @export
gof.VennDiagram <- function(obj, orig) {
  # Goodness of fit tests for venneuler diagram
  x <- obj$centers[, 1]
  y <- obj$centers[, 2]
  r <- obj$diameters/2

  pars <- as.vector(matrix(c(x, y, r), nrow = 3, byrow = TRUE))
  fit <- as.vector(eulerr:::intersect_ellipses(pars, circles = TRUE))

  stress <- eulerr:::venneuler_stress(orig, fit)
  diag_error <- max(abs(fit/sum(fit) - orig/sum(orig)))

  list(stress = stress, diag_error = diag_error)
}
