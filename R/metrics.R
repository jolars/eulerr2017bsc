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

  if (2^length(x) - 1 != length(orig)) {
    stress <- NA
    diagError <- NA
  } else {
    fit <- as.vector(eulerr:::intersect_ellipses(pars, circle = TRUE))

    stress <- eulerr:::stress(orig, fit)
    diagError <- max(abs(fit/sum(fit) - orig/sum(orig)))
  }

  list(stress = stress, diagError = diagError)
}

#' @rdname gof
#' @export
gof.VennDiagram <- function(obj, orig) {
  # Goodness of fit tests for venneuler diagram
  x <- obj$centers[, 1]
  y <- obj$centers[, 2]
  r <- obj$diameters/2

  pars <- as.vector(rbind(x, y, r))
  fit <- as.vector(eulerr:::intersect_ellipses(pars, circle = TRUE))

  stress <- eulerr:::stress(orig, fit)
  diagError <- max(abs(fit/sum(fit) - orig/sum(orig)))

  list(stress = stress, diagError = diagError)
}

#' @rdname gof
#' @export
gof.eulerAPE <- function(obj, orig) {
  # Goodness of fit tests for eulerAPE diagram

  dat <- matrix(unlist(strsplit(obj[7:9], split = "|", fixed = TRUE)),
                nrow = 3, byrow = TRUE)
  dat <- dat[order(dat[, 1]), ]
  dat <- dat[, -1]
  dat <- matrix(as.numeric(dat), nrow = 3)
  dat <- dat[, c(3, 4, 1, 2, 5)]
  dat[, 5] <- dat[, 5] * pi/180
  pars <- as.numeric(t(dat))

  fit <- as.vector(eulerr:::intersect_ellipses(pars, circle = FALSE))/100

  stress <- eulerr:::stress(orig, fit)
  diagError <- diagError(orig, fit)

  list(stress = stress, diagError = diagError)
}

#' @rdname gof
#' @export
gof.vennjs <- function(obj, orig) {
  x <- unlist(lapply(obj, "[", "x"))
  y <- unlist(lapply(obj, "[", "y"))
  r <- unlist(lapply(obj, "[", "radius"))

  fit <- eulerr:::intersect_ellipses(as.vector(rbind(x, y, r)), circle = TRUE)

  stress <- eulerr:::stress(orig, fit)
  diagError <- diagError(orig, fit)

  list(stress = stress, diagError = diagError)
}

#' diagError
#'
#' @param orig Original values
#' @param fit Fitted values
#'
#' @return diagError
#' @export
diagError <- function(orig, fit) {
  max(abs(fit/sum(fit) - orig/sum(orig)))
}

#' stress
#'
#' @param orig Original values
#' @param fit Fitted values
#'
#' @return stress
#' @export
stress <- function(orig, fit) {
  eulerr:::stress(orig, fit)
}
