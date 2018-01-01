#' Form a degenerate conic from two ellipses and then split it into two lines
#'
#' @param C1 Ellipse 1
#' @param C2 Ellipse 2
#'
#' @return Two lines
#' @export
degenerate_split_conics <- function(C1, C2) {
  alp <- det(t(C1))
  bet <- det(rbind(C1[,1], C1[,2],C2[,3])) + det(rbind(C1[,1],C2[,2],C1[,3])) +
    det(rbind(C2[,1],C1[,2],C1[,3]))
  gam <- det(rbind(C1[,1],C2[,2],C2[,3])) + det(rbind(C2[,1],C1[,2],C2[,3])) +
    det(rbind(C2[,1],C2[,2],C1[,3]))
  del <- det(t(C2))
  lambda <- RConics::cubic(c(alp, bet, gam, del))
  lambdaRe <- Re(lambda[Im(lambda) == 0])
  CC <- lambdaRe[1]*C1 + C2
  CC[abs(CC) < .Machine$double.eps^0.95] <- 0
  ll <- RConics::splitDegenerateConic(CC)
  ll
}

#' Ellipse arcs
#'
#' @param saxes Semi-axes
#' @param loc Center
#' @param theta Rotation
#' @param n Number of vertices
#' @param rng Range of the arc to return.
#'
#' @return Points on the arc of an ellipse.
#' @export
ellipse_arc <- function(saxes = c(1, 1),
                        loc = c(0, 0),
                        theta = 0,
                        n = 200,
                        rng = c(0, 2*pi)) {
  b <- min(saxes[1], saxes[2])
  a <- max(saxes[1], saxes[2])
  d2 <- (a - b) * (a + b)
  if (length(rng) == 1)
    phi <- rng - theta
  else
    phi <- seq(rng[1], rng[2], len = n) - theta
  sp <- sin(phi)
  cp <- cos(phi)
  r <- a * b/sqrt((saxes[2] * cp)^2 + (saxes[1] * sp)^2)
  P <- matrix(nrow = n, ncol = 2)
  P[, 1] <- r * cp
  P[, 2] <- r * sp

  if (theta != 0) {
    P <- P %*% matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)),
                      byrow = TRUE, nrow = 2, ncol = 2)
  }
  P <- P + matrix(loc[1:2], nrow = nrow(P), ncol = 2, byrow = TRUE)
  P
}
