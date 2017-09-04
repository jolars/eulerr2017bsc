# Form a degenerate conic from two ellipses and then split it into two lines
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
