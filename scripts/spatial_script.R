spcov <- function(R,alpha) {
  v <- sqrt(nrow(R))
  D <- as.matrix(dist(R))
  V <- exp(-D/alpha)
  cov1 <- t(chol(V)) %*% rnorm(nrow(R))
  Rd <- as.data.frame(R)
  colnames(Rd) <- c("x", "y")
  Rd$C <- as.numeric((cov1 - mean(cov1)) / sd(cov1))
  return(Rd)
}
