traceHat <-function(fit) {
  fit1 <- fit
  U1 <- cBind( t(getME(fit1,"A")),as(getME(fit1,"X"),"dgCMatrix"))
  n1 <- nrow(getME(fit1,"A"))
  D1 <- cBind(Diagonal(n1), Matrix(0, n1,ncol(getME(fit1,"X"))))  
  M1 <-rBind(U1,D1)
  rho <- sum(diag(solve(crossprod(M1)) %*% crossprod(U1)))
  return(rho)
}
