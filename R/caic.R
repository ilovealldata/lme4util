
calAIC <- function(fit) {
  fit1 <- fit
  logL <- sum(dnorm(getME(fit1,"y"),predict(fit1),attr(VarCorr(fit1),"sc"),log=T))
  p <- length(fixef(fit1))
  N <- nrow(getME(fit1,"X"))
  cor <-c(0.0,0.0)
  #K.corr <- (N-p-1)*(rho+1)/(N-p-2) + (p+1)/(N-p-2)
  cor[1]<-N*(N-p-1)/((N-p)*(N-p-2)) 
  cor[2]<-N*(p+1)/((N-p)*(N-p-2))
  rho <- traceHat(fit1)
  cAIC <- -2*logL+2*( cor[1]*(rho+1) +cor[2])
  return( list(logL,rho,cor,cAIC) )
}
