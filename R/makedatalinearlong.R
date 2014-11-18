makedata2waylinear <- function (timelength,idnum, groupnum,beta ,sig10,sig2,esig ) {
  
  n<-timelength*idnum
  grplength=as.integer(n/groupnum)
  ggrp  <- 1:groupnum
  idd <- 1:idnum
  ttime <- 1:timelength
  
  id <- factor(rep(idd,each=timelength))
  time <-  rep(ttime,idnum)
  grp <- factor(rep(ggrp,each=grplength))
  n <- length(id)
  X <- as.matrix(cbind(rep(1.0,n),time))
  sig1 <- matrix(sig10,2,2)
  r01 <- mvrnorm(idnum, c(0.0,0.0), sig1)
  r1 <- matrix(t(r01))
  Z1 <- kronecker(Diagonal(idnum), as.matrix(cbind(rep(1.0,length(ttime)),ttime)) )
  if (sig2 > 0.0) {
    r2 <- matrix(rnorm(groupnum,0.0,sig2),groupnum,1)
    Z2 <- kronecker(Diagonal(groupnum), matrix(rep(1,grplength),grplength,1))
    y <- as.vector(X %*% matrix(beta,2,1)+Z1 %*% r1+Z2 %*% r2+matrix(rnorm(n,0.0,esig),n,1))
    re<- list(r1=r1,r2=r2)
  }
  else {
    y <- as.vector(X %*% matrix(beta,2,1)+Z1 %*% r1 )
    re<- list(r1=r1)
  }
  dat = data.frame(time=time,id=id,grp=grp,y=y)
  list(Timelength=timelength,Idnum=idnum, Groupnum=groupnum, Beta=beta ,
       SIg10=sig10,Sig2=sig2,Esig=esig,re=re,dat=dat)
} 

