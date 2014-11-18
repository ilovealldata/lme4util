CrossedDataBAL <- function (Mu, NFac, Nlvl, VC) {
  
  nf=NFac
  nl=Nlvl
  nll=length(nl)
  nvc=length(VC)
  n=prod(Nlvl)
  #nf+1 == length(nl) ? n=prod(Nlvl) : error("number of elements in level vector is length of factors vector + 1")
  
  if (length(Mu) == 1) { 
    y=rep(Mu,n) + rnorm(n,0.0,VC[nvc])
    dat=data.frame(y=y)
  }
  else { 
    y=Mu + rnorm(n,0.0,VC[nvc])
    dat=data.frame(y=y) 
  }
  
  rv<-list()
  for ( i in 1:(nll-1) ) {
    if (i==1) {
      Z=kronecker(Diagonal(nl[i]), 
                  Matrix( rep(1.0,prod(nl[(i+1):nvc])), prod(nl[(i+1):nvc]),1))
    }
    else { 
      Z=kronecker( Matrix(rep(1.0,prod(nl[1:(i-1)])), prod(nl[1:(i-1)]),1),
                   kronecker( Diagonal(nl[i]), Matrix(rep(1.0, prod(nl[(i+1):nll])), prod(nl[(i+1):nll]),1 )) 
      )
    }
    
    if (VC[i] > 0.0) {
      rv[[i]]=rnorm(nl[i],0.0,VC[i])
      dat$y =dat$y + as.numeric(Z %*% rv[[i]])
    }
    dat[paste("x",i,sep='')]= factor( as.numeric( Z %*% Matrix(1:nl[i],nl[i],1 ) ))
  }
  dat$id=factor(1:n)
  list(Mu=Mu,NFac=NFac,Nlvl=Nlvl,VC=VC,n=n,re=rv,dat=dat);
}
############################ NestedData  #################################

NestedDataBAL <- function (Mu, NFac, Nlvl, VC) {
  
  nf=NFac
  nl=Nlvl
  nll=length(nl)
  nvc=length(VC)
  n=prod(Nlvl)
  
  #nf+1 == length(nl) ? n=prod(Nlvl) : error("number of elements in level vector is length of factors vector + 1")
  
  if (length(Mu) == 1) { 
    y=rep(Mu,n) + rnorm(n,0.0,VC[nvc])
    dat=data.frame(y=y)
  }
  else { 
    y=Mu + rnorm(n,0.0,VC[nvc])
    dat=data.frame(y=y) 
  }
  
  rv<-list()
  for ( i in 1:(nll-1) ) {
    Z=kronecker(Diagonal(prod(nl[1:i])), 
                Matrix( rep(1.0,prod(nl[(i+1):nll])), prod(nl[(i+1):nll]),1))
    if (VC[i] > 0.0) {
      rv[[i]]=rnorm(prod(nl[1:i]),0.0,VC[i])
      dat$y =dat$y + as.numeric(Z %*% rv[[i]])
    }
    dat[paste("x",i,sep='')]= factor( as.numeric( Z %*% Matrix(1:prod(nl[1:i]),prod(nl[1:i]),1 ) ))
  }
  dat$id=factor(1:n)
  list(Mu=Mu,NFac=NFac,Nlvl=Nlvl,VC=VC,n=n,re=rv,dat=dat);
}
