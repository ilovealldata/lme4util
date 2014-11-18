library(lme4)
library(Matrix)
library(cAIC4)
library(mlmRev)
library(lme4util)

fm1 <- lmer(Yield ~ 1|Batch, Dyestuff ,REML=F)
system.time(print(cAIC(fm1)))
system.time(print(cAIC(fm1,analytic = FALSE)))
system.time(print(calAIC(fm1)))

fm2 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy,REML=F)
system.time(print(cAIC(fm2)))
system.time(print(cAIC(fm2,analytic = FALSE)))
system.time(print(calAIC(fm2)))


mu = 0
nf =2
nlvl = c(4,4,4)
vc = c(3.0,2.0,1.0)
model <- y ~ 1 + (1|x1) + (1|x2)

dat1 = NestedDataBAL(mu,nf,nlvl,vc)
m1<-lmer(model, dat1$dat,REML=F)
calAIC(m1)

dat2 = CrossedDataBAL(mu,nf,nlvl,vc)
m2<-lmer(model, dat2$dat,REML=F)
calAIC(m2)