\name{uniCox-internal}
\title{Internal uniCox functions}
\alias{initcx}
\alias{calcsx}
\alias{findroot}
\alias{func.scor}
\alias{binary.search}
\alias{comp.path}
\alias{cox.func}
\alias{coxscor}
\alias{coxstuff}
\alias{timeorder}
\alias{coxvar}
\alias{coxscor2}
\alias{coxstuff2}
\alias{coxvar2}
\alias{my.rowSums}
\alias{print.uniCoxCVFit}
\alias{print.uniCoxFit}
\alias{balanced.folds}
\alias{permute.rows}
\description{Internal uniCox functions}
\usage{
initcx(y,ic)
calcsx(x,kq,iriskq,ic)
findroot(beta1,beta2,lam,x,sx,iriskq,kq,ddq,ic,del=.001)
func.scor(beta,lam,x,sx,iriskq,kq,ddq,ic)
binary.search(a,b,lam,x,sx,iriskq,kq,ddq,ic,del=.001)
comp.path(lamlist,betalims,x,sx,iriskq,kq,ddq,ic)
cox.func(x,y,censoring.status,s0=0)
coxscor(x, y, ic, offset = rep(0., length(y))) 
coxstuff(x, y, ic, offset = rep(0., length(y))) 
timeorder(y,icensq,x)
coxvar(x, y, ic, offset = rep(0., length(y)), coxstuff.obj = NULL)
coxscor2(x, y, ic, offset = rep(0., length(y))) 
coxstuff2(x, y, ic, offset = rep(0., length(y))) 
coxvar2(x, y, ic, offset = rep(0., length(y)), coxstuff.obj = NULL)
my.rowSums(x)
print.uniCoxCVFit(x,...)
print.uniCoxFit(x,...)
balanced.folds(y, nfolds = min(min(table(y)), 10))
permute.rows(x)
}
\author{Rob Tibshirani}
\details{
  These are not to be called by the user.
}
\keyword{internal}

