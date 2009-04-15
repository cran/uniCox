\name{uniCoxCV}
\alias{uniCoxCV}
\title{Function to cross-validate a high dimensional Cox survival model
using Univariate Shrinkage}
\description{
Function to cross-validate a high dimensional Cox survival model
using Univariate Shrinkage}
\usage{uniCoxCV(fit,x,y,status,nfolds=5,folds=NULL)}
\arguments{
  \item{fit}{object returned by call to uniCox}
  \item{x}{Feature matrix, n obs by p variables}
  \item{y}{Vector of  n survival times}
 \item{status}{Vector of n censoring indicators (1= died or event occurred,
 0=survived, or event was censored)}
  \item{nfolds}{Number of cross-valdiation folds}
  \item{folds}{Optional list of sample  numbers defining folds}
}


\details{This function does cross-validation for a prediction model for survival data
with high-dimensional covariates, using the Unvariate Shringae
method.}
\value{
  A list with components
\item{devcvm}{Average drop in CV deviance for each lambda value}
\item{ncallcvm=ncallcvm}{Average  number of features with non-zero wts in the CV, for
each lambda value}
\item{se.devcvm}{Standard error of average drop in CV deviance for each lambda value}
\item{devcv}{Drop in CV deviance for each lambda value}
\item{ncallcv}{Number of features with non-zero wts in the CV, for
each lambda value}
\item{folds}{Indices for CV folds}
\item{call}{Call to this function}
}

\source{
Tibshirani, R.
Univariate shrinkage in the Cox model for high dimensional data  (2009).
http://www-stat.stanford.edu/~tibs/ftp/cus.pdf
To appear SAGMB.
}
\examples{
library(survival)
# generate some data
x=matrix(rnorm(200*1000),ncol=1000)
y=abs(rnorm(200))
x[y>median(y),1:50]=x[y>median(y),1:50]+3
status=sample(c(0,1),size=200,replace=TRUE)

# fit uniCox model
a=uniCox(x,y,status)

# do cross-validation to examine choice of lambda
aa=uniCoxCV(a,x,y,status)
}
\keyword{regression}% at least one, from doc/KEYWORDS
\keyword{survival}% __ONLY ONE__ keyword per line

