\name{uniCox}
\alias{uniCox}
\title{Function to fit a high dimensional Cox survival model using Univariate Shrinkage}
\description{Function to fit a high dimensional Cox survival model using Univariate Shrinkage}
\usage{uniCox(x,y,status,lamlist=NULL,nlam=20,del.thres=.01, max.iter=5)}
\arguments{
  \item{x}{Feature matrix, n obs by p variables}
  \item{y}{Vector of  n survival times}
 \item{status}{Vector of n censoring indicators (1= died or event occurred,0=survived, or event was censored)}
\item{lamlist}{Optional vector of lambda values for solution path}
\item{nlam}{Number of lambda values to consider}
\item{del.thres}{Convergence threshold}
\item{max.iter}{Maximum number of iterations for each lambda}
}


\details{This function builds a prediction model for survival data
with high-dimensional covariates, using the Unvariate Shringae
method.}
\value{
  A list with components
\item{lamlist}{Values of lambda used}
\item{beta}{Coef estimates,  number of features by  number of lambda values}
\item{mx}{Mean of feature columns}
\item{vx}{Square root of Fisher information for each feature}
\item{s0}{Exchangeability factor for denominator of score statistic}
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

xtest=matrix(rnorm(50*1000),ncol=1000)
ytest=abs(rnorm(50))
xtest[ytest>median(ytest),1:50]=xtest[ytest>median(ytest),1:50]+3

statustest=sample(c(0,1),size=50,replace=TRUE)


# fit uniCox  model
a=uniCox(x,y,status)

# look at results
 print(a)

# do cross-validation to examine choice of lambda
aa=uniCoxCV(a,x,y,status)

# look at results
 print(aa)

# get predictions on a test set
yhat=predict.uniCox(a,xtest)

# fit survival model to predicted values
coxph(Surv(ytest,statustest)~yhat[,7])
}
\keyword{regression}% at least one, from doc/KEYWORDS
\keyword{survival}% __ONLY ONE__ keyword per line

