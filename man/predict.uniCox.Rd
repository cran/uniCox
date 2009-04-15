\name{predict.uniCox}
\alias{predict.uniCox}
\title{Function to compute the linear predictor from a coxUniv fit}
\description{Function to compute the linear predictor from a coxUniv fit
}
\usage{predict.uniCox(object,x,...)}
\arguments{
   \item{object}{Object returned by uniCox}
  \item{x}{Feature matrix, n obs by p variables}
   \item{...}{Included for compatibility with generic predict function}
}


\details{This function compute the linear predictor from a coxUniv fit
for a set of test features}
\value{
 A matrix of dimension (number rows of x) by  ( number of lambda values),  representing
the predictions x%*%beta for each lambda
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


# fit model
a=uniCox(x,y,status)

# get predictions on a test set
yhat=predict.uniCox(a,xtest)

# fit survival model to predicted values for 7th val of lambda
coxph(Surv(ytest,statustest)~yhat[,7])
}
\keyword{regression}% at least one, from doc/KEYWORDS
\keyword{survival}% __ONLY ONE__ keyword per line

