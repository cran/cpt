\name{cpt}
\alias{cpt}

\title{
Classification Permutation Test
}

\description{
Non-parametric test for equality of multivariate distributions.  Trains a classifier to classify (multivariate) observations as coming from one of two distributions.  If the classifier is able to classify the observations better than would be expected by chance (using permutation inference), then the null hypothesis that the two distributions are equal is rejected.  
}

\usage{
cpt(Z, T, leaveout = 0, class.methods = "forest", metric = "rate", 
    paired=FALSE, perm.N = 1000, leaveout.N=100)
}

\arguments{
  \item{Z}{
     The data.  An n by p matrix, where n is the number of observations, and p is the number of covariates.
}
  \item{T}{
     The treatment variable.  Is converted to a factor.  Should have only two levels.
}
  \item{leaveout}{
     The number of treatment and control observations to include in the test set.  If 0, no data is left out and the in-sample test statistic is used.  If an integer greater than or equal to 1, the number of both treatment observations and control observations to leave out (thus, half the size of the test set).  Values between 0 and 1 are converted to ceiling(min(table(T))*leaveout.
}
  \item{class.methods}{
     A character vector of the different classification methods to use.  Currently supports only one method at a time.  Can be "logistic", "logistic2", "forest", or "lda".  The "logistic2" classifier is logistic regression with all two-way interactions included in the model. 
}
  \item{metric}{
     A character vector of the different test statistics to use.  Currently supports only one metric at a time.  Can be any combination of "rate", "mse", or "probability".  
}
  \item{paired}{
     Do a paired permutation test.  The data Z must be ordered such that the first observation with T==1 is paired with the first observation with T==2, the second observation with T==1 is paired with the second observation with T==2, etc.  This can be accomplished by either letting the first n/2 rows be the treatment observations, and last n/2 rows being the control observations (in the same order), or by using the first two rows for the first pair, the second two rows for the second pair, etc.
}
  \item{perm.N}{
     The number of permutations.
}
  \item{leaveout.N}{
     The number of training set / test set iterations.  In each iteration, a random test set is generated.  Thus, test sets will typically overlap somewhat.  There is one exception: If leaveout = 1 and leaveout.N = n, then a traditional leave-one-out procedure is used (each obersvation is left out exactly once).
}
}

\value{
A list containing
  \item{pval}{The estimated p-value.}
  \item{teststat}{The observed test statistic.}
  \item{nulldist}{The permutation distribution.}
  
}

\references{
The Classification Permutation Test:  A Nonparametric Test for Equality of Multivariate Distributions.  Gagnon-Bartsch and Shem-Tov, 2016.  Available at: \href{https://arxiv.org/abs/1611.06408}{https://arxiv.org/abs/1611.06408}.
}

\author{ 
  Johann Gagnon-Bartsch <johanngb@umich.edu> 
} 

\examples{
## Create some simulated data
n = 50  # 50 observations
p = 5   # 5 covariates
T = rep(c(0,1),each=25)     # Two groups, 25 observations each
Z = matrix(rnorm(n*p),n,p)  # Random data (null is true)

## Run CPT
cpt.results = cpt(Z, T, class.methods="lda")
print(cpt.results$pval)

## False Null
Z[1:25, 1] = Z[1:25, 1] + 1  # Now the null is false 
cpt.results = cpt(Z, T, class.methods="lda")
print(cpt.results$pval)
}

\keyword{multivariate}
