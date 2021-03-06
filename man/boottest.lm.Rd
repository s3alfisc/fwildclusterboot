% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boottest_lm.R
\name{boottest.lm}
\alias{boottest.lm}
\title{Fast wild cluster bootstrap inference for object of class lm}
\usage{
\method{boottest}{lm}(
  object,
  clustid,
  param,
  B,
  bootcluster = "max",
  conf_int = NULL,
  seed = NULL,
  beta0 = NULL,
  sign_level = NULL,
  type = "rademacher",
  impose_null = TRUE,
  p_val_type = "two-tailed",
  tol = 1e-06,
  maxiter = 10,
  na_omit = TRUE,
  nthreads = getBoottest_nthreads(),
  ...
)
}
\arguments{
\item{object}{An object of class lm}

\item{clustid}{A character vector containing the names of the cluster variables}

\item{param}{A character vector of length one. The name of the regression
coefficient for which the hypothesis is to be tested}

\item{B}{Integer. The number of bootstrap iterations. When the number of clusters is low,
increasing B adds little additional runtime.}

\item{bootcluster}{A character vector. Specifies the bootstrap clustering variable or variables. If more
than one variable is specified, then bootstrapping is clustered by the intersections of
clustering implied by the listed variables. To mimic the behavior of stata's boottest command,
the default is to cluster by the intersection of all the variables specified via the \code{clustid} argument,
even though that is not necessarily recommended (see the paper by Roodman et al cited below, section 4.2).
Other options include "min", where bootstrapping is clustered by the cluster variable with the fewest clusters.}

\item{conf_int}{A logical vector. If TRUE, boottest computes confidence
intervals by p-value inversion. If FALSE, only the p-value is returned.}

\item{seed}{An integer. Allows the user to set a random seed. If NULL, \code{boottest()} sets an
internal seed. Hence by default, calling \code{boottest()} multiple times on the same object will produce
the same test statistics.}

\item{beta0}{A numeric. Shifts the null hypothesis
H0: param = beta0 vs H1: param != beta0}

\item{sign_level}{A numeric between 0 and 1 which sets the significance level
of the inference procedure. E.g. sign_level = 0.05
returns 0.95\% confidence intervals. By default, sign_level = 0.05.}

\item{type}{character or function. The character string specifies the type
of boostrap to use: One of "rademacher", "mammen", "norm"
and "webb". Alternatively, type can be a function(n) for drawing
wild bootstrap factors. "rademacher" by default.
For the Rademacher and Mammen distribution, if the number of replications B exceeds
the number of possible draw ombinations, 2^(#number of clusters), then \code{boottest()}
will use each possible combination once (enumeration).}

\item{impose_null}{Logical. Controls if the null hypothesis is imposed on
the bootstrap dgp or not. Null imposed \code{(WCR)} by default.
If FALSE, the null is not imposed \code{(WCU)}}

\item{p_val_type}{Character vector of length 1. Type of p-value.
By default "two-tailed".}

\item{tol}{Numeric vector of length 1. The desired accuracy
(convergence tolerance) used in the root finding procedure to find the confidence interval.
1e-6 by default.}

\item{maxiter}{Integer. Maximum number of iterations used in the root finding procedure to find the confidence interval.
10 by default.}

\item{na_omit}{Logical. If TRUE, \code{boottest()} omits rows with missing
variables in the cluster variable that have not previously been deleted
when fitting the regression object (e.g. if the cluster variable was not used
when fitting the regression model).}

\item{nthreads}{The number of threads. Can be: a) an integer lower than,
or equal to, the maximum number of threads; b) 0: meaning
all available threads will be used; c) a number strictly
between 0 and 1 which represents the fraction of all threads
to use. The default is to use 1 core.}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
An object of class \code{boottest}

\item{p_val}{The bootstrap p-value.}
\item{t_stat}{The bootstrap t-statistic.}
\item{conf_int}{The bootstrap confidence interval.}
\item{param}{The tested parameter.}
\item{N}{Sample size. Might differ from the regression sample size if the
cluster variables contain NA values.}
\item{B}{Number of Bootstrap Iterations.}
\item{clustid}{Names of the cluster Variables.}
\item{N_G}{Dimension of the cluster variables as used in boottest.}
\item{sign_level}{Significance level used in boottest.}
\item{type}{Distribution of the bootstrap weights.}
\item{p_test_vals}{All p-values calculated while calculating the confidence
interval.}
\item{test_vals}{All t-statistics calculated while calculating the
confidence interval.}
\item{regression}{The regression object used in boottest.}
\item{call}{Function call of boottest.}
}
\description{
\code{boottest.lm} is a S3 method that allows for fast wild cluster
bootstrap inference for objects of class lm by  implementing
the fast wild bootstrap algorithm developed in Roodman et al., 2019.
}
\section{Confidence Intervals}{

\code{boottest} computes confidence intervals by inverting p-values.
In practice, the following procedure is used:
\itemize{
\item Based on an initial guess for starting values, calculate p-values
for 26 equal spaced points between the starting values.
\item Out of the 26 calculated p-values, find the two pairs of values x
for which the corresponding p-values px cross the significance level
sign_level.
\item Feed the two pairs of x into an numerical root finding procedure and
solve for the root. boottest currently relies on
\code{stats::uniroot} and sets an absolute tolerance of 1e-06 and
stops the procedure after 10 iterations.
}
}

\section{Standard Errors}{

\code{boottest} does not calculate standard errors.
}

\examples{
library(fwildclusterboot)
data(voters)
lm_fit <-lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,
         data = voters)
boot1 <- boottest(lm_fit, 
                  B = 9999, 
                  param = "treatment",
                   clustid = "group_id1")
boot2 <- boottest(lm_fit,
                  B = 9999,
                  param = "treatment",
                 clustid = c("group_id1", "group_id2"))
boot3 <- boottest(lm_fit,
                  B = 9999,
                  param = "treatment",
                  clustid = c("group_id1", "group_id2"),
                  sign_level = 0.2,
                  seed = 8,
                  beta0 = 2)
summary(boot1)
plot(boot1)
}
\references{
Roodman et al., 2019, "Fast and wild: Bootstrap inference in
STATA using boottest", The STATA Journal.
(\url{https://journals.sagepub.com/doi/full/10.1177/1536867X19830877})
}
