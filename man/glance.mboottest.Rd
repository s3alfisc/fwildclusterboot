% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{glance.mboottest}
\alias{glance.mboottest}
\title{S3 method to glance at objects of class boottest}
\usage{
\method{glance}{mboottest}(x, ...)
}
\arguments{
\item{x}{object of type mboottest}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
A single row summary "glance" of an object of type boottest
- lists characteristics of the input regression model
}
\description{
S3 method to glance at objects of class boottest
}
\examples{
\dontrun{
requireNamespace("fwildclusterboot")
data(voters)
lm_fit <- lm(
proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,
  data = voters
)
mboot <- mboottest(
    object = lm_fit,
    clustid = "group_id1",
    B = 999,
    R = R
)
generics::glance(mboot)
}
}
