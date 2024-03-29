% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boottest_ivreg.R
\name{boottest.ivreg}
\alias{boottest.ivreg}
\title{Fast wild cluster bootstrap inference for object of class lm}
\usage{
\method{boottest}{ivreg}(
  object,
  clustid,
  param,
  B,
  bootcluster = "max",
  conf_int = TRUE,
  R = NULL,
  r = 0,
  sign_level = 0.05,
  type = "rademacher",
  impose_null = TRUE,
  p_val_type = "two-tailed",
  tol = 1e-06,
  floattype = "Float64",
  getauxweights = FALSE,
  maxmatsize = NULL,
  bootstrapc = FALSE,
  liml = FALSE,
  fuller = NULL,
  kappa = NULL,
  arubin = FALSE,
  ssc = boot_ssc(adj = TRUE, fixef.K = "none", cluster.adj = TRUE, cluster.df =
    "conventional"),
  ...
)
}
\arguments{
\item{object}{An object of class lm}

\item{clustid}{A character vector or rhs formula containing the names of
the cluster variables}

\item{param}{A character vector or rhs formula of length one. The name of
the regression
coefficient for which the hypothesis is to be tested}

\item{B}{Integer. The number of bootstrap iterations. When the number of
clusters is low,
increasing B adds little additional runtime}

\item{bootcluster}{A character vector or rhs formula of length 1. Specifies
the bootstrap clustering variable or variables. If more
than one variable is specified, then bootstrapping is clustered
by the intersections of
clustering implied by the listed variables. To mimic the behavior
of stata's boottest command,
the default is to cluster by the intersection of all the variables
specified via the \code{clustid} argument,
even though that is not necessarily recommended (see the paper by
Roodman et al cited below, section 4.2).
Other options include "min", where bootstrapping is clustered by
the cluster variable with the fewest clusters.
Further, the subcluster bootstrap (MacKinnon & Webb, 2018) is
supported - see the \code{vignette("fwildclusterboot", package = "fwildclusterboot")} for details.}

\item{conf_int}{A logical vector. If TRUE, boottest computes confidence
intervals by test inversion. If FALSE, only the p-value is returned.}

\item{R}{Hypothesis Vector giving linear combinations of coefficients.
Must be either NULL or a vector of the same length as \code{param}. If NULL,
a vector of ones of length param.}

\item{r}{A numeric. Shifts the null hypothesis
H0: param = r vs H1: param != r}

\item{sign_level}{A numeric between 0 and 1 which sets the significance level
of the inference procedure. E.g. sign_level = 0.05
returns 0.95\% confidence intervals. By default, sign_level = 0.05.}

\item{type}{character or function. The character string specifies the type
of boostrap to use: One of "rademacher", "mammen", "norm", "gamma"
and "webb". Alternatively, type can be a function(n) for drawing
wild bootstrap factors. "rademacher" by default.
For the Rademacher and Mammen distribution, if the number of
replications B exceeds
the number of possible draw ombinations, 2^(#number of clusters),
then \code{boottest()}
will use each possible combination once (enumeration).}

\item{impose_null}{Logical. Controls if the null hypothesis is imposed on
the bootstrap dgp or not. Null imposed \code{(WCR)} by default.
If FALSE, the null is not imposed \code{(WCU)}}

\item{p_val_type}{Character vector of length 1. Type of p-value.
By default "two-tailed". Other options include "equal-tailed",
">" and "<".}

\item{tol}{Numeric vector of length 1. The desired accuracy
(convergence tolerance) used in the root finding procedure to find
the confidence interval.
Relative tolerance of 1e-6 by default.}

\item{floattype}{Float64 by default. Other option: Float32. Should floating
point numbers in Julia be represented as 32 or 64 bit?}

\item{getauxweights}{Logical. FALSE by default. Whether to save auxilliary
weight matrix (v)}

\item{maxmatsize}{NULL by default = no limit. Else numeric scalar to set
the maximum size of auxilliary weight matrix (v), in gigabytes}

\item{bootstrapc}{Logical scalar, FALSE by default. TRUE  to request
bootstrap-c instead of bootstrap-t}

\item{liml}{Logical scalar. False by default. TRUE for liml or fuller liml}

\item{fuller}{NULL by default. Numeric scalar. fuller liml factor}

\item{kappa}{Null by default. fixed <U+03BA> for k-class estimation}

\item{arubin}{False by default. Logical scalar. TRUE for Anderson-Rubin Test.}

\item{ssc}{An object of class \code{boot_ssc.type} obtained with the function
\code{\link[=boot_ssc]{boot_ssc()}}. Represents how the small sample
adjustments are computed. The defaults are \verb{adj = TRUE, fixef.K = "none", cluster.adj = "TRUE", cluster.df = "conventional"}.
You can find more details in the help file for \code{boot_ssc()}.
The function is purposefully designed to mimic
fixest's \code{\link[fixest:ssc]{fixest::ssc()}} function.}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
An object of class \code{boottest}

\item{p_val}{The bootstrap p-value.}
\item{conf_int}{The bootstrap confidence interval.}
\item{param}{The tested parameter.}
\item{N}{Sample size. Might differ from the regression sample size if
the cluster variables contain NA values.}
\item{boot_iter}{Number of Bootstrap Iterations.}
\item{clustid}{Names of the cluster Variables.}
\item{N_G}{Dimension of the cluster variables as used in boottest.}
\item{sign_level}{Significance level used in boottest.}
\item{type}{Distribution of the bootstrap weights.}
\item{impose_null}{Whether the null was imposed on the bootstrap dgp or not.}
\item{R}{The vector "R" in the null hypothesis of interest Rbeta = r.}
\item{r}{The scalar "r" in the null hypothesis of interest Rbeta = r.}
\item{point_estimate}{R'beta. A scalar: the constraints vector times the
regression coefficients.}
\item{grid_vals}{All t-statistics calculated while calculating the
confidence interval.}
\item{p_grid_vals}{All p-values calculated while calculating the confidence
interval.}
\item{t_stat}{The 'original' regression test statistics.}
\item{t_boot}{All bootstrap t-statistics.}
\item{regression}{The regression object used in boottest.}
\item{call}{Function call of boottest.}
\item{engine}{The employed bootstrap algorithm.}
\item{nthreads}{The number of threads employed.}
}
\description{
\code{boottest.ivreg} is a S3 method that allows for fast wild cluster
bootstrap inference for objects of class ivreg by  implementing
the fast wild bootstrap algorithm developed in Roodman et al., 2019
for instrumental variable models (WRE, Davidson & McKinnon, 2010)
}
\section{Setting Seeds}{

To guarantee reproducibility, you need to
set a global random seed via \code{set.seed()}
}

\section{Run \code{boottest} quietly}{

You can suppress all warning and error messages by setting the following global
options:
\code{options(rlib_warning_verbosity = "quiet")}
\code{options(rlib_message_verbosity = "quiet")}
Not that this will turn off all warnings (messages) produced via \code{rlang::warn()} and
\code{rlang::inform()}, which might not be desirable if you use other software build on
\code{rlang}, as e.g. the \code{tidyverse}.
}

\examples{
\dontrun{
requireNamespace("ivreg")
requireNamespace("fwildclusterboot")

# drop all NA values from SchoolingReturns
SchoolingReturns <- na.omit(SchoolingReturns)
ivreg_fit <- ivreg(log(wage) ~ education + age +
  ethnicity + smsa + south + parents14 |
  nearcollege + age + ethnicity + smsa
    + south + parents14,
data = SchoolingReturns
)

boot_ivreg <- boottest(
  object = ivreg_fit,
  B = 999,
  param = "education",
  clustid = "kww",
  type = "mammen",
  impose_null = TRUE
)
summary(boot_ivreg)
print(boot_ivreg)
plot(boot_ivreg)
nobs(boot_ivreg)
pval(boot_ivreg)
confint(boot_ivreg)
generics::tidy(boot_ivreg)
}

}
\references{
Roodman et al., 2019, "Fast and wild: Bootstrap inference in
STATA using boottest", The STATA Journal.
(\url{https://ideas.repec.org/p/qed/wpaper/1406.html})

Cameron, A. Colin, Jonah B. Gelbach, and Douglas L. Miller.
"Bootstrap-based improvements for inference with clustered errors."
The Review of Economics and Statistics 90.3 (2008): 414-427.

Cameron, A.Colin & Douglas L. Miller. "A practitioner's
guide to cluster-robust inference" Journal of Human Resources (2015)
\doi{doi:10.3368/jhr.50.2.317}

Davidson & MacKinnon. "Wild Bootstrap Tests for IV regression"
Journal of Economics and Business Statistics (2010)
\doi{https://doi.org/10.1198/jbes.2009.07221}

MacKinnon, James G., and Matthew D. Webb.
"The wild bootstrap for few (treated) clusters."
The Econometrics Journal 21.2 (2018): 114-135.

MacKinnon, James G., and Matthew D. Webb.
"Cluster-robust inference: A guide to empirical practice"
Journal of Econometrics (2022)
\doi{https://doi.org/10.1016/j.jeconom.2022.04.001}

MacKinnon, James. "Wild cluster bootstrap confidence
intervals." L'Actualite economique 91.1-2 (2015): 11-33.

Webb, Matthew D. Reworking wild bootstrap based inference
for clustered errors. No. 1315. Queen's Economics Department Working
Paper, 2013.
}
