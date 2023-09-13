# CRAN release: fwildclusterboot 0.15

- For rademacher weights, switches to `dqrng::dqrrademacher`. This leads to substabtial 
  performance increases (see below). But the random number generation changes slightly, so 
  you might no longer reproduce exactly the same weights draw under the same seeds, and in turn, 
  your bootstrap inferences might change slightly. 
- For other changes, see the changelog for versions `0.13-0.14.3`, all of which have not 
  been published on CRAN. 
  
```r
  > library(bench)
> 
> n <- 999999 * 100
> 
> mark(
+   dqrrademacher(n), 
+   dqrng::dqsample(
+     x = c(-1L, 1L),
+     size = n,
+     replace = TRUE
+   ), 
+   iterations = 5, 
+   check = FALSE
+ )
# A tibble: 2 × 13
  expression                                                   min median itr/s…¹ mem_a…² gc/se…³ n_itr  n_gc
  <bch:expr>                                                <bch:> <bch:>   <dbl> <bch:b>   <dbl> <int> <dbl>
1 dqrrademacher(n)                                           123ms  166ms    5.25   381MB    3.15     5     3
2 dqrng::dqsample(x = c(-1L, 1L), size = n, replace = TRUE)  615ms  647ms    1.47   763MB    1.47     5     5
# … with 5 more variables: total_time <bch:tm>, result <list>, memory <list>, time <list>, gc <list>, and
#   abbreviated variable names ¹​`itr/sec`, ²​mem_alloc, ³​`gc/sec`
```


# fwildclusterboot 0.14.3

- Fix a bug with CI inversion when `r` was set to be close to the estimated parameter. (CI inversion failed). See [#138](https://github.com/s3alfisc/fwildclusterboot/issues/138). Thanks to Achim Zeileis & team for raising this issue!


# fwildclusterboot 0.14.2

Minor fixes for CRAN release.


# fwildclusterboot 0.14.1

- brings back the `print()` method, it had a use case after all
- some fixes to the documentation


# fwildclusterboot 0.14

## Breaking Changes

- the `print.boottest()` and `print.mboottest()` method have been deprecated, as both did not have a distinct use case. 
- Bugfix: `boottest()` should never have run with `fixest::feols()` and 
  varying slopes syntax via `var1[var2]`. Unfortunately it did for the heteroskedastic bootstrap - it's a bug. I am very sorry if you are affected by this! This version adds an error message for this case.

## Performance

Version 0.14 ...

- sparsifies the "fast and reliable" bootstraps - bootstrap types 31, 33, 13 (which leads to good speed gains for problems with high dimensional fixed effects)
- allows to project out cluster fixed effects when running the "fast and reliable" algorithms "11" and "31"
- computes the generalized inverse `pinv` via rcpp eigen instead of `MASS::ginv()` whenever `Matrix::solve()` fails 
- unlocks parallelization (nthreads was internally set to 1 for some reason)


## rOpenSci Review feedback 

- update docs:
  - add a vignette on wild bootstrap concepts (wild bootstrap 101)
  - better explanation of plot method in docs and vignette 
  - some guidelines on how to turn messages and warnings off
- reorganization of ropensci ssr tags into code
- it is now possible to interrupt rcpp loops


## Misc

- throws a clear error message when the subcluster bootstrap is tried for the fast and reliable algos (currently not supported)
- bumps the required `WildBootTests.jl` version to `0.9.7`


# fwildclusterboot 0.13

## Potentially Breaking Changes: 

* `boottest()`, `mboottest()` and `boot_aggregate()`no longer have a dedicated `seed` argument. From version 0.13, reproducibility of results can only be controlled by setting a **global seed** via `drqng::dqset.seed()` and `set.seed()`. For more context, see the discussion below. As a consequence, results produced via old versions of `fwildlcusterboot` are no longer exactly reproducible. 

* When the bootstrap is run via `engine = "WildBootTests.jl"`, the bootstrapped t-statistics and the original t-statistic are now returned as vectors (to align with the results from other `enginges`). Previously, they were returned as matrices. 

## Other Changes: 

* `boottest()` receives a new argument, `sampling`, which controls if random numbers are drawn via functions from `base` or the `dqrng` package. 
* Some code refactoring. All bootstrap algorithms and their associated files have been renamed (e.g. `boot_algo2.R` is not called `boot_algo_fastnwild.R`, etc.).
* Much nicer error and message formatting, via `rlang::abort()`, `warn()` and `inform()`. `rlang` is added as a dependency.

## Background on the Change to Seeding

Prior to the changes introduced in `v0.13`, `boottest()` will always call `set.seed()` or `dqrng::dqset.seed()` internally, regardless of whether the `seed` argument is specified or not (in the ladder case, it will create an internal seed by randomly drawing from a large set of integers). I consider this harmless, as setting seeds inside `boottest()` in this way does not affect the reproducibility of scripts run end-to-end.

However, I have learned that is generally considered bad practice to overwrite global variables without notification - for example, the authors of [numpy](https://numpy.org/doc/stable/reference/random/generated/numpy.random.seed.html) have deprecated their `np.random.seed()` function for this reason. 

Here is a quick example on what happens if a function "reseeds": it affects the future chain of random draws. 

```r
fn_reseed <- function(x){set.seed(x)}

set.seed(123)
rnorm(1)
# [1] -0.5604756
fn_reseed(1)
rnorm(1)
# [1] -0.6264538

set.seed(123)
rnorm(1); rnorm(1)
# [1] -0.5604756
# [1] -0.2301775
```
The two 'second' calls to `rnorm(1)` are based on different global seed states. 

As a result, I have decided to deprecate the `seed' function argument. Random number generation must now **be** set outside of `boottest()` using `set.seed()` and `dqrng::dqset.seed()`. 

This means that bootstrap results generated via versions < 0.13 will no longer be exactly replicable under the new version, but with a sufficiently large number of bootstrap iterations, this change should not affect any of your conclusions. 

# fwildclusterboot 0.12.1

This is a hot-fix release which turns of tests on CRAN that fail in non-standard CRAN test environments. 

# fwildclusterboot 0.12

This is the first CRAN release since version `0.9`. It comes with a set of new features, but also potentially breaking changes. This section summarizes all developments since version `0.9`. 

### Potentially breaking changes: 
* `boottest()'s` function argument `boot_algo` has been renamed to `engine`
* the `setBoottest_boot_algo()` function was renamed to `setBoottest_engine()`

### Bug fixes and internal changes

* When a multi-parameter hypothesis of the form R beta = r was tested, the *heteroskedastic* wild bootstrap would nevertheless always test 
"beta_k = 0" vs "beta_k != 0", with "beta_k = param". I am sorry for that bug!
* The `Matrix.utils` package is at danger of CRAN removal - it has been replaced by custom functions for internal use.

### New features and Improvements

+ A new function argument has been added - `bootstrap_type`. In combination with the `impose_null` function argument, it allows to choose between different cluster bootstrap types - WCx11, WCx13, WCx31, WCx33. For more details on these methods, see the working paper by [MacKinnon, Nielsen & Webb (2022)](https://www.econ.queensu.ca/sites/econ.queensu.ca/files/wpaper/qed_wp_1485.pdf). Currently, these new bootstrap types only compute p-values. Adding support for confidence intervals is work in progress.
+ A `boot_aggregate()` method now supports the aggregation of coefficients in staggered difference-in-differences following the methods by [Sun & Abraham (2021, Journal of Econometrics)](https://arxiv.org/pdf/1804.05785.pdf) in combination with the `sunab()` function from [`fixest`](https://lrberge.github.io/fixest/reference/sunab.html)has been added. Essentially, `boot_aggregate()` is a copy of [`aggregate.fixest`](https://lrberge.github.io/fixest/reference/aggregate.fixest.html): the only difference is that inference is powered by a wild bootstrap.
+ The heteroskedastic bootstrap is now significantly faster, and WCR21 and WCR31 versions are now supported (i.e. HC2 and HC3 'imposed' on the bootstrap dgp.)


# fwildclusterboot 0.11.3 

+ significant speed improvements for the heteroskedastic bootstrap


# fwildclusterboot 0.11.2 

+ significant speed improvements for the x1 bootstrap algorithms,
  `bootstrap_type %in% c("11", "31")`, both for WCR and WCU

# fwildclusterboot 0.11.1 

### New bootstrap algorithms following MNW (2022)

+ A new function argument has been added - `bootstrap_type`. In combination with the `impose_null` function argument, it allows to choose between different cluster bootstrap types - WCx11, WCx13, WCx31, WCx33. For more details on these methods, see the working paper by [MacKinnon, Nielsen & Webb (2022)](https://www.econ.queensu.ca/sites/econ.queensu.ca/files/wpaper/qed_wp_1485.pdf).

### `boot_aggregate()` method for Sun-Abrahams Event Studies

A `boot_aggregate()` method to supports the aggregation of coefficients in staggered difference-in-differences following the methods by [Sun & Abraham (2021, Journal of Econometrics)](https://arxiv.org/pdf/1804.05785.pdf) in combination with the `sunab()` function from [`fixest`](https://lrberge.github.io/fixest/reference/sunab.html)has been added. Essentially, `boot_aggregate()` is a copy of [`aggregate.fixest`](https://lrberge.github.io/fixest/reference/aggregate.fixest.html): the only difference is that inference is powered by a wild bootstrap.

### Other syntax changes, potentially breaking! 

+ The `boot_algo` function argument has been renamed to `engine`.
+ The `setBoottest_boot_algo()` function has been renamed to `setBoottest_engine()`.
In consequence, the syntax introduced in 0.11 changes to 


```
boottest(
  lm_fit, 
  param = ~treatment, 
  clustid = ~group_id1,
  B = 9999, 
  impose_null = TRUE,
  engine = "R", 
  bootstrap_type = "11"
)
```

To run everything through `WildBootTests.jl`, you would have to specify 

```
boottest(
  lm_fit, 
  param = ~treatment, 
  clustid = ~group_id1,
  B = 9999, 
  impose_null = TRUE,
  engine = "WildBootTests.jl", 
  bootstrap_type = "11"
)
```


# fwildclusterboot 0.11

+ This release introduces new wild cluster bootstrap variants as described in [MacKinnon, Nielsen & Webb (2022)](https://www.econ.queensu.ca/sites/econ.queensu.ca/files/wpaper/qed_wp_1485.pdf). The implementation is still quite bare-bone: it only allows to test hypotheses of the form $\beta_k = 0$ vs $\beta_k \neq 0$, does not allow for regression weights or fixed effects, and further does not compute confidence intervals. 

You can run one of the 'new' variants - e.g. the "WCR13", by specifying the `bootstrap_type` function argument accordingly: 

```
boottest(
  lm_fit, 
  param = ~treatment, 
  clustid = ~group_id1,
  B = 9999, 
  impose_null = TRUE,
  engine = "R", 
  bootstrap_type = "31"
)
```


# fwildclusterboot 0.10

+ introduces a range of new methods: `nobs()`, `pval()`, `teststat()`, `confint()` and `print()`
+ multiple (internal) changes for ropensci standards alignment
+ drop the `t_boot` (`teststat_boot`) function arguments -> they are now
  TRUE by default
+ fix a bug in the lean algorithms - it always tested hypotheses of the form 
  beta = 0 instead of R'beta = r, even when R != 1 and r != 0
+ enable full enumeration for R-lean tests 
+ enable deterministic 'full enumeration tests' - these are exact

# fwildclusterboot 0.9

+ v0.9 moves data pre-processing from `model.frame` methods to `model_matrix` methods. I had wanted to do so for a while, but issue #42, as raised by Michael Topper, has finally convinced me to start working on this project. 

+ Moving to `model_matrix` methods unlocks new functionality for how `boottest()` plays with `fixest` objects - it is now possible to run `boottest()` after `feols()` models that use syntactic sugar:

```
library(fwildclusterboot)
library(fixest)

data(voters)
feols_fit <- feols(proposition_vote ~ i(treatment, ideology1) ,
    data = voters
)
boot1 <- boottest(feols_fit,
    B = 9999,
    param = "treatment::0:ideology1",
    clustid = "group_id1"
)

feols_fits <- fixest::feols(proposition_vote ~ treatment | sw(Q1_immigration, Q2_defense), data = voters)
res <- lapply(feols_fits, \(x) boottest(x, B = 999, param = "treatment", clustid = "group_id1"))  

voters$split <- sample(1:2, nrow(voters), TRUE)
feols_fits <- fixest::feols(proposition_vote ~ treatment, split = ~split, data = voters)

res <- lapply(feols_fits, \(x) boottest(x, B = 999, param = "treatment", clustid = "group_id1"))  
```

Some formula sugar still leads to errors, e.g. 

```
feols_fit2 <- feols(proposition_vote ~ treatment | Q1_immigration^Q2_defense,
    data = voters
)

boot1 <- boottest(feols_fit2,
    B = 9999,
    param = "treatment",
    clustid = "group_id1"
)
```

+ The release further fixes a multicollinearity bug that occured when `lm()` or `fixest()` silently deleted multicollinar variable(s). Thanks to Kurt Schmidheiny for reporting!

+ The `na_omit` function argument has been dropped. If the cluster variable is not included in the regression model, it is now not allowed to contain NA values. 

+ Several function arguments can now be fed to `boottest()` as formulas (`param`, `clustid`, `bootcluster`, `fe`).

```
data(voters)
feols_fit <- feols(proposition_vote ~ treatment ,
    data = voters
)
boot <- boottest(feols_fit,
    B = 9999,
    param = ~ treatment,
    clustid = ~ group_id1
)
```

# fwildclusterboot 0.8

### Two new bootstrap algorithms: 'WildBootTests.jl' and 'R-lean'

#### boot_algo = 'WildBootTests.jl'

+ `fwildclusterboot` now supports calling [WildBootTests.jl](https://github.com/droodman/WildBootTests.jl), which is a very fast Julia implementation of the wild cluster bootstrap algorithm. To do so, a new function argument is introduced, `boot_algo`, through which it is possible to control the executed bootstrap algorithm. 

```{r}
# load data set voters included in fwildclusterboot
data(voters)
# estimate the regression model via lm
lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , data = voters)
boot_lm <- boottest(
  lm_fit, 
  clustid = "group_id1", 
  param = "treatment", 
  B = 9999, 
  boot_algo = "WildBootTests.jl"
)
```
+ WildBootTests.jl is (after compilation) orders of magnitudes faster than `fwildclusterboot's` native R implementation, and speed gains are particularly pronounced for large problems with a large number of clusters and many bootstrap iterations. 

+ Furthermore, `WildBootTests.jl` supports a range of models and tests that were previously not supported by `fwildclusterboot`: most importantly a) wild cluster bootstrap tests of multiple joint hypotheses and b) the WRE bootstrap by Davidson & MacKinnon for instrumental variables estimation. On top of the cake ... the WRE is really fast. 

```{r}
library(ivreg)
data("SchoolingReturns", package = "ivreg")
# drop all NA values from SchoolingReturns
SchoolingReturns <- na.omit(SchoolingReturns)
ivreg_fit <- ivreg(log(wage) ~ education + age + ethnicity + smsa + south + parents14 |
                           nearcollege + age  + ethnicity + smsa + south + parents14, data = SchoolingReturns)

boot_ivreg <- boottest(
  object = ivreg_fit,
  B = 999,
  param = "education",
  clustid = "kww",
  type = "mammen",
  impose_null = TRUE
)
generics::tidy(boot_ivreg)
#              term  estimate statistic   p.value    conf.low conf.high
# 1 1*education = 0 0.0638822  1.043969 0.2482482 -0.03152655 0.2128746
```

+ For guidance on how to install and run `WildBooTests.jl`, have a look at the associated [article](https://s3alfisc.github.io/fwildclusterboot/articles/WildBootTests.html).

+ Also, note that running the wild cluster bootstrap through `WildBootTests.jl` is often very memory-efficient.


#### boot_algo = 'R-lean' 

A key limitation of the vectorized 'fast' cluster bootstrap algorithm as implemented in `fwildclusterboot` is that it is very memory-demanding. For 'larger' problems, running `boottest()` might lead to out-of-memory errors. To offer an alternative, `boottest()` now ships a 'new' rcpp- and loop-based implementation of the wild cluster bootstrap (the 'wild2' algorithm in Roodman et al).

```{r}
boot_lm <- boottest(
  lm_fit, 
  clustid = "group_id1", 
  param = "treatment", 
  B = 9999, 
  boot_algo = "R-lean"
)
```

### Heteroskeadstic Wild Bootstrap 

It is now possible to run `boottest()` without specifying a `clustid` function argument. In this case, `boottest()` runs a heteroskedasticity-robust wild bootstrap (HC1), which is implemented in c++. 

```{r}
boot_hc1 <- boottest(lm_fit, param = "treatment", B = 9999)
summary(boot_hc1)
```

### `boottest()` function argument `beta0` deprecated

For consistency with `WildBootTests.jl`, the `boottest()` function argument `beta0` is now replaced by a new function argument, `r`. 

### Frühjahrsputz

I have spent some time to clean up `fwildclusterboot's` internals, which should now hopefully be more readable and easier to maintain. 

### Testing

`fwildclusterboot` is now pre-dominantly tested against `WildBootTests.jl`. Tests that depend on Julia are by default not run on CRAN, but are regularly run on Mac, Windows and Linux via [github actions](https://github.com/s3alfisc/fwildclusterboot/actions/workflows/R-CMD-check.yaml).


# fwildclusterboot 0.7

+ Bug fixes, see issues [#26](https://github.com/s3alfisc/fwildclusterboot/issues/26) and [#27](https://github.com/s3alfisc/fwildclusterboot/issues/27) regarding preprocessing for fixest when weights are passed to feols() as a formula or when cluster is specified in fixest as a column vector. 


# fwildclusterboot 0.6

+ Bug fix: for one-sided hypotheses for the WRU bootstrap (if impose_null = FALSE), the returned p-values were incorrect - they were reported as 'p', but should have been '1-p'. E.g. if the reported p-values was reported as 0.4, it should have been reported as 0.6. 
+ A new function argument `ssc` gives more control over the small sample adjustments made within `boottest()`. It closely mirrors the `ssc` argument in `fixest`. The only difference is that `fwildclusterboot::boot_ssc()'s` `fixef.K` argument currently has only one option, `'none'`, which means that the fixed effect parameters are discarded when calculating the number of estimated parameters k. 
The default argument of `boot_ssc()` are `adj = TRUE, fixef.K = "none", cluster.adj = TRUE` and `cluster.df = "conventional"`. In fixest, the `cluster.df` argument is `"min"` by default. 
Prior to v 0.6, by default, no small sample adjustments regarding the sample size N and the number of estimated parameters k were applied. The changes in v0.6 may slightly affect the output of `boottest()`. For exact reproducibility of previous results, set `adj = FALSE`. Setting `adj = TRUE` will not affect p-values and confidence intervals for *oneway clustering*, but the internally calculated t-stat, which is divided by sqrt(N-k)/(N-1). For *twoway* clustering, it might affect the number and order of invalid bootstrapped t-statistics (due to non-positive definite covariance matrices) and, through this channel, affect bootstrapped inferential parameters.

+ Testing: unit tests are now run on [github actions](https://github.com/s3alfisc/fwildclusterboot/actions/workflows/R-CMD-check.yaml) against [wildboottestjlr](https://github.com/s3alfisc/wildboottestjlr), which is a [JuliaConnectoR](https://github.com/stefan-m-lenz/JuliaConnectoR) based wrapper around [WildBootTests.jl](https://github.com/droodman/WildBootTests.jl), a Julia implementation of the fast wild cluster bootstrap algorithm. 
+ Additionally, minor speed tweaks. 

# fwildclusterboot 0.5.1

+ Fixes a bug with Mammen weights introduced in version 0.5 -> switch back to `sample()` function. To guarantee reproducibilty with Mammen weights, either a seed 
 needs to be specified in `boottest()` or a global seed needs to be set via `set.seed()`.
+ Deletes some unnecessary computations from boot_algo2() -> speed improvements
+ For B = 2^(#number of clusters), Rademacher weights should have been enumerated - 
  instead, they were drawn randomly and enumeration only occured for B > 2^(#number of clusters). Now, enumeration occurs if B >= 2^(#number of clusters).

# fwildclusterboot 0.5

+ Version 0.5 fixes an error for the bootstrap with weighted least squares introduced with version 0.4. All unit tests 
  that compare fwildclusterboot with weighted least squares results from boottest.stata pass. In particular, enumerated cases pass with exact equality (in such cases, the bootstrap weights matrices are exactly identical in both R and Stata).
+ `boottest()` now stops if `fixest::feols()` deletes non-NA values (e.g. singleton fixed effects deletion) and asks the user to delete such rows prior to estimation via `feols()` & `boottest()`. Currently, `boottest()'s` pre-processing cannot handle such deletions - this remains future work.
+ To align `fwildclusterboot` with Stata's boottest command (Roodman et al, 2019), Mammen weights are no longer enumerated in `fwildclusterboot::boottest()`.
+ `boottest()` no longer sets an internal seed (previously set.seed(1)) if no seed is provided as a function argument. 
+ Sampling of the bootstrap weights is now powered by the `dqrng` package, which speeds up the creation of the bootstrap weights matrix. To set a "global" seed, one now has use the `dqset.seed()` function from the `dqrng package`, which is added as a dependency.


# fwilclusterboot 0.4

+ New feature I: `boottest()` now allows for univariate tests that involve multiple 
  variables. E.g. one can now test hypothesis as `var1 + var2 = c` where c is a scalar. More details on the syntax can be found in the vignette. All methods of for 
  objects of class `boottest` have been updated.
+ New feature II: `boottest()` now also supports "equal-tailed" p-values and one-sided hypotheses. For one-sided tests, confidence intervals are currently not supported. 
+ Internal changes: To allow for multivariable tests, the `boot_algo2()` function has slightly been modified. `invert_p_val2()` is superseded by `invert_p_val()`. 
+ Further, a CRAN error is fixed - some tests for exact equality failed with relative difference e-05 on openBLAS. In consequence, all exact tests are set to reltol = 1e-04. 
+ Make better use of functionality of `dreamerr` package. 

# fwildclusterboot 0.3.7

+ Bug fix: the output of `boottest()` varied depending on the class of the 
  input fixed effects for regressions both via `lfe::felm()` and `fixest::feols()`. 
  This bug occurred because `boottest()` does not work with a pre-processed model.frame object from either `felm()` or `feols()` but works with the original input data. While both `felm()` and `feols()` change non-factor fixed effects variables to factors internally, `boottest()` did not check but implicitely assumed that all fixed effects used in the regression models are indeed factors in the original data set. As a consequence, if one or more fixed effects were e.g. numeric, `boottest()` would produce incorrect results without throwing an error. 
  With version 0.3.7, `boottest()` checks internally if all variables in the original data set which are used as fixed effects are factor variables and if not, changes them to factors. 
  Thanks for timotheedotc for raising the issue on github, which can be found here: https://github.com/s3alfisc/fwildclusterboot/issues/14. 
+ Some tests have been added that compare output from `boottest()` with the wild cluster bootstrap implemented via `clusterSEs`.


# fwildclusterboot 0.3.6
  
+ Bug fix regarding suggested packages and CRAN: [see github issue #12](https://github.com/s3alfisc/fwildclusterboot/issues/12). 
Added `if(requireNamespace("pkgname"))` statements for suggested packages in the vignettes,
examples and tests. Note that unit tests will now only execute on CRAN if both `fixest` and `lfe` 
can be installed on the OS.


# fwildclusterboot 0.3.5

+ Bug fix: For Rademacher and Mammen weights and cases where (2^ number of clusters) < # boostrap iterations,  (deterministic ) full enumeration should have been employed for sampling the bootstrap weights.
Full enumeration means the following: for e.g. 6 numbers of clusters, only 
2^6 = 64 unique draws from either the Rademacher or Mammen distributions exists. 
Therefore, `boottest()` overwrites the user-provided number of bootstrap iterations 
to $B = \text{(2^ number of clusters)}$  if a larger number is chosen. 
The bug now occured because the bootstrap weights were drawn **randomly with 
replacement** instead of using **full enumeration**.
Note: full enumeration was introduced with version 0.3.3. Thanks to fschoner for finding the bug! [see github issue #11](https://github.com/s3alfisc/fwildclusterboot/issues/11)

+ Bug fix: A small bug has been fixed related to missing values in the cluster variables. 

+ By default, `boottest()` now sets an internal seed if no seed is provided by 
  the user via the `seed` function argument.

+ Several improvements to the documentation. 


# fwildclusterboot 0.3.4

+ Fix CRAN errors caused by a small bug in the vignette

# fwildclusterboot 0.3.3

+ implements full enumeration for Rademacher and Mammen Weights if $2^k<B$, where k is the number of clusters and B the number of bootstrap iterations

# fwildclusterboot 0.3.2

+ Fixes a CRAN test error message for Oracle Solaris. 

# fwildclusterboot 0.3.1 

+ A `glance.boottest()` method was added, which enables the use of the `modelsummary` package with `fwildclusterboot`. 
+ The `tidy.boottest()` method is no longer exported. You can still access it via `fwildclusterboot:::tidy.boottest()` or by loading the `generics` package via `library(generics)`.

# fwildclusterboot 0.3.0

+ Additional performance improvements through parallelization. By default, 
  `boottest()` uses half the available threads for parallel execution. The 
  number of threads can be set via the `nthreads` function argument.
+ Additional function arguments for `boottest()` - the user can now set the 
  tolerance and maximum number of iterations for the calculation of
  confidence intervals. By default, `tol = 1e-6` and 
  `maxiter = 10`.
+ The package no longer depends on `data.table` and `fabricatr` - both are now
  only suggested. Further, the package now comes with an example data set 
  'voters'.


# fwildclusterboot 0.2.0

Add support for 

+ tests of two-sided, univariate hypotheses
+ regression weights
+ the subcluster bootstrap
+ restricted (WCR) and unrestricted (WCU) bootstrap
