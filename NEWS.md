# fwildclusterboot 0.8.1

+ Moves data pre-processing from `model.frame` methods to `model_matrix` methods, and thereby fixes a multicollinearity bug that occured when `lm()` or `fixest()` silently deleted multicollinar variable(s).

This also allows to use a range of so far 'forbidden' functionalities, in particular of `fixest::feols()` - it is now possible to run `boottest()` after `feols()` models that use syntactic sugar, e.g. 


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

feols_fits <- fixest::feols(proposition_vote ~ csw(treatment, ideology1) | sw(Q1_immigration, Q2_defense), data = voters)
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
SchoolingReturns <- SchoolingReturns[rowMeans(sapply(SchoolingReturns, is.na)) == 0,]
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
Prior to v 0.6, by default, no small sample adjustments regarding the sample size N and the number of estimated parameters k were applied. The changes in v0.6 may slightly affect the output of `boottest()`. For exact reproducibility of previous results, set `adj = FALSE`. Setting `adj = TRUE` will not affect p-values and confidence intervals for *oneway clustering*, but the internally calculated t-stat, which is divided by $\sqrt{(N-k)/(N-1)}$. For *twoway* clustering, it might affect the number and order of invalid bootstrapped t-statistics (due to non-positive definite covariance matrices) and, through this channel, affect bootstrapped inferential parameters.

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
