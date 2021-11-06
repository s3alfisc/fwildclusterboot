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
