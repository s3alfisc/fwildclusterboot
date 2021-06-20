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
