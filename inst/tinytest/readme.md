All unit tests of `fwildclusterboot` can be found in https://github.com/s3alfisc/fwildclusterboot/tree/master/inst/tinytest. 

All tests run on github actions (for each commit) but are turned off on CRAN for two reasons: First, most tests depend on ``WildBootTests.jl` and Julia - currently, it is not possible to test against Julia on CRAN. Second, most of these tests take a quite a bit of time. Some run for so long that CRAN would not allow them anyways. For all tests, the most up-to-date version of Julia and `WildBootTests.jl` is used. 

## Main Tests 

+ [test_model_equivalence]() tests if different ways to specify models through supported model functions produce equivalent results - e.g. does running `boottest()` on an object created via `feols(..., cluster ~ var)` returns the same results as running `boottest()` on ``feols(..., cluster = "var")`?

+ [test_r_julia]() Tests for objects of type `lm` if `boot_algo = R` and `boot_algo = WildBootTests.jl` produce equivalent results. It tests for approximate equality (with tolerance) for large N and large B cases and for exact equality under enumeration. 

## Other tests 

+ [test_tstat_equivalence.R]() tests if `boottest()` and `waldboottest()` produce non-bootstrapped t-statistics and F/Chi-Square statistics that exactly match those produced by `fixest::feols()`, `fixest::wald()` and `ivreg::ivreg()`.
+ [test_small_n_cluster.R]() tests if under enumeration, the set of distinct bootstrapped t-statistics / p-values is finite. 
+ [test_crosstab.R]() tests the `crosstab()` function employed in `boot_algo2()`.
+ [test_error_warning.R]() tests if error messages and warnings pop up when appropriate. For example, is a warning message returned if a variable specified as a cluster contains NA values and is **not included** in the original model formula?
+ [test_seed.R]() Do methods of type `boottest()` and `waldboottest()` return the same results under the same seed?
+ [uncategorized_tests.R]() collect tests that do not properly fit any test suites above.

## Tests that should be there but are not (yet)

+ tests of `tidy`, `glance`, `summary` and `plot` methods