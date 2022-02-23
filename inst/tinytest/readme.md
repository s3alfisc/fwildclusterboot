All unit tests of `fwildclusterboot` can be found in https://github.com/s3alfisc/fwildclusterboot/tree/master/inst/tinytest. 

Roughly, they consist of two sets of tests. The first set of tests checks if `boottest` produces equivalent results for equivalent regression objects of types `lm`, `fixest` and `felm`. The main goal is to check that the internal pre-processing within `boottest()` is aligned. These tests are labelled under *pre-processing tests*. 


+ [internal_method_consistency]() tests if different ways to specify models through supported model functions produce equivalent results - e.g. does running `boottest()` on an object created via `feols(..., cluster ~ var)` returns the same results as running `boottest()` on ``feols(..., cluster = "var")`?

+ [external_method_consistency]() tests if `boottest.fixest`, `boottest.lm` and `boottest.felm` produce equivalent results for equivalent specifications. 

+ [test_ssc]() Tests if the non-bootstrapped t-stats calculated in `boottest()` exactly match the t-stats computed via `fixest::feols()` under equivalent small sample corrections (ssc's). These tests are currently only run for `boot_algo = R` as `WildBootTests.jl` currently supports only specific small sample adjustments. 

Other minor tests: 


The second set of tests checks if the different bootstrap algorithm implementations supported via the `boot_algo` function argument lead to the same results. 

+ [test_r_julia]() Tests for objects of type `lm` if `boot_algo = R` and `boot_algo = WildBootTests.jl` produce equivalent results. It tests for approximate equality (with tolerance) for large N and large B cases and for exact equality under enumeration. 

