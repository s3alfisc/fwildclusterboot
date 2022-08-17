# Tests 

## Main Tests

- `test_error_warning.R` tests that messages and warnings occur when desired
- `test_method_equivalence.R` tests that `boottest()` returns equivalent 
   results when applied to differently specified objects of type `lm`, `feols` 
   and `fixest`
- `test-r-vs-julia.R` tests `fwildclusterboot` against `WildBootTests.jl`
- `test_tstat-equivalence.R` tests equivalence of t-statistics of `boottest()` 
   against fixest
- `test-r-lean.R` tests the 'lean' implementation of the wild cluster bootstrap 
   for correctness
- `test-seed.R` tests how seed behavior affects `boottest()` output
- `test-new-bootstrap-variants` tests the WCR11 and WCU11 wild cluster bootstrap implementations of `boot_algo3()` (from "fast and reliable") against those of `boot_algo2()` (from "fast and wild")



## Other Tests

- `test_fixest_sugar.R` tests if `boottest()` works properly when fixest formula 
   sugar is used
- `test_multicollinearity.R` tests that the `model_matrix()` methods employed in 
   `boottest.R` drop multicollinear variables
- `test_tidy.R` tests the `tidy()`, `summary()` and `plot` methods
- `test_crosstab.R` tests the `crosstab` function used in `boottest()`
- `test_global_vars` tests all 'global variables' functionality, e.g. setting of 
   seeds and number of threads to use with `boottest()`
- `test_to_integer.R` tests the internally used `to_integer` function