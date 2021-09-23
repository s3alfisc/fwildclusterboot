All unit tests can be found in https://github.com/s3alfisc/fwildclusterboot/tree/master/inst/tinytest. 

*internal consistency*:

These tests check if `boottest` produces equivalent results for equivalent regression objects of types `lm`, `fixest` and `felm`. Their main goal is to check that the internal pre-processing within `boottest()` is aligned. 

- [x] https://github.com/s3alfisc/fwildclusterboot/blob/master/inst/tinytest/test_bootcluster.R: tests if the `boottest.felm`, `boottest.fixest` & `boottest.lm` methods produce equivalent results for different bootstrap clusters 
- [x] https://github.com/s3alfisc/fwildclusterboot/blob/master/inst/tinytest/test_crosstab.R: tests that different ways to calculate crosstabs produce equivalent results
- [x] https://github.com/s3alfisc/fwildclusterboot/blob/master/inst/tinytest/test_deterministic_alpha_beta0.R:  tests if the `boottest.felm`, `boottest.fixest` & `boottest.lm` methods produce equivalent results for different significance levels alpha and null hypotheses shifted via beta
- [x] https://github.com/s3alfisc/fwildclusterboot/blob/master/inst/tinytest/test_deterministic_default.R: tests if the `boottest.felm`, `boottest.fixest` & `boottest.lm` methods produce equivalent results for different numbers of fixed effects, clustering
- [x] https://github.com/s3alfisc/fwildclusterboot/blob/master/inst/tinytest/test_deterministic_weights.R: tests if the `boottest.felm`, `boottest.fixest` & `boottest.lm` methods produce equivalent results for weighted least squares
- [x] https://github.com/s3alfisc/fwildclusterboot/blob/master/inst/tinytest/test_impose_null.R: tests if the `boottest.felm`, `boottest.fixest` & `boottest.lm` methods produce equivalent results when the null hypothesis is not imposed (WCU)
- [x] https://github.com/s3alfisc/fwildclusterboot/blob/master/inst/tinytest/test_nthreads.R: tests if the `boottest.felm`, `boottest.fixest` & `boottest.lm` methods produce equivalent results when different numbers of cores are used
- [x] https://github.com/s3alfisc/fwildclusterboot/blob/master/inst/tinytest/test_numeric_fe_clusters.R: tests if the `boottest.felm`, `boottest.fixest` & `boottest.lm` methods produce equivalent results when fixed effects or clustering variables are either factors or numerical variables in the original data
- [x] https://github.com/s3alfisc/fwildclusterboot/blob/master/inst/tinytest/test_p_val_type.R: test if the `boottest.felm`, `boottest.fixest` & `boottest.lm` methods produce equivalent results for different types of p-values
- [x] https://github.com/s3alfisc/fwildclusterboot/blob/master/inst/tinytest/test_seed.R
- [x] https://github.com/s3alfisc/fwildclusterboot/blob/master/inst/tinytest/test_small_n_cluster.R
- [x] https://github.com/s3alfisc/fwildclusterboot/blob/master/inst/tinytest/test_tidy.R
- [x] https://github.com/s3alfisc/fwildclusterboot/blob/master/inst/tinytest/test_tol_maxiter.R
- [x] https://github.com/s3alfisc/fwildclusterboot/blob/master/inst/tinytest/test_type.R

*external consistency*:
This suite of tests compares results from `fwildclusterboot::boottest()` with `stata::boottest`, `clusterSEs` & `fixest`.
- [x] https://github.com/s3alfisc/fwildclusterboot/blob/master/inst/tinytest/test_clusterSEs.R tests if results from `boottest()` align with the wild cluster bootstrap implementation in the`clusterSEs` package
- [x] https://github.com/s3alfisc/fwildclusterboot/blob/master/inst/tinytest/test_small_sample_correction_tstat.R: `boottest()` internally recomputes the regression t-statistic, using the small sample correction G / (G-1). This test checks if the t-statistics produced by `boottest()` are the same as those produced by `fixest::feols()` using the same small sample correction
- [x] https://github.com/s3alfisc/fwildclusterboot/blob/master/inst/tinytest/test_stata.R. This tests checks if `fwildclusterboot::boottest()` produces equivalent results as `stata::boottest` for large B and large N. Note that for rademacher and mammen weights, if full enumeration is used, the resulting p-values from stata and R should be *almost* exactly identical because there is no sampling uncertainty, `r::boottest` tries to mimic (almost) all design choices of `stata::boottest`. All remaining differences should be *numerical errors*.

*expected errors & warnings*: 
Test if `boottest()` thows errors and warnings when expected. 
- [x] https://github.com/s3alfisc/fwildclusterboot/blob/master/inst/tinytest/test_error_warning.R test if the `boottest.felm`, `boottest.fixest` & `boottest.lm` methods produce errors and warnings when expected