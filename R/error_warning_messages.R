cis_only_for_fnw11 <- function(){
  message(
    paste(
      "Note: Confidence Intervals are currently only supported for", 
      "the R engine with 'bootstrap_type = 'fnw11' '."
    )
  )
}

arg_beta0_is_deprecated_error <- function(){

  stop(
    "The function argument 'beta0' is deprecated.", 
    "Please use the function argument 'r' instead, ", 
    "by which it is replaced."
  )
   
}

please_remove_fixest_na_error <- function(object){
  
  stop(
    paste(
      "feols() removes fixed effects with the following values: ",
      object$fixef_removed,
      ". Currently, boottest()'s internal pre-processing does not",
      "account for this deletion. Therefore, please exclude such fixed",
      "effects prior to estimation with feols(). You can find them listed",
      "under '$fixef_removed' of your fixest object."
    )
  )
  
}

no_plot_when_no_ci_error <- function(){
  stop(
    "No plot method if boottest()'s function argument 'conf_int = FALSE'."
  )
}

only_ols_for_fixest_error <- function(){
  stop(
    "boottest() only supports OLS estimation via fixest::feols() - it ", 
    "does not support non-linear models computed via e.g. fixest::fepois() ", 
    "or fixest::feglm."
  )
}

no_weights_when_fe_error <- function(){
  stop(
    "boottest() unfortunately currently does not support WLS and fixed", 
    "effects. Please set fe = NULL to run a bootstrap with WLS."
  )
}

no_fixef_for_fast_reliable_error <- function(){
  
  stop(
    "No fixed effects are supported for bootstrap_types ", 
    "'11', '13', '31', '33'."
  )
  
}

bootcluster_neither_cluster_nor_params <- function(){
  stop(
    "A bootcluster variable is neither contained in the cluster ", 
    "variables nor in the model coefficients."
  )
}

cluster_nobs_error <- function(){
  stop(
    "The number of observations in 'cluster' and 'nobs()' do not match."
  )
}

bootcluster_nobs_error <- function(){
  stop(
    "The number of observations in 'bootcluster' and 'nobs()' do not match."
  )
}

nas_in_clustid_error <- function(){
  stop(
    "`boottest()` cannot handle NAs in `clustid` variables that are not ", 
    "part of the estimated model object."
  )
}

nas_in_bootclustid_error <- function(){
  stop(
    "`boottest()` cannot handle NAs in `bootcluster` variables that are ", 
    "not part of the estimated model object."
  )
}

boottest_engine_error <- function(){
  stop(
    "The value of `getOption(\"boottest_engine\")` is currently not legal. ",
    "Please use function `setBoottest_engine()` to set it to an appropriate ", 
    "value."
  )
}

boottest_nthreads_error <- function(){
  stop(
      "The value of `getOption(\"boottest_nthreads\")` is currently",
      "not legal. ",
      "Please use function `setBoottest_nthreads` to set it to an",
      "appropriate value."
  )
}

# error messages 
advanced_formula_error <- function(){
  stop(
    "Advanced formula notation in fixest / fixest via ^ to interact ",
    "fixed effects is currently not supported in boottest().", 
    call = TRUE
  )
}

varying_slopes_error <- function(){
  stop(
    "Varying slopes in fixest / fixest via [] to interact ", 
    "fixed effects is currently not supported in boottest()."
  )
}

fixef_not_fixef_error <- function(fe){
  
  stop(
    paste(
      "The fixed effect to be projected out in the bootstrap,",fe,
      "is not included as a dedicated fixed effect in the", 
      "estimated model."
    )
  )
  
}

fe_in_test_error <- function(fe){
  
  stop(
    paste(
      "The function argument fe =", fe, "is included in the",
      "hypothesis (via the `param` argument). This is not allowed.",
      "Please set fe to another factor variable or NULL."
    )
  )
  
  
}

B_too_small_error <- function(){
  stop(
    "The function argument B is smaller than 100. The number of bootstrap",
    "iterations needs to be 100 or higher in order to guarantee that the", 
    "root finding procudure used to find the confidence set",
    "works properly."
  )
}

test_dimension_error <- function(R, r){
  stop(
    paste(
      "The dimensions of func args R and r do not match. The number",
      "of rows of R is ", nrow(R), ", but the length of r is",
      length(r), "."
    )
  )
}

non_scalar_test_error <- function(){
  
  stop(
    "Only bootstrap_type = 'fnw11' is currently supported with ", 
    "hypotheses that contain more than one parameter. This feature ", 
    "will be added in the near future. If you receive this error message ", 
    "please complain on github, it will motivate Alex to implement this ", 
    "feature."
  )
  
}

only_scalar_test_rlean_error <- function(){
  stop(
    "The 'lean' bootstrap algorithm - which runs the ",
    "heteroskedastic wild bootstrap - is currently not supported for ",
    "hypotheses about more than one parameter."
  )
}

no_clustering_for_rlean_error <- function(){
  
  stop(
    "The R-lean algorithm currently only supports oneway clustering."
  )
  
}

no_fixef_for_rlean_error <- function(){
  
  stop(
    "boottest() currently does not support ", 
    "fixed effects with engine = 'R-lean'."
  )
  
}

no_weights_for_rlean_error <- function(){
  stop(
    "boottest() currently does not support regression ",
    "weights with engine = 'R-lean'."
  )
}

no_wcu_for_rlean_error <- function(){
  
  stop(
    "boottest() currently does not support the 'WCU' bootstrap ",
    "- which does not impose the null on the ", 
    "bootstrap dgp - for engine = 'R-lean'."
  )
  
}

only_onweway_clustering_for_fast_reliable <- function(){
  
  stop(
    "The '13', '31', and '33' bootstrap variants currently ", 
    "only support oneway clustering when 'boot_engine' == 'R'."
  )
  
}

no_subcluster_bootstrap_fast_reliable <- function(){
  stop(
    paste(
      "The subcluster bootstrap is currently not supported", 
      "for the bootstrap types '11', '31', '13' and '33'."
    )
  )
}

bootcluster_not_max_min_clustid <- function(clustid){
  stop(
    paste0(
      "For the 'fast and reliable' implementations, ", 
      "the 'bootcluster' argument must be 'max', 'min' or '", 
      clustid, "'."
    )
  )
}

full_enumeration_warning <- function(N_G_2, N_G_bootcluster){
  message(
    paste(
      "Note: ",
      "There are only",
      N_G_2,
      "unique draws from the rademacher distribution for",
      N_G_bootcluster,
      "bootstrap clusters. Therefore, B = ",
      N_G_2,
      " with full enumeration. Consider using webb weights instead.",
      "Further, note that under full enumeration and with B =",
      N_G_2,
      "bootstrap draws, only 2^(#clusters - 1) = ",
      2^(N_G_bootcluster - 1),
      " distinct t-statistics and p-values can be computed. For a",
      "more thorough discussion, see Webb `Reworking wild bootstrap",
      "based inference for clustered errors` (2013)."
    )
  )
  
}

multi_hypothesis_only_via_julia <- function(){
  stop(
    "Hypotheses with q>1 are currently only supported via WildBootTests.jl. ", 
    "Please set the function argument 'engine = WildBootTests.jl'."
  )
}

one_sided_cis_only_via_julia <- function(){
  message(
    paste(
      "Note:", 
      "Currently, boottest() calculates confidence intervals for one-sided", 
      "hypotheses only for `engine = 'WildBootTests.jl'`."
    )
  )
  
}

R_must_be_NULL_or_param <- function(){
  stop(
    "The constraints vector must either be NULL or a numeric of", 
    "the same length as the `param` input vector."
  )
}

no_13_33_bootstap_via_julia <- function(bootstrap_type){
  stop(
    paste(
      "The bootstrap of type",
      bootstrap_type,
      "is not yet supported via 'WildBootTests. You can run it via", 
      "the 'R' engine instead.'. But note that this algos are still", 
      "written in pure R, and therefore rather slow.")
  )
}

param_not_in_model_error <- function(param){
  stop(
    paste(
      "The parameter", param, "is not included in the estimated",
      "model. Maybe you are trying to test for an interaction",
      " parameter? To see all model parameter names,",
      "run names(coef(model))."
    )
  )
}

only_2SLS_for_IV_error <- function(){
  stop(
    "Currently, only 2SLS is supported. Please set the `ivreg`",
    "function argument `method` to `OLS`."
  )
}

only_cluster_fixef_for_11_31_error <- function(){
  stop(
    "The '11' and '31' bootstrap variants currently only support ",
    "cluster fixed effects in the bootstrap. Please set 'fe = NULL' or to ", 
    "the cluster variable."
  )
  
}

no_fixef_for_13_33_error <- function(){
  stop(
    "The '13' and '33' bootstrap variants currently do support ", 
    "fixed effects in the bootstrap. Please set 'fe = NULL'."
  )
}

no_weights_for_fast_reliable_error <- function(){
  stop(
    "The '11' and '13' ", 
    "bootstrap variants currently only support weights ", 
    "for engine = 'WildBootTests.jl' The '13' and '33' do not support ",
    "weights for both engines."
  )
}

no_bootstrap_type_for_rlean_error <- function(bootstrap_type){
  stop(
    paste(
      "The bootstrap of type",
      bootstrap_type,
      "is not yet supported via 'R-lean'. You can potentially run it via ", 
      "the 'R' or 'WildBootTests.jl' engines instead.'"
    )
  )
  
}

only_webb_rademacher_for_rlean_error <- function(){
  stop(
    "For the 'lean' bootstrap algorithm, only webb and rademacher ", 
    "weights are supported."
  )
}

no_13_33_for_felm_error <- function(){
  stop(
    "The bootstrap types '13' and '33' are not yet supported for objects ",
    "of type felm. Support will be added in the future. Until then, you ",
    "can run the '13' and '33' bootstrap types with objects of type 'lm' ",
    "and 'fixest'."
  )
}

ci_root_finding_initiation_failed <- function(){
  stop(
    "The inflation factor for initial guesses for standard errors was not ",
    "large enough. In consequence, the root-finding procedure to compute ",
    "confidence intervals via p-value inversion could not be initiated. ",
    "In a future release, it will be possible to specify a costum inflation ",
    "factor as a function argument to boottest(). Until then, you can still ",
    "use boottest() to calculate p-values by setting the boottest() function ",
    "argument conf_int to FALSE."
  )
}

no_iv_for_felm_error <- function(){
  
  stop(
    "IV regression is currently not supported by boottest() for ",
    "objects of type 'felm'. Please use ",
    "or 'ivreg::ivreg' for IV-regression."
  )
  
}

no_fixef.K_warning <- function(){
  message(
    paste(
      "Note: Currently, boottest() only supports fixef.K = 'none'."
    )
  )
}
