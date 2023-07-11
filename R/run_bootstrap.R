run_bootstrap <- function(
  object, 
  engine, 
  preprocess, 
  bootstrap_type, 
  B, 
  point_estimate, 
  impose_null, 
  r, 
  sign_level, 
  param, 
  p_val_type, 
  nthreads, 
  type, 
  full_enumeration, 
  small_sample_correction, 
  conf_int, 
  maxiter, 
  tol, 
  clustid, 
  fe, 
  R_long, 
  heteroskedastic, 
  ssc, 
  floattype ,
  bootstrapc ,
  getauxweights ,
  maxmatsize, 
  sampling, 
  # arguments only needed for boot_algo_julia - can be much cleaner
  bootcluster
  ){
  
  #' Run different wild (cluster) bootstrap algorithms
  #' 
  #' Wrapper function to run all different bootstrap algorithms supported 
  #' by fwildclusterboot: `boot_algo_textbook_cpp.R`, `boot_algo_fastnwild.R`,
  #'  `boot_algo_fastnreliable.R`, `boot_algo_julia.R`. 
  #' For more information, see the `Details` section.
  #'
  #' @param object The initial regression object
  #' @param engine Character scalar. Either "R", "R-lean" or "WildBootTests.jl".
  #'  Controls if `boottest()` should run via its native R implementation 
  #'  or `WildBootTests.jl`.
  #'  "R" is the default and implements the cluster bootstrap
  #'  as in Roodman (2019). "WildBootTests.jl" executes the
  #'  wild cluster bootstrap via the WildBootTests.jl
  #'  package. For it to run, Julia and WildBootTests.jl need
  #'  to be installed.
  #'  The "R-lean" algorithm is a memory friendly, but less
  #'  performant rcpp-armadillo based implementation of the wild
  #'  cluster bootstrap.
  #'  Note that if no cluster is provided, boottest() always
  #'  defaults to the "lean" algorithm. You can set the employed
  #'  algorithm globally by using the
  #'  `setBoottest_engine()` function.
  #' @param preprocess A list: output of the preprocess2 function.
  #' @param bootstrap_type Determines which wild cluster bootstrap type should be 
  #' run. Options are "fnw11","11", "13", "31" and "33" for the wild cluster 
  #' bootstrap and "11" and "31" for the heteroskedastic bootstrap.
  #' For more information, see the details section. "fnw11" is the default for 
  #' the cluster bootstrap, which runs a "11" type 
  #' wild cluster bootstrap via the algorithm outlined in "fast and wild" 
  #' (Roodman et al (2019)). "11" is the default for the heteroskedastic 
  #' bootstrap.
  #' @param B number of bootstrap iterations
  #' @param point_estimate The constraints vector R times the estimated coefficients, R x beta
  #' @param impose_null logical scalar. Should the null be imposed on the
  #' bootstrap dgp or not?
  #' @param r Shifts the null hypothesis.
  #' @param sign_level The significance level.
  #' @param param name of the test parameter.
  #' @param p_val_type type Type of p-value. By default "two-tailed".
  #' Other options: "equal-tailed", ">", "<"
  #' @param nthreads The number of threads. Can be: a) an integer lower than,
  #'                 or equal to, the maximum number of threads; b) 0: meaning
  #'                 all available threads will be used; c) a number strictly
  #'                 between 0 and 1 which represents the fraction of all
  #'                 threads to use. The default is to use 50\% of all
  #'                 threads. You can set permanently the number of threads
  #'                 used within this package using the function ...
  #' @param type character or function. The character string specifies the
  #'        type of boostrap to use: One of "rademacher", "mammen", "norm"
  #'        and "webb". Alternatively, type can be a function(n) for drawing
  #'        wild bootstrap factors. "rademacher" by default.
  #' @param full_enumeration Is full enumeration employed? Full enum.
  #' is used if N_G^2 < B for Mammen and Rademacher weights
  #' @param small_sample_correction The small sample correction to be applied.
  #' See ssc().
  #' If FALSE, run wild cluster bootstrap.
  #' @param object the regression object
  #' @param conf_int A logical vector. If TRUE, boottest computes confidence
  #' intervals by test inversion. If FALSE, only the p-value is returned.
  #' @param maxiter Integer. Maximum number of iterations used in the root
  #' finding procedure to find the confidence interval.
  #' @param tol Numeric vector of length 1. The desired accuracy
  #'        (convergence tolerance) used in the root finding procedure to find
  #'         the confidence interval.
  #' @param clustid The name of the cluster variables, as a character vector
  #' @param fe The name of the fixed effect, as a character scalar
  #' @param R_long The (internally) transformed constraints vector
  #' @param heteroskedastic If TRUE, runs the heteroskedastic wild bootstrap. FALSE for 
  #' all wild cluster bootstrap variants
  #' @param ssc An object of class `boot_ssc.type` obtained with the function
  #'  [fwildclusterboot::boot_ssc()]. Represents how the small sample
  #'   adjustments are computed. The defaults are `adj = TRUE, fixef.K = "none",
  #'   cluster.adj = "TRUE", cluster.df = "conventional"`.
  #'             You can find more details in the help file for `boot_ssc()`.
  #'             The function is purposefully designed to mimic fixest's
  #'             [fixest::ssc()] function.
  #' @param floattype Float64 by default. Other option: Float32. Should floating
  #'  point numbers in Julia be represented as 32 or 64 bit? Only relevant when
  #'   'engine = "WildBootTests.jl"'
  #' @param bootstrapc Logical scalar, FALSE by default. TRUE  to request
  #' bootstrap-c instead of bootstrap-t. Only relevant when
  #' 'engine = "WildBootTests.jl"'
  #' @param getauxweights Logical. Whether to save auxilliary weight matrix (v)
  #' @param maxmatsize NULL by default = no limit. Else numeric scalar to set
  #' the maximum size of auxilliary weight matrix (v), in gigabytes. Only
  #' relevant when 'engine = "WildBootTests.jl"'
  #' @param sampling 'dqrng' or 'standard'. If 'dqrng', the 'dqrng' package is
  #' used for random number generation (when available). If 'standard', 
  #' functions from the 'stats' package are used when available. 
  #' This argument is mostly a convenience to control random number generation in 
  #' a wrapper package around `fwildclusterboot`, `wildrwolf`. 
  #' I recommend to use the fast' option. 
  #' @param bootcluster A character vector or rhs formula of length 1. Specifies
  #' the bootstrap clustering variable or variables. If more
  #' than one variable is specified, then bootstrapping is clustered by the
  #' intersections of
  #' clustering implied by the listed variables. To mimic the behavior of
  #' stata's boottest command,
  #' the default is to cluster by the intersection of all the variables
  #' specified via the `clustid` argument,
  #' even though that is not necessarily recommended (see the paper by
  #' Roodman et al cited below, section 4.2).
  #' Other options include "min", where bootstrapping is clustered by
  #' the cluster variable with the fewest clusters.
  #' Further, the subcluster bootstrap (MacKinnon & Webb, 2018) is
  #' supported - see the `vignette("fwildclusterboot", package =
  #' "fwildclusterboot")` for details
  #' 
  #' @return A list of bootstrap results
  #' @noRd
  #' 
  #' @return A list of inputs and outputs of the wild cluster bootstrap
  #' algorithms 
  #' 
  #' @section Different Bootstrap Implementations / Algorithms:
  #' \itemize{
  #' \item `boot_algo_textbook_cpp.R`: Implements the heteroskedastic wild bootstrap
  #' (`WildboottestHC`) as well as the textbook wild cluster bootstrap 
  #' (`WildboottestHC`) in (R)cpp. 
  #' \item `boot_algo_fastnwild.R`: Implements the 'classical' wild cluster bootstrap 
  #' via the "Fast and Wild" algorithm (Roodman et al, 2019).
  #' \item `boot_algo_fastnreliable.R`: Implements the ('classical') as well as 'S', 'C' and 
  #' 'V' wild cluster bootstrap types following algorithms in 
  #' "Fast and Reliable" (MacKinnon et al, 2023).
  #' \item `boot_algo_julia.R`: Wrapper around 'WildBootTests.jl".
  #' }
  #' 
  #' @references Roodman et al., 2019, "Fast and wild: Bootstrap inference in
  #'             STATA using boottest", The STATA Journal.
  #'      (<https://ideas.repec.org/p/qed/wpaper/1406.html>)
  #' @references MacKinnon, James G., Morten Ørregaard Nielsen, and 
  #' Matthew D. Webb. Fast and reliable jackknife and bootstrap
  #' methods for cluster-robust inference. No. 1485. 2022. 
  
  

  
  if (engine == "R") {
    
    if(bootstrap_type == "fnw11"){
      
      res <- boot_algo_fastnwild(
        preprocessed_object = preprocess,
        boot_iter = B,
        point_estimate = point_estimate,
        impose_null = impose_null,
        r = r,
        sign_level = sign_level,
        param = param,
        p_val_type = p_val_type,
        nthreads = nthreads,
        type = type,
        full_enumeration = full_enumeration,
        small_sample_correction = small_sample_correction,
        conf_int = conf_int,
        maxiter = maxiter,
        tol = tol, 
        sampling = sampling
      )
      
      
    } else {
      
      # need some function checks here ... 
      check_boot_algo_fastnreliable(
        weights = stats::weights(object), 
        clustid = clustid,
        bootcluster = bootcluster, 
        fe = fe,
        bootstrap_type = bootstrap_type, 
        R = R_long, 
        r = r
      )
      
      res <- boot_algo_fastnreliable(
        preprocessed_object = preprocess,
        B = B,
        bootstrap_type = bootstrap_type,
        r = r,
        sign_level = sign_level,
        param = param,
        p_val_type = p_val_type,
        nthreads = nthreads,
        type = type,
        full_enumeration = full_enumeration,
        small_sample_correction = small_sample_correction,
        object = object, 
        impose_null = impose_null, 
        sampling = sampling
      )
      
      boot_vcov <- boot_coef <- NULL
      boot_vcov <- res$boot_vcov
      boot_coef <- res$boot_coef
      
      conf_int <- p_grid_vals <- grid_vals <- FALSE
      
      
    }
    
  } else if (engine == "R-lean") {
    
    check_r_lean(
      weights = stats::weights(object),
      clustid = clustid,
      fe = fe, 
      impose_null = impose_null
    )
    
    if(bootstrap_type == "fnw11"){
      bootstrap_type <- "11"
    }
    
    res <- boot_algo_textbook_cpp(
      preprocessed_object = preprocess,
      boot_iter = B,
      point_estimate = point_estimate,
      impose_null = impose_null,
      r = r,
      sign_level = sign_level,
      param = param,
      p_val_type = p_val_type,
      bootstrap_type = bootstrap_type, 
      nthreads = nthreads,
      type = type,
      full_enumeration = full_enumeration,
      small_sample_correction = small_sample_correction,
      heteroskedastic = heteroskedastic
    )
    
    conf_int <- p_grid_vals <- grid_vals <- FALSE
    
  } else if (engine == "WildBootTests.jl") {
    
    julia_ssc <- get_ssc_julia(ssc)
    small <- julia_ssc$small
    clusteradj <- julia_ssc$clusteradj
    clustermin <- julia_ssc$clustermin
    fedfadj <- 0L
    
    if (ssc[["fixef.K"]] != "none") {
      no_fixef.K_warning()
    }
    
    res <- boot_algo_julia(
      preprocess = preprocess,
      impose_null = impose_null,
      r = r,
      B = B,
      bootcluster = bootcluster,
      clustid = clustid,
      sign_level = sign_level,
      conf_int = conf_int,
      tol = tol,
      p_val_type = p_val_type,
      type = type,
      floattype = floattype,
      bootstrapc = bootstrapc,
      # LIML = LIML,
      # ARubin = ARubin,
      getauxweights = getauxweights,
      maxmatsize = maxmatsize,
      # fweights = 1L,
      small = small,
      clusteradj = clusteradj,
      clustermin = clustermin,
      fe = fe,
      fedfadj = fedfadj, 
      bootstrap_type = bootstrap_type 
    )
  } 
  
  res
  
  
  
  
}