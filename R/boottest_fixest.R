#' Fast wild cluster bootstrap inference for object of class fixest
#' 
#' `boottest.fixest` is a S3 method that allows for fast wild cluster 
#' bootstrap inference for objects of class fixest by  implementing
#' the fast wild bootstrap algorithm developed in Roodman et al., 2019 and 
#' implemented in the STATA package `boottest`. 
#' 
#' @param object An object of class fixest and estimated via `fixest::feols()`. Non-linear models are not supported. 
#' @param clustid A character vector containing the names of the cluster variables
#' @param param A character vector. The name of the regression
#'        coefficient(s) for which the hypothesis is to be tested
#' @param B Integer. The number of bootstrap iterations. When the number of clusters is low, 
#'        increasing B adds little additional runtime. 
#' @param bootcluster A character vector. Specifies the bootstrap clustering variable or variables. If more
#'        than one variable is specified, then bootstrapping is clustered by the intersections of
#'        clustering implied by the listed variables. To mimic the behavior of stata's boottest command, 
#'        the default is to cluster by the intersection of all the variables specified via the `clustid` argument, 
#'        even though that is not necessarily recommended (see the paper by Roodman et al cited below, section 4.2). 
#'        Other options include "min", where bootstrapping is clustered by the cluster variable with the fewest clusters.
#' @param fe A character vector of length one which contains the name of the fixed effect to be projected
#'        out in the bootstrap. Note: if regression weights are used, fe 
#'        needs to be NULL.
#' @param sign_level A numeric between 0 and 1 which sets the significance level 
#'        of the inference procedure. E.g. sign_level = 0.05 
#'        returns 0.95% confidence intervals. By default, sign_level = 0.05.
#' @param conf_int A logical vector. If TRUE, boottest computes confidence 
#'        intervals by p-value inversion. If FALSE, only the p-value is returned.
#' @param seed An integer. Allows the user to set a random seed. If you want to set a "global" seed, set it via `dqrng::dqset.seed()`. For Mammen weights, you have to use `set.seed()` instead. 
#' @param R Hypothesis Vector giving linear combinations of coefficients. Must be either NULL or a vector of the same length as `param`. If NULL, a vector of ones of length param.
#' @param r A numeric. Shifts the null hypothesis 
#'        H0: param = r vs H1: param != r
#' @param beta0 Superseded function argument, replaced by function argument 'r'        
#' @param type character or function. The character string specifies the type
#'        of boostrap to use: One of "rademacher", "mammen", "norm"
#'        and "webb". Alternatively, type can be a function(n) for drawing 
#'        wild bootstrap factors. "rademacher" by default.  
#'        For the Rademacher distribution, if the number of replications B exceeds 
#'        the number of possible draw ombinations, 2^(#number of clusters), then `boottest()` 
#'        will use each possible combination once (enumeration).
#' @param impose_null Logical. Controls if the null hypothesis is imposed on
#'        the bootstrap dgp or not. Null imposed `(WCR)` by default.
#'        If FALSE, the null is not imposed `(WCU)`
#' @param p_val_type Character vector of length 1. Type of p-value. 
#'        By default "two-tailed". Other options include "equal-tailed", ">" and "<".
#' @param tol Numeric vector of length 1. The desired accuracy 
#'        (convergence tolerance) used in the root finding procedure to find the confidence interval.
#'        1e-6 by default.
#' @param maxiter Integer. Maximum number of iterations used in the root finding procedure to find the confidence interval.
#'        10 by default.
#' @param na_omit Logical. If TRUE, `boottest()` omits rows with missing 
#'        variables in the cluster variable that have not previously been deleted
#'        when fitting the regression object (e.g. if the cluster variable was not used 
#'        when fitting the regression model).
#' @param nthreads The number of threads. Can be: a) an integer lower than, 
#'                 or equal to, the maximum number of threads; b) 0: meaning 
#'                 all available threads will be used; c) a number strictly
#'                 between 0 and 1 which represents the fraction of all threads 
#'                 to use. The default is to use 1 core.
#' @param ssc An object of class `boot_ssc.type` obtained with the function \code{\link[fwildclusterboot]{boot_ssc}}. Represents how the small sample adjustments are computed. The defaults are `adj = TRUE, fixef.K = "none", cluster.adj = "TRUE", cluster.df = "conventional"`. 
#'             You can find more details in the help file for `boot_ssc()`. The function is purposefully designed to mimic fixest's \code{\link[fixest]{ssc}} function. 
#' @param boot_algo Character scalar. Either "R" or "WildBootTests.jl". Controls the algorithm employed by boottest.
#'                  "R" is the default and implements the cluster bootstrap as in Roodman (2019). "WildBootTests.jl" executes the wild cluster bootstrap by via the WildBootTests.jl
#'                  package. For it to run, Julia and WildBootTests.jl need to be installed. Check out the set_up_ ... functions
#'                  The "fast and wild" algorithm is extremely fast for small number of clusters, but because it is fully vectorized, very memory-demanding.
#'                  For large number of clusters and large number of bootstrap iterations, the fast and wild algorithm becomes infeasible. If a out-of-memory error #
#'                  occurs, the "lean" algorithm is a memory friendly, but less performant rcpp-armadillo based implementation of the wild cluster bootstrap. 
#'                  Note that if no cluster is provided, boottest() always defaults to the "lean" algorithm. Note that you can set the employed algorithm globally by using the 
#'                  `setBoottest_boot_algo()` function.         
#' @param fweights Logical. FALSE by default, TRUE for frequency weights.
#' @param floattype Float64 by default. Other option: Float32. Should floating point numbers in Julia be represented as 32 or 64 bit?
#' @param maxmatsize ... Only relevant when "boot_algo" is set to "WildBootTests.jl".
#' @param bootstrapc ... Only relevant when "boot_algo" is set to "WildBootTests.jl". Runs the boostrap-c as advertised by Young (2019).
#' @param t_boot ... 
#' @param getauxweights ... 
#' @param ... Further arguments passed to or from other methods.
#' @import JuliaConnectoR
#' @importFrom dreamerr check_arg validate_dots

#' @return An object of class \code{boottest}
#' 
#' \item{p_val}{The bootstrap p-value.}
#' \item{conf_int}{The bootstrap confidence interval.}
#' \item{param}{The tested parameter.}
#' \item{N}{Sample size. Might differ from the regression sample size if 
#'      the cluster variables contain NA values.}
#' \item{B}{Number of Bootstrap Iterations.}
#' \item{clustid}{Names of the cluster Variables.}
#' \item{N_G}{Dimension of the cluster variables as used in boottest.}
#' \item{sign_level}{Significance level used in boottest.}
#' \item{type}{Distribution of the bootstrap weights.}
#' \item{impose_null}{Whether the null was imposed on the bootstrap dgp or not.}
#' \item{R}{The vector "R" in the null hypothesis of interest Rbeta = r.}
#' \item{r}{The scalar "r" in the null hypothesis of interest Rbeta = r.}
#' \item{point_estimate}{R'beta. A scalar: the constraints vector times the regression coefficients.}
#' \item{p_test_vals}{All p-values calculated while calculating the confidence
#'      interval.}
#' \item{t_stat}{The 'original' regression test statistics.}
#' \item{test_vals}{All t-statistics calculated while calculating the 
#'       confidence interval.}
#'  \item{t_boot}{All bootstrap t-statistics.}     
#' \item{regression}{The regression object used in boottest.}
#' \item{call}{Function call of boottest.}
#' 
#' @export
#' @method boottest fixest
#' @section Confidence Intervals:
#' \code{boottest} computes confidence intervals by inverting p-values. 
#'       In practice, the following procedure is used:
#' \itemize{
#' \item Based on an initial guess for starting values, calculate p-values 
#'       for 26 equal spaced points between the starting values.
#' \item Out of the 26 calculated p-values, find the two pairs of values x 
#'       for which the corresponding p-values px cross the significance 
#'       sign_level sign_level.
#' \item Feed the two pairs of x into an numerical root finding procedure 
#'       and solve for the root. boottest currently relies on
#'       \code{stats::uniroot} and sets an absolute tolerance of 1e-06 and
#'       stops the procedure after 10 iterations.
#' }
#' @section Standard Errors:
#' \code{boottest} does not calculate standard errors.
#' @references Roodman et al., 2019, "Fast and wild: Bootstrap inference in 
#'             STATA using boottest", The STATA Journal.
#'             (\url{https://journals.sagepub.com/doi/full/10.1177/1536867X19830877})
#' @references Cameron, A. Colin, Jonah B. Gelbach, and Douglas L. Miller. "Bootstrap-based improvements for inference with clustered errors." The Review of Economics and Statistics 90.3 (2008): 414-427.
#' @references MacKinnon, James G., and Matthew D. Webb. "The wild bootstrap for few (treated) clusters." The Econometrics Journal 21.2 (2018): 114-135.
#' @references MacKinnon, James. "Wild cluster bootstrap confidence intervals." L'Actualite economique 91.1-2 (2015): 11-33.             
#' @references Webb, Matthew D. Reworking wild bootstrap based inference for clustered errors. No. 1315. Queen's Economics Department Working Paper, 2013.

#' @examples
#' \dontrun{
#' if(requireNamespace("fixest")){
#' library(fwildclusterboot)
#' library(fixest)
#' data(voters)
#' feols_fit <-feols(proposition_vote ~ treatment + ideology1 + log_income,
#'            fixef =  "Q1_immigration", 
#'            data = voters)
#' boot1 <- boottest(feols_fit, 
#'                   B = 9999, 
#'                   param = "treatment",
#'                   clustid = "group_id1")
#' boot2 <- boottest(feols_fit, 
#'                   B = 9999, 
#'                   param = "treatment", 
#'                   clustid = c("group_id1", "group_id2"))
#' boot3 <- boottest(feols_fit,
#'                   B = 9999,
#'                   param = "treatment", 
#'                   clustid = c("group_id1", "group_id2"),
#'                   fe = "Q1_immigration")
#' boot4 <- boottest(feols_fit, 
#'                   B = 9999, 
#'                   param = "treatment", 
#'                   clustid = c("group_id1", "group_id2"),
#'                   fe = "Q1_immigration", 
#'                   sign_level = 0.2, 
#'                   seed = 8,
#'                   r = 2)
#' # test treatment + ideology1 = 2                   
#' boot5 <- boottest(feols_fit, 
#'                   B = 9999, 
#'                   clustid = c("group_id1", "group_id2"),
#'                   param = c("treatment", "ideology1"),
#'                   R = c(1, 1), 
#'                   r = 2)
#' summary(boot1)
#' plot(boot1)
#' }
#' }

boottest.fixest <- function(object,
                            param,
                            B,
                            clustid = NULL, 
                            bootcluster = "max",
                            fe = NULL,
                            sign_level = 0.05,
                            conf_int = TRUE,
                            seed = NULL,
                            R = NULL,
                            r = 0,
                            beta0 = r, 
                            type = "rademacher",
                            impose_null = TRUE,
                            p_val_type = "two-tailed",
                            tol = 1e-6, 
                            maxiter = 10,
                            na_omit = TRUE,
                            nthreads = getBoottest_nthreads(), 
                            ssc = boot_ssc(adj = TRUE, 
                                           fixef.K = "none", 
                                           cluster.adj = TRUE, 
                                           cluster.df = "conventional"),
                            boot_algo = getBoottest_boot_algo(),
                            floattype = "Float64", 
                            maxmatsize = FALSE, 
                            bootstrapc = FALSE, 
                            t_boot = FALSE, 
                            getauxweights = FALSE,
                            fweights = FALSE,
                            ...) {
  
  
  call <- match.call()
  
  dreamerr::validate_dots(stop = TRUE)
  
  # Step 1: check arguments of feols call
  check_arg(object, "MBT class(fixest)")
  check_arg(clustid, "NULL | character scalar | character vector")
  check_arg(param, "MBT scalar character | character vector")
  check_arg(B, "MBT scalar integer GT{99}")  
  check_arg(sign_level, "scalar numeric GT{0} LT{1}")
  check_arg(type, "charin(rademacher, mammen, norm, gamma, webb)")
  check_arg(p_val_type, 'charin(two-tailed, equal-tailed,>, <)')
  
  check_arg(conf_int, "logical scalar")
  check_arg(seed, "scalar integer | NULL")
  check_arg(R, "NULL| scalar numeric | numeric vector")
  check_arg(r, "numeric scalar | NULL")
  check_arg(beta0, "numeric scalar | NULL")
  check_arg(fe, "character scalar | NULL")
  check_arg(bootcluster, "character vector")
  check_arg(tol, "numeric scalar GT{0}")
  check_arg(maxiter, "scalar integer GT{5}")
  check_arg(boot_ssc, 'class(ssc) | class(boot_ssc)')
  check_arg(boot_algo, "charin(R, R-lean, WildBootTests.jl)")
  
  check_arg(floattype, "charin(Float32, Float64)")
  check_arg(maxmatsize, "scalar integer | NULL")
  check_arg(bootstrapc, "scalar logical")

  if(missing(r) & !missing(beta0)){
    warning("Note that the 'beta0' function argument is superseded by a new argument, 'r'. Please specify your hypothesis via the new function argument instead of using 'beta0'.")
  }
  
  if(boot_algo == "R-lean"){
    if(!is.null(fe)){
      stop("boottest() currently does not support fixed effects with boot_algo = 'R-lean'.")
    }
  }
  
  if(is.null(seed)){
    internal_seed <- get_seed()
  } else {
    set.seed(seed)
    internal_seed <- get_seed()
  }
  
  if(boot_algo == "R"){
    if(type %in% c("rademacher", "webb", "norm")){
      dqrng::dqset.seed(internal_seed)
    } else {
      set.seed(internal_seed)
    }
  } else if(boot_algo == "R-lean"){
    set.seed(internal_seed)
  } else if(boot_algo == "WildBootTests.jl"){
    JuliaConnectoR::juliaEval('using Random')
    #JuliaConnectoR::juliaEval('using StableRNGs')
    #JuliaConnectoR::juliaEval(paste0("rng = StableRNG(",internal_seed,")"))
    rng_char <- paste0("Random.seed!(", internal_seed, ")")
    JuliaConnectoR::juliaEval(rng_char)
    internal_seed <- JuliaConnectoR::juliaEval(paste0("Random.MersenneTwister(", as.integer(internal_seed),")"))
  }
  
  
  
  # fixest specific checks
  if(object$method != "feols"){
    stop("boottest() only supports OLS estimation via fixest::feols() - it does not support non-linear models computed via e.g. fixest::fepois() or fixest::feglm.")
  }
  
  if(!is.null(object$fixef_removed)){
    stop(paste("feols() removes fixed effects with the following values: ", object$fixef_removed, ". Currently, boottest()'s internal pre-processing does not account for this deletion. Therefore, please exclude such fixed effects prior to estimation with feols(). You can find them listed under '$fixef_removed' of your fixest object."))
  }
  
  # -------------------------------------------- 
  
  # check appropriateness of nthreads
  nthreads <- check_set_nthreads(nthreads)

  if(is.null(clustid)){
    heteroskedastic <- TRUE  
    if(boot_algo == "R"){
      # heteroskedastic models should always be run through R-lean
      boot_algo <- "R-lean"
    }
  } else {
    heteroskedastic <- FALSE
  }
  
  
  R <- process_R(R = R, 
                 param = param)
  
  
  if(boot_algo != "WildBootTests.jl"){
    r_algo_checks(R = R, 
                  p_val_type = p_val_type, 
                  conf_int = conf_int,
                  B = B)
  }
 
  check_params_in_model(object = object, param = param)
  
  check_boottest_args_plus(object = object, 
                           R = R, 
                           param = param,
                           sign_level = sign_level, 
                           B = B, 
                           clustid = clustid, 
                           fe = fe)
  
  # preprocess the data: Y, X, weights, fixed_effect
  preprocess <- preprocess(object = object, 
                           cluster = clustid,
                           fe = fe, 
                           param = param,
                           bootcluster = bootcluster, 
                           na_omit = na_omit, 
                           R = R,
                           boot_algo = boot_algo)
  
  enumerate <- 
    check_set_full_enumeration(preprocess = preprocess, 
                               heteroskedastic = heteroskedastic,
                               B = B, 
                               type = type, 
                               boot_algo = boot_algo)
  full_enumeration <- enumerate$full_enumeration
  B <- enumerate$B
  
  N <- preprocess$N
  k <- length(coef(object))
  G <- vapply(preprocess$clustid, function(x) length(unique(x)), numeric(1))
  vcov_sign <- preprocess$vcov_sign
  small_sample_correction <- get_ssc(boot_ssc_object = ssc, N = N, k = k, G = G, vcov_sign = vcov_sign, heteroskedastic = heteroskedastic)
  clustid_dims <- preprocess$clustid_dims
  # R*beta; 
  point_estimate <- as.vector(object$coefficients[param] %*% preprocess$R0[param])
  

  if(boot_algo == "R"){
    res <- boot_algo2(preprocessed_object = preprocess,
                      boot_iter = B,
                      point_estimate = point_estimate,
                      impose_null = impose_null,
                      r = r,
                      sign_level = sign_level,
                      param = param,
                      # seed = seed,
                      p_val_type = p_val_type, 
                      nthreads = nthreads, 
                      type = type, 
                      full_enumeration = full_enumeration, 
                      small_sample_correction = small_sample_correction, 
                      conf_int = conf_int, 
                      maxiter = maxiter, 
                      tol = tol)
  } else if(boot_algo == "R-lean") {
    res <- boot_algo1(preprocessed_object = preprocess,
                      boot_iter = B,
                      point_estimate = point_estimate,
                      impose_null = impose_null,
                      r = r,
                      sign_level = sign_level,
                      param = param,
                      # seed = seed,
                      p_val_type = p_val_type,
                      nthreads = nthreads,
                      type = type,
                      full_enumeration = full_enumeration,
                      small_sample_correction = small_sample_correction, 
                      heteroskedastic = heteroskedastic, 
                      seed = internal_seed)
    conf_int <- p_grid_vals <- grid_vals <- FALSE
  } else if(boot_algo == "WildBootTests.jl"){
    
    fedfadj <- 0L
    
    # translate ssc into small_sample_adjustment
    small_sample_adjustment <- small <- FALSE
    if(ssc[['adj']] == TRUE){
      if(ssc[['cluster.adj']] == TRUE){
        small_sample_adjustment <- small <- TRUE
      }
    } 
    
    if(ssc[['fixef.K']] != "none" || ssc[['cluster.df']] != "conventional"){
      message(paste("Currently, boottest() only supports fixef.K = 'none' and cluster.df = 'conventional' when 'boot_algo = WildBootTests.jl'."))
    }
    
    res <- boot_algo_julia(preprocess = preprocess,
                           impose_null = impose_null,
                           r = r,
                           B = B,
                           bootcluster = bootcluster, 
                           clustid = clustid,
                           sign_level = sign_level,
                           conf_int = conf_int, 
                           tol = tol, 
                           small_sample_adjustment = small_sample_adjustment, 
                           p_val_type = p_val_type, 
                           type = type,
                           floattype = floattype,
                           bootstrapc = bootstrapc, 
                           # LIML = LIML, 
                           # ARubin = ARubin, 
                           getauxweights = getauxweights, 
                           internal_seed = internal_seed, 
                           maxmatsize = maxmatsize, 
                           fweights = 1L, 
                           small = small, 
                           fe = fe, 
                           fedfadj = fedfadj)
  }
    
  # collect results
  res_final <- list(
    point_estimate = point_estimate,
    p_val = res$p_val,
    conf_int = res$conf_int,
    p_grid_vals = res$p_grid_vals,
    grid_vals = res$grid_vals,
    t_stat = res$t_stat,
    t_boot = res$t_boot,
    # regression = res$object,
    param = param,
    N = preprocess$N,
    boot_iter = B,
    clustid = clustid,
    # depvar = depvar,
    N_G = preprocess$N_G,
    sign_level = sign_level,
    call = call,
    type = type,
    impose_null = impose_null,
    R = R,
    r = r, 
    boot_algo = boot_algo, 
    nthreads = nthreads
  )
  

  class(res_final) <- "boottest"
  invisible(res_final)
  
}



#' Fast wild cluster bootstrap inference for object of class feols
#'
#' `mboottest.fixest` is a S3 method that allows for fast wild cluster
#' bootstrap inference of multivariate hypotheses for objects of class feols by
#' implementing the fast wild bootstrap algorithm developed in Roodman et al., 2019.
#'
#' @param object An object of class feols
#' @param clustid A character vector containing the names of the cluster variables
#' @param B Integer. The number of bootstrap iterations. When the number of clusters is low,
#'        increasing B adds little additional runtime.
#' @param bootcluster A character vector. Specifies the bootstrap clustering variable or variables. If more
#'        than one variable is specified, then bootstrapping is clustered by the intersections of
#'        clustering implied by the listed variables. To mimic the behavior of stata's boottest command,
#'        the default is to cluster by the intersection of all the variables specified via the `clustid` argument,
#'        even though that is not necessarily recommended (see the paper by Roodman et al cited below, section 4.2).
#'        Other options include "min", where bootstrapping is clustered by the cluster variable with the fewest clusters.
#' @param fe A character vector of length one which contains the name of the fixed effect to be projected
#'        out in the bootstrap. Note: if regression weights are used, fe 
#'        needs to be NULL.
#' @param seed An integer. Controls the random number generation, which is handled via the `StableRNG()` function from the `StableRNGs` Julia package.
#' @param R Hypothesis Vector or Matrix giving linear combinations of coefficients. Must be either a vector of length k or a matrix of dimension q x k, where q is the number
#'        of joint hypotheses and k the number of estimated coefficients.
#' @param r A vector of length q, where q is the number of tested hypotheses. Shifts the null hypothesis
#'        H0: param = r vs H1: param != r. If not provided, a vector of zeros of length q.
#' @param type character or function. The character string specifies the type
#'        of boostrap to use: One of "rademacher", "mammen", "norm", "gamma"
#'        and "webb". Alternatively, type can be a function(n) for drawing
#'        wild bootstrap factors. "rademacher" by default.
#'        For the Rademacher and Mammen distribution, if the number of replications B exceeds
#'        the number of possible draw ombinations, 2^(#number of clusters), then `boottest()`
#'        will use each possible combination once (enumeration).
#' @param impose_null Logical. Controls if the null hypothesis is imposed on
#'        the bootstrap dgp or not. Null imposed `(WCR)` by default.
#'        If FALSE, the null is not imposed `(WCU)`
#' @param p_val_type Character vector of length 1. Type of p-value.
#'        By default "two-tailed". Other options include "equal-tailed", ">" and "<".
#' @param tol Numeric vector of length 1. The desired accuracy
#'        (convergence tolerance) used in the root finding procedure to find the confidence interval.
#'        Relative tolerance of 1e-6 by default.
#' @param na_omit Logical. If TRUE, `boottest()` omits rows with missing
#'        variables in the cluster variable that have not previously been deleted
#'        when fitting the regression object (e.g. if the cluster variable was not used
#'        when fitting the regression model).
#' @param floattype Float64 by default. Other option: Float32. Should floating point numbers in Julia be represented as 32 or 64 bit?
#' @param fweights Logical. FALSE by default, TRUE for frequency weights.
#' @param getauxweights Logical. FALSE by default. Whether to save auxilliary weight matrix (v)
#' @param t_boot Logical. Should bootstrapped t-statistics be returned?
#' @param maxmatsize NULL by default = no limit. Else numeric scalar to set the maximum size of auxilliary weight matrix (v), in gigabytes
#' @param bootstrapc Logical scalar, FALSE by default. TRUE  to request bootstrap-c instead of bootstrap-t
#' @param ssc An object of class `boot_ssc.type` obtained with the function \code{\link[fwildclusterboot]{boot_ssc}}. Represents how the small sample adjustments are computed. The defaults are `adj = TRUE, fixef.K = "none", cluster.adj = "TRUE", cluster.df = "conventional"`.
#'             You can find more details in the help file for `boot_ssc()`. The function is purposefully designed to mimic fixest's \code{\link[fixest]{ssc}} function.
#' @param ... Further arguments passed to or from other methods.
#'
#' @import JuliaConnectoR
#' @importFrom dreamerr check_arg validate_dots
#' @importFrom stats as.formula coef model.matrix model.response model.weights residuals rlnorm rnorm update
#'
#' @method mboottest fixest
#'
#' @return An object of class \code{mboottest}
#'
#' \item{p_val}{The bootstrap p-value.}
#' \item{conf_int}{The bootstrap confidence interval.}
#' \item{param}{The tested parameter.}
#' \item{N}{Sample size. Might differ from the regression sample size if the
#'          cluster variables contain NA values.}
#' \item{B}{Number of Bootstrap Iterations.}
#' \item{clustid}{Names of the cluster Variables.}
#' \item{N_G}{Dimension of the cluster variables as used in boottest.}
#' \item{type}{Distribution of the bootstrap weights.}
#' \item{t_stat}{The original test statistics - either imposing the null or not - with small sample correction `G / (G-1)`.}
#' \item{test_vals}{All t-statistics calculated while calculating the
#'       confidence interval.}
#'  \item{t_boot}{All bootstrap t-statistics.}
#' \item{regression}{The regression object used in boottest.}
#' \item{call}{Function call of boottest.}
#' \item{getauxweights}{The bootstrap auxiliary weights matrix v. Only returned if getauxweights = TRUE.}
#' \item{t_boot}{The bootstrapped t-statistics. Only returned if t_boot = TRUE.}
#' \item{boot_algo}{The employed bootstrap algorithm.}
#' @export
#'
#' @references Roodman et al., 2019, "Fast and wild: Bootstrap inference in
#'             STATA using boottest", The STATA Journal.
#'             (\url{https://journals.sagepub.com/doi/full/10.1177/1536867X19830877})
#' @references Cameron, A. Colin, Jonah B. Gelbach, and Douglas L. Miller. "Bootstrap-based improvements for inference with clustered errors." The Review of Economics and Statistics 90.3 (2008): 414-427.
#' @references MacKinnon, James G., and Matthew D. Webb. "The wild bootstrap for few (treated) clusters." The Econometrics Journal 21.2 (2018): 114-135.
#' @references MacKinnon, James. "Wild cluster bootstrap confidence intervals." L'Actualite economique 91.1-2 (2015): 11-33.
#' @references Webb, Matthew D. Reworking wild bootstrap based inference for clustered errors. No. 1315. Queen's Economics Department Working Paper, 2013.
#' @examples
#' \dontrun{
#' library(fwildclusterboot)
#' library(clubSandwich)
#' R <- clubSandwich::constrain_zero(2:3, coef(lm_fit))
#' wboottest <- 
#'   mboottest(object = lm_fit, 
#'                clustid = "group_id1", 
#'                B = 999, 
#'                R = R)
#' generics::tidy(wboottest)
#'}

mboottest.fixest <- function(object,
                            clustid,
                            B,
                            R,
                            r = rep(0,nrow(R)),
                            bootcluster = "max",
                            fe = NULL, 
                            seed = NULL,
                            type = "rademacher",
                            impose_null = TRUE,
                            p_val_type = "two-tailed",
                            tol = 1e-6,
                            na_omit = TRUE,
                            floattype = "Float64",
                            #small_sample_adjustment = TRUE,
                            fweights = FALSE,
                            getauxweights = FALSE,
                            t_boot = FALSE,
                            maxmatsize = NULL,
                            bootstrapc = FALSE,
                            ssc = boot_ssc(adj = TRUE,
                                           fixef.K = "none",
                                           cluster.adj = TRUE,
                                           cluster.df = "conventional"),
                            ...) {
  
  call <- match.call()
  
  dreamerr::validate_dots(stop = TRUE)
  
  # Step 1: check arguments of feols call
  check_arg(object, "MBT class(fixest)")
  check_arg(clustid, "MBT character scalar | character vector")
  check_arg(B, "MBT scalar integer")  
  check_arg(R, "MBT numeric vector | numeric matrix")
  
  check_arg(type, "charin(rademacher, mammen, norm, gamma, webb)")
  check_arg(p_val_type, 'charin(two-tailed, equal-tailed,>, <)')
  
  check_arg(seed, "scalar integer | NULL")
  check_arg(r, "numeric vector | NULL")
  check_arg(fe, "character scalar | NULL")
  check_arg(bootcluster, "character vector")
  check_arg(tol, "numeric scalar GT{0}")
  # check_arg(maxiter, "scalar integer")
  check_arg(boot_ssc, 'class(ssc) | class(boot_ssc)')

  check_arg(floattype, "charin(Float32, Float64)")
  check_arg(maxmatsize, "scalar integer | NULL")
  check_arg(bootstrapc, "scalar logical")
  
  if(is.null(seed)){
    internal_seed <- get_seed()
  } else {
    set.seed(seed)
    internal_seed <- get_seed()
  }
  
  # set random seed
  JuliaConnectoR::juliaEval('using Random')
  rng_char <- paste0("Random.seed!(", internal_seed, ")")
  JuliaConnectoR::juliaEval(rng_char)
  internal_seed <- JuliaConnectoR::juliaEval(paste0("Random.MersenneTwister(", as.integer(internal_seed),")"))
  
  # fixest specific checks
  if(object$method != "feols"){
    stop("mboottest() only supports OLS estimation via fixest::feols() - it does not support non-linear models computed via e.g. fixest::fepois() or fixest::feglm.")
  }
  
  if(!is.null(object$fixef_removed)){
    stop(paste("feols() removes fixed effects with the following values: ", object$fixef_removed, ". Currently, boottest()'s internal pre-processing does not account for this deletion. Therefore, please exclude such fixed effects prior to estimation with feols(). You can find them listed under '$fixef_removed' of your fixest object."))
  }

  fedfadj <- 0L
  
  check_mboottest_args_plus(object = object, 
                            R = R, 
                            r = r,
                            B = B)
  
  preprocess <- preprocess(object = object, 
                           cluster = clustid,
                           fe = fe, 
                           param = NULL,
                           bootcluster = bootcluster, 
                           na_omit = na_omit, 
                           R = R,
                           boot_algo = "WildBootTests.jl")
  
  enumerate <- 
    check_set_full_enumeration(preprocess = preprocess, 
                               B = B, 
                               type = type, 
                               boot_algo = "WildBootTests.jl")
  full_enumeration <- enumerate$full_enumeration
  B <- enumerate$B
  
  clustid_dims <- preprocess$clustid_dims

  # number of clusters used in bootstrap - always derived from bootcluster
  N_G_bootcluster <- preprocess$N_G_bootcluster
  N_G_2 <- 2^N_G_bootcluster
  if (type == "rademacher") {
    if(N_G_2 <= B){
      warning(paste("There are only", N_G_2, "unique draws from the rademacher distribution for", N_G_bootcluster, "bootstrap clusters. Therefore, B = ", N_G_2, " with full enumeration. Consider using webb weights instead. Further, note that under full enumeration and with B =", N_G_2, "bootstrap draws, only 2^(#clusters - 1) = ", 2^(N_G_bootcluster - 1), " distinct t-statistics and p-values can be computed. For a more thorough discussion, see Webb `Reworking wild bootstrap based inference for clustered errors` (2013)."),
              call. = FALSE, 
              noBreaks. = TRUE
      )
    }
  }
  
  # translate ssc into small_sample_adjustment
  if(ssc[['adj']] == TRUE && ssc[['cluster.adj']] == TRUE){
    small_sample_adjustment <- small <- TRUE
  } else {
    small_sample_adjustment <- small <- FALSE
  }
  
  if(ssc[['fixef.K']] != "none" || ssc[['cluster.df']] != "conventional"){
    message(paste("Currently, boottest() only supports fixef.K = 'none' and cluster.df = 'conventional' when 'boot_algo = WildBootTests.jl'."))
  }
  
  # send R objects to Julia
  # assign all values needed in WildBootTests.jl
  
  resp <- as.numeric(preprocess$Y)
  predexog <- preprocess$X
  if(is.matrix(preprocess$R)){
    R <- preprocess$R
  } else {
    R <- matrix(preprocess$R, 1, length(preprocess$R))
  }
  r <- r
  reps <- as.integer(B) # WildBootTests.jl demands integer
  
  # Order the columns of `clustid` this way:
  # 1. Variables only used to define bootstrapping clusters, as in the subcluster bootstrap.
  # 2. Variables used to define both bootstrapping and error clusters.
  # 3. Variables only used to define error clusters.
  # In the most common case, `clustid` is a single column of type 2.
  
  if(length(bootcluster == 1) && bootcluster == "max"){
    bootcluster_n <- clustid
  } else if(length(bootcluster == 1) && bootcluster == "min"){
    bootcluster_n <- names(preprocess$N_G[which.min(preprocess$N_G)])
  }
  
  # only bootstrapping cluster: in bootcluster and not in clustid
  c1 <- bootcluster_n[which(!(bootcluster_n %in% clustid))]
  # both bootstrapping and error cluster: all variables in clustid that are also in bootcluster
  c2 <- clustid[which(clustid %in% bootcluster_n)]
  # only error cluster: variables in clustid not in c1, c2
  c3 <- clustid[which(!(clustid %in% c(c1, c2)))]
  all_c <- c(c1, c2, c3)
  #all_c <- lapply(all_c , function(x) ifelse(length(x) == 0, NULL, x))
  
  # note that c("group_id1", NULL) == "group_id1"
  clustid_mat <- data.frame(preprocess$model_frame[, all_c])
  names(clustid_mat) <- all_c
  clustid_df <- base::as.matrix(sapply(clustid_mat, to_integer))
  
  # `nbootclustvar::Integer=1`: number of bootstrap-clustering variables
  # `nerrclustvar::Integer=nbootclustvar`: number of error-clustering variables
  nbootclustvar <- ifelse(bootcluster == "max", length(clustid), length(bootcluster))
  nerrclustvar <- length(clustid)
  
  obswt <-  preprocess$weights
  feid <- as.integer(preprocess$fixed_effect[,1])
  imposenull <- ifelse(is.null(impose_null) || impose_null == TRUE, TRUE, FALSE)
  rtol <- tol
  
  JuliaConnectoR::juliaEval('using WildBootTests')
  WildBootTests <- JuliaConnectoR::juliaImport("WildBootTests")

  ptype <- switch(p_val_type,
                  "two-tailed" = "symmetric",
                  "equal-tailed" = "equaltail",
                  "<" = "lower",
                  ">" = "upper",
                  ptype
  )
  
  auxwttype <- switch(type,
                      "rademacher" = "rademacher",
                      "mammen" = "mammen",
                      "norm" = "normal",
                      "webb" = "webb",
                      "gamma" = "gamma",
                      auxwttype
  )
  
  eval_list <- list(floattype,
                    R,
                    r,
                    resp = resp,
                    predexog = predexog,
                    clustid = clustid_df,
                    nbootclustvar = nbootclustvar,
                    nerrclustvar = nerrclustvar,
                    nbootclustvar = nbootclustvar,
                    nerrclustvar = nerrclustvar,
                    obswt = obswt,
                    imposenull = imposenull,
                    rtol = rtol,
                    small = small,
                    rng = internal_seed,
                    auxwttype = auxwttype,
                    ptype = ptype,
                    reps = reps,
                    fweights = FALSE,
                    bootstrapc = bootstrapc
  )
  
  if(!is.null(fe)){
    eval_list[["feid"]] <- feid
    eval_list[["fedfadj"]] <- fedfadj
  }
  
  if(!is.null(maxmatsize)){
    eval_list[["maxmatsize"]] <- maxmatsize
  }
  
  wildboottest_res <- do.call(WildBootTests$wildboottest, eval_list)
  
  # collect results:
  p_val <- WildBootTests$p(wildboottest_res)
  conf_int <- NA
  t_stat <- WildBootTests$teststat(wildboottest_res)
  t_boot <- FALSE
  
  if(t_boot == TRUE){
    t_boot <- WildBootTests$dist(wildboottest_res)
  }
  
  if(getauxweights == TRUE){
    getauxweights <- WildBootTests$auxweights(wildboottest_res)
  }
  
  plotpoints <- WildBootTests$plotpoints(wildboottest_res)
  plotpoints <- cbind(plotpoints$X[[1]], plotpoints$p)
  
  res_final <- list(
    # point_estimate = point_estimate,
    p_val = p_val,
    conf_int = conf_int,
    # p_test_vals = res_p_val$p_grid_vals,
    # test_vals = res_p_val$grid_vals,
    t_stat = t_stat,
    t_boot = t_boot,
    auxweights = getauxweights,
    #regression = res$object,
    # param = param,
    N = preprocess$N,
    B = B,
    clustid = clustid,
    # depvar = depvar,
    N_G = preprocess$N_G,
    call = call,
    type = type,
    impose_null = impose_null,
    R = R,
    r = r,
    plotpoints = plotpoints
  )
  
  class(res_final) <- "mboottest"
  
  invisible(res_final)
}
