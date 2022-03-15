#' Fast wild cluster bootstrap inference for object of class lm
#'
#' `boottest.ivreg` is a S3 method that allows for fast wild cluster
#' bootstrap inference for objects of class ivreg by  implementing
#' the fast wild bootstrap algorithm developed in Roodman et al., 2019
#' for instrumental variable models (WRE, Davidson & McKinnon, 2010)
#'
#' @param object An object of class lm
#' @param clustid A character vector containing the names of the cluster variables
#' @param param A character vector of length one. The name of the regression
#'        coefficient for which the hypothesis is to be tested
#' @param B Integer. The number of bootstrap iterations. When the number of clusters is low,
#'        increasing B adds little additional runtime.
#' @param bootcluster A character vector. Specifies the bootstrap clustering variable or variables. If more
#'        than one variable is specified, then bootstrapping is clustered by the intersections of
#'        clustering implied by the listed variables. To mimic the behavior of stata's boottest command,
#'        the default is to cluster by the intersection of all the variables specified via the `clustid` argument,
#'        even though that is not necessarily recommended (see the paper by Roodman et al cited below, section 4.2).
#'        Other options include "min", where bootstrapping is clustered by the cluster variable with the fewest clusters.
#' @param sign_level A numeric between 0 and 1 which sets the significance level
#'        of the inference procedure. E.g. sign_level = 0.05
#'        returns 0.95% confidence intervals. By default, sign_level = 0.05.
#' @param conf_int A logical vector. If TRUE, boottest computes confidence
#'        intervals by p-value inversion. If FALSE, only the p-value is returned.
#' @param seed An integer. Controls the random number generation, which is handled via the `StableRNG()` function from the `StableRNGs` Julia package.
#' @param R Hypothesis Vector giving linear combinations of coefficients. Must be either NULL or a vector of the same length as `param`. If NULL, a vector of ones of length param.
#' @param beta0 A numeric. Shifts the null hypothesis
#'        H0: param = beta0 vs H1: param != beta0
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
#' @param LIML Logical scalar. False by default. TRUE for LIML or Fuller LIML
#' @param Fuller NULL by default. Numeric scalar. Fuller LIML factor
#' @param kappa Null by default. fixed κ for k-class estimation
#' @param ARubin False by default. Logical scalar. TRUE for Anderson-Rubin Test.
#' @param ssc An object of class `boot_ssc.type` obtained with the function \code{\link[fwildclusterboot]{boot_ssc}}. Represents how the small sample adjustments are computed. The defaults are `adj = TRUE, fixef.K = "none", cluster.adj = "TRUE", cluster.df = "conventional"`.
#'             You can find more details in the help file for `boot_ssc()`. The function is purposefully designed to mimic fixest's \code{\link[fixest]{ssc}} function.
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom dreamerr check_arg validate_dots
#'
#' @method boottest ivreg
#'
#' @return An object of class \code{boottest}
#'
#' \item{p_val}{The bootstrap p-value.}
#' \item{conf_int}{The bootstrap confidence interval.}
#' \item{param}{The tested parameter.}
#' \item{N}{Sample size. Might differ from the regression sample size if the
#'          cluster variables contain NA values.}
#' \item{B}{Number of Bootstrap Iterations.}
#' \item{clustid}{Names of the cluster Variables.}
#' \item{N_G}{Dimension of the cluster variables as used in boottest.}
#' \item{sign_level}{Significance level used in boottest.}
#' \item{type}{Distribution of the bootstrap weights.}
#' \item{t_stat}{The original test statistics - either imposing the null or not - with small sample correction `G / (G-1)`.}
#' \item{test_vals}{All t-statistics calculated while calculating the
#'       confidence interval.}
#'  \item{t_boot}{All bootstrap t-statistics.}
#' \item{regression}{The regression object used in boottest.}
#' \item{call}{Function call of boottest.}
#' \item{getauxweights}{The bootstrap auxiliary weights matrix v. Only returned if getauxweights = TRUE.}
#' \item{t_boot}{The bootstrapped t-statistics. Only returned if t_boot = TRUE.}
#'
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
#' library(ivreg)
#' library(fwildclusterboot)
#' 
#' # drop all NA values from SchoolingReturns
#' SchoolingReturns <- SchoolingReturns[rowMeans(sapply(SchoolingReturns, is.na)) == 0,]
#' ivreg_fit <- ivreg(log(wage) ~ education + age + 
#'                                ethnicity + smsa + south + parents14 |
#'                                nearcollege + age  + ethnicity + smsa 
#'                                + south + parents14, 
#'                                data = SchoolingReturns)
#' 
#' boot_ivreg <- boottest(object = ivreg_fit,
#'                        B = 999,
#'                        param = "education",
#'                        clustid = "kww",
#'                        type = "mammen",
#'                        impose_null = TRUE)
#' summary(boot_ivreg)
#' 
#'}


boottest.ivreg <- function(object,
                           clustid,
                           param,
                           B,
                           bootcluster = "max",
                           conf_int = TRUE,
                           seed = NULL,
                           R = NULL,
                           beta0 = 0,
                           sign_level = 0.05,
                           type = "rademacher",
                           impose_null = TRUE,
                           p_val_type = "two-tailed",
                           tol = 1e-6,
                           na_omit = TRUE,
                           floattype = "Float64",
                           fweights = FALSE,
                           getauxweights = FALSE,
                           t_boot = FALSE,
                           maxmatsize = NULL,
                           bootstrapc = FALSE,
                           LIML = FALSE,
                           Fuller = NULL,
                           kappa = NULL,
                           ARubin = FALSE,
                           ssc = boot_ssc(adj = TRUE,
                                          fixef.K = "none",
                                          cluster.adj = TRUE,
                                          cluster.df = "conventional"),
                           ...){
  
  # check inputs
  call <- match.call()
  dreamerr::validate_dots(stop = TRUE)
  
  check_arg(object, "MBT class(ivreg)")
  check_arg(clustid, "MBT character scalar | character vector")
  check_arg(param, "MBT scalar character | character vector")
  check_arg(B, "MBT scalar integer GT{0}")
  check_arg(sign_level, "scalar numeric GT{0} LT{1}")
  check_arg(type, "charin(rademacher, mammen, norm, gamma, webb)")
  check_arg(conf_int, "logical scalar ")
  check_arg(seed, "scalar integer | NULL")
  check_arg(R, "NULL| scalar numeric | numeric vector")
  check_arg(beta0, "numeric scalar | NULL")
  check_arg(bootcluster, "character vector")
  check_arg(tol, "numeric scalar GT{0}")
  check_arg(floattype, "charin(Float32, Float64)")
  check_arg(fweights, "scalar logical")
  check_arg(t_boot, "scalar logical")
  check_arg(getauxweights, "scalar logical")
  check_arg(maxmatsize, "scalar integer | NULL")
  check_arg(bootstrapc, "scalar logical")
  # IV specific arguments
  check_arg(LIML, "scalar logical")
  check_arg(Fuller, "NULL | scalar numeric")
  check_arg(kappa, "NULL | scalar numeric")
  check_arg(ARubin, "scalar logical")
  check_arg(p_val_type, 'charin(two-tailed, equal-tailed,>, <)')
  check_arg(boot_ssc, 'class(ssc) | class(boot_ssc)')
  
  # translate ssc into small_sample_adjustment
  if(ssc[['adj']] == TRUE && ssc[['cluster.adj']] == TRUE){
    small_sample_adjustment <- TRUE
  } else {
    small_sample_adjustment <- FALSE
  }
  
  if(ssc[['fixef.K']] != "none" || ssc[['cluster.df']] != "conventional"){
    message(paste("Currently, boottest() only supports fixef.K = 'none' and cluster.df = 'conventional'."))
  }
  
  if ((conf_int == TRUE || is.null(conf_int)) & B <= 100) {
    stop("The function argument B is smaller than 100. The number of bootstrap
          iterations needs to be 100 or higher in order to guarantee that the
          root finding procudure used to find the confidence set
          works properly.",
         call. = FALSE
    )
  }
  
  
  # which parametrs can be tested?
  if (mean(param %in% names(c(object$exogenous, object$endogenous)) != 1)) {
    stop(paste("The parameter", param, "is not included in the estimated model.
               Maybe you are trying to test for an interaction parameter?
               To see all model parameter names, run names(coef(model))."))
  }
  
  if(is.null(R)){
    R <- rep(1, length(param))
  } else {
    if(length(R) != length(param)){
      stop("The constraints vector must either be NULL or a numeric of the same length as the `param` input vector.")
    }
  }
  
  if (((1 - sign_level) * (B + 1)) %% 1 != 0) {
    message(paste("Note: The bootstrap usually performs best when the
                  confidence level (here,", 1 - sign_level, "%)
                  times the number of replications plus 1
                  (", B, "+ 1 = ", B + 1, ") is an integer."))
  }
  
  if(object$method != "OLS"){
    stop("Currently, only 2SLS is supported. Please set the function argument method to `OLS`.")
  }
  
  # throw error if specific function arguments are used in lm() call
  call_object <- names(object$call)[names(object$call) != ""]
  banned_fun_args <- c("contrasts", "subset", "offset", "instruments")
  if (sum(call_object %in% banned_fun_args) > 0) {
    stop(paste(
      "boottest.ivreg currently does not accept objects of type lm with
      function arguments",
      paste0(banned_fun_args[1:(length(banned_fun_args) - 1)], collapse = ", "),
      "and", banned_fun_args[length(banned_fun_args)], "."
    ),
    call. = FALSE
    )
  }
  
  # preprocess data: X, Y, weights, fixed effects
  preprocess <- preprocess_julia(object = object,
                                 cluster = clustid,
                                 fe = NULL,
                                 param = param,
                                 bootcluster = bootcluster,
                                 na_omit = na_omit,
                                 R = R)
  
  clustid_dims <- preprocess$clustid_dims
  point_estimate <- as.vector(object$coefficients[param] %*% preprocess$R0[param])
  
  clustid_fml <- as.formula(paste("~", paste(clustid, collapse = "+")))
  
  # number of clusters used in bootstrap - always derived from bootcluster
  N_G_bootcluster <- preprocess$N_G_bootcluster
  N_G_2 <- 2^N_G_bootcluster
  if (type == "rademacher") {
    if(N_G_2 <= B){
      warning(paste("There are only", N_G_2, "unique draws from the rademacher distribution for", N_G_bootcluster, "bootstrap clusters. Therefore, B = ", N_G_2, " with full enumeration. Consider using webb weights instead."),
              call. = FALSE, 
              noBreaks. = TRUE
      )
      warning(paste("Further, note that under full enumeration and with B =", N_G_2, "bootstrap draws, only 2^(#clusters - 1) = ", 2^(N_G_bootcluster - 1), " distinct t-statistics and p-values can be computed. For a more thorough discussion, see Webb `Reworking wild bootstrap based inference for clustered errors` (2013)."),
              call. = FALSE, 
              noBreaks. = TRUE
      )
    }
  }
    
  # assign all values needed in WildBootTests.jl
  
  resp <- as.numeric(preprocess$Y)
  predexog <- preprocess$X_exog
  predendog <- preprocess$X_endog
  inst <- preprocess$instruments
  if(is.matrix(preprocess$R)){
    R <- preprocess$R
  } else {
    R <- matrix(preprocess$R, 1, length(preprocess$R))
  }
  r <- beta0
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
  } else {
    bootcluster_n <- bootcluster
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
  level <-  1 - sign_level
  getCI <- ifelse(is.null(conf_int) || conf_int == TRUE, TRUE, FALSE)
  imposenull <- ifelse(is.null(impose_null) || impose_null == TRUE, TRUE, FALSE)
  rtol <- tol
  small <- small_sample_adjustment
  
  JuliaConnectoR::juliaEval('using WildBootTests')
  JuliaConnectoR::juliaEval('using Random')
  
  WildBootTests <- JuliaConnectoR::juliaImport("WildBootTests")
  rng <- juliaEval(paste0("Random.MersenneTwister(", seed, ")"))
  
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
                    predendog = predendog,
                    inst = inst,
                    clustid = clustid_df,
                    nbootclustvar = nbootclustvar,
                    nerrclustvar = nerrclustvar,
                    obswt = obswt,
                    level = level,
                    getCI = getCI,
                    imposenull = imposenull,
                    rtol = rtol,
                    small = small,
                    rng = rng,
                    auxwttype = auxwttype,
                    ptype = ptype,
                    reps = reps,
                    fweights = fweights,
                    bootstrapc = bootstrapc,
                    LIML = LIML,
                    ARubin = ARubin
                    
  )
  
  if(!is.null(maxmatsize)){
    eval_list[["maxmatsize"]] <- maxmatsize
  }
  
  if(!is.null(Fuller)){
    eval_list[["Fuller"]] <- Fuller
  }
  
  if(!is.null(kappa)){
    eval_list[["kappa"]] <- kappa
  }
  
  wildboottest_res <- do.call(WildBootTests$wildboottest, eval_list)
  
  
  # collect results:
  p_val <- WildBootTests$p(wildboottest_res)
  if(getCI == TRUE){
    conf_int <- WildBootTests$CI(wildboottest_res)
  } else{
    conf_int <- NA
  }
  t_stat <- WildBootTests$teststat(wildboottest_res)
  if(t_boot == TRUE){
    t_boot <- WildBootTests$dist(wildboottest_res)
  }
  
  if(getauxweights == TRUE){
    getauxweights <- WildBootTests$auxweights(wildboottest_res)
  }
  
  plotpoints <- WildBootTests$plotpoints(wildboottest_res)
  plotpoints <- cbind(plotpoints$X[[1]], plotpoints$p)
  
  
  res_final <- list(
    point_estimate = point_estimate,
    p_val = p_val,
    conf_int = conf_int,
    # p_test_vals = res_p_val$p_grid_vals,
    # test_vals = res_p_val$grid_vals,
    t_stat = t_stat,
    t_boot = t_boot,
    auxweights = getauxweights,
    #regression = res$object,
    param = param,
    N = preprocess$N,
    B = B,
    clustid = clustid,
    # depvar = depvar,
    N_G = preprocess$N_G,
    sign_level = sign_level,
    call = call,
    type = type,
    impose_null = impose_null,
    R = R,
    beta0 = beta0,
    plotpoints = plotpoints, 
    boot_algo = "WildBootTests.jl"
  )
  
  
  class(res_final) <- "boottest"
  
  invisible(res_final)
  
}