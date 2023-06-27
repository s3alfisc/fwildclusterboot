#' Simple tool that aggregates the value of CATT coefficients in
#' staggered difference-in-difference setups with inference based on 
#' a wild cluster bootstrap (see details) - similar to `fixest::aggregate()`
#' 
#' This is a function helping to replicate the estimator from Sun and
#' Abraham (2021, Journal of Econometrics). You first need to perform 
#' an estimation with cohort and relative periods dummies 
#' (typically using the function i), this leads to estimators of the 
#' cohort average treatment effect on the treated (CATT). Then you can
#' use this function to retrieve the average treatment effect on each 
#' relative period,or for any other way you wish to aggregate the CATT.
#' 
#' Note that contrary to the SA article, here the cohort share 
#' in the sample is considered to be a perfect measure for the 
#' cohort share in the population.
#' 
#' Most of this function is written by Laurent Bergé and used 
#' in the fixest package published under GPL-3, 
#' https://cran.r-project.org/web/packages/fixest/index.html
#' minor changes by Alexander Fischer
#' 
#' @param x An object of type fixest estimated using `sunab()`
#' @param agg A character scalar describing the variable names to be
#'  aggregated, it is pattern-based. All variables that match the pattern 
#'  will be aggregated. It must be of the form `"(root)"`, the parentheses 
#'  must be there and the resulting variable name will be `"root"`. You 
#'  can add another root with parentheses: `"(root1)regex(root2)"`, in
#'  which case the resulting name is `"root1::root2"`. To name the resulting 
#'  variable differently you can pass a named vector: `c("name" = "pattern")`
#'  or `c("name" = "pattern(root2)")`. It's a bit intricate sorry, please
#'  see the examples.
#' @param full Logical scalar, defaults to `FALSE`. If `TRUE`, then all 
#' coefficients are returned, not only the aggregated coefficients.
#' @param use_weights Logical, default is `TRUE`. If the estimation was 
#' weighted, whether the aggregation should take into account the weights.
#'  Basically if the weights reflected frequency it should be `TRUE`.
#' @param clustid A character vector or rhs formula containing the names of the
#' cluster variables. If NULL,
#'        a heteroskedasticity-robust (HC1) wild bootstrap is run.
#' @param B Integer. The number of bootstrap iterations. When the number of
#'  clusters is low,
#'        increasing B adds little additional runtime.
#' @param bootcluster A character vector or rhs formula of length 1. Specifies
#' the bootstrap clustering variable or variables. If more
#'        than one variable is specified, then bootstrapping is clustered by the
#'         intersections of
#'        clustering implied by the listed variables. To mimic the behavior of
#'        stata's boottest command,
#'        the default is to cluster by the intersection of all the variables
#'        specified via the `clustid` argument,
#'        even though that is not necessarily recommended (see the paper by
#'         Roodman et al cited below, section 4.2).
#'        Other options include "min", where bootstrapping is clustered by
#'        the cluster variable with the fewest clusters.
#'        Further, the subcluster bootstrap (MacKinnon & Webb, 2018) is
#'         supported - see the `vignette("fwildclusterboot", package =
#'          "fwildclusterboot")` for details.
#' @param fe A character vector or rhs formula of length one which contains
#' the name of the fixed effect to be projected
#'        out in the bootstrap. Note: if regression weights are used, fe
#'        needs to be NULL.
#' @param sign_level A numeric between 0 and 1 which sets the significance level
#'        of the inference procedure. E.g. sign_level = 0.05
#'        returns 0.95% confidence intervals. By default, sign_level = 0.05.
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
#' @param bootstrap_type Determines which wild cluster bootstrap type should be 
#' run. Options are "fnw11", which runs a "11" type 
#' wild cluster bootstrap via the algorithm outlined in "fast and wild" 
#' (Roodman et al (2019)). 
#' @param beta0 Deprecated function argument. Replaced by function argument 'r'.
#' @param type character or function. The character string specifies the type
#'        of boostrap to use: One of "rademacher", "mammen", "norm"
#'        and "webb". Alternatively, type can be a function(n) for drawing
#'        wild bootstrap factors. "rademacher" by default.
#'        For the Rademacher distribution, if the number of replications B
#'        exceeds
#'        the number of possible draw ombinations, 2^(#number of clusters),
#'         then `boottest()`
#'        will use each possible combination once (enumeration).
#' @param impose_null Logical. Controls if the null hypothesis is imposed on
#'        the bootstrap dgp or not. Null imposed `(WCR)` by default.
#'        If FALSE, the null is not imposed `(WCU)`
#' @param p_val_type Character vector of length 1. Type of p-value.
#'        By default "two-tailed". Other options include "equal-tailed",
#'        ">" and "<".
#' @param tol Numeric vector of length 1. The desired accuracy
#'        (convergence tolerance) used in the root finding procedure to find
#'         the confidence interval.
#'        1e-6 by default.
#' @param maxiter Integer. Maximum number of iterations used in the root
#' finding procedure to find the confidence interval.
#'        10 by default.
#' @param nthreads The number of threads. Can be: a) an integer lower than,
#'                 or equal to, the maximum number of threads; b) 0: meaning
#'                 all available threads will be used; c) a number strictly
#'                 between 0 and 1 which represents the fraction of all threads
#'                 to use. The default is to use 1 core.
#' @param ssc An object of class `boot_ssc.type` obtained with the function
#'  [fwildclusterboot::boot_ssc()]. Represents how the small sample
#'   adjustments are computed. The defaults are `adj = TRUE, fixef.K = "none",
#'   cluster.adj = "TRUE", cluster.df = "conventional"`.
#'             You can find more details in the help file for `boot_ssc()`.
#'             The function is purposefully designed to mimic fixest's
#'             [fixest::ssc()] function.
#' @param getauxweights Logical. Whether to save auxilliary weight matrix (v)
#' @param floattype Float64 by default. Other option: Float32. Should floating
#'  point numbers in Julia be represented as 32 or 64 bit? Only relevant when
#'   'engine = "WildBootTests.jl"'
#' @param maxmatsize NULL by default = no limit. Else numeric scalar to set
#' the maximum size of auxilliary weight matrix (v), in gigabytes. Only
#' relevant when 'engine = "WildBootTests.jl"'
#' @param bootstrapc Logical scalar, FALSE by default. TRUE  to request
#' bootstrap-c instead of bootstrap-t. Only relevant when
#' 'engine = "WildBootTests.jl"'
#' @param sampling 'dqrng' or 'standard'. If 'dqrng', the 'dqrng' package is
#' used for random number generation (when available). If 'standard', 
#' functions from the 'stats' package are used when available. 
#' This argument is mostly a convenience to control random number generation in 
#' a wrapper package around `fwildclusterboot`, `wildrwolf`. 
#' I recommend to use the fast' option. 
#' @param ... misc function arguments 
#' 
#' @export
#' 
#' @importFrom utils setTxtProgressBar txtProgressBar
#' 
#' @return A data frame with aggregated coefficients, p-values and 
#' confidence intervals. 
#' 
#' @importFrom dreamerr check_value_plus
#' 
#' @examples 
#' 
#' \dontrun{
#' if(requireNamespace("fixest")){
#'library(fixest)
#'data(base_stagg)
#'# The DiD estimation
#'res_sunab = feols(y ~ x1 + sunab(year_treated, year) | id + year, base_stagg)
#'res_sunab_3ref = feols(y ~ x1 + sunab(
#'  year_treated, year, ref.p = c(.F + 0:2, -1)) |
#'                         id + year, 
#'                       cluster = "id",
#'                       base_stagg, 
#'                       ssc = ssc(adj = FALSE, cluster.adj = FALSE))
#'
#' aggregate(res_sunab, agg = "ATT")
#'# test ATT equivalence
#'boot_att <- 
#'  boot_aggregate(
#'    res_sunab, 
#'    B = 9999, 
#'    agg = "ATT", 
#'    clustid = "id" 
#'  )
#' head(boot_att)
#' 
#'#'boot_agg2 <- 
#'  boot_aggregate(
#'    res_sunab, 
#'    B = 99999, 
#'    agg = TRUE,
#'    ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE)
#'  )
#' 
#' }
#' }


boot_aggregate <- function(
    x,
    agg,
    full = FALSE,
    use_weights = TRUE,
    clustid = NULL, 
    B,
    bootcluster = "max",
    fe = NULL,
    sign_level = 0.05,
    beta0 = NULL,
    type = "rademacher",
    impose_null = TRUE,
    bootstrap_type = "fnw11",
    p_val_type = "two-tailed",
    nthreads = getBoottest_nthreads(),
    tol = 1e-06, 
    maxiter = 10,
    ssc = boot_ssc(
      adj = TRUE,
      fixef.K = "none",
      cluster.adj = TRUE,
      cluster.df = "conventional"
    ),
    engine = getBoottest_engine(),
    floattype = "Float64",
    maxmatsize = FALSE,
    bootstrapc = FALSE,
    getauxweights = FALSE,
    sampling = "dqrng",
    ...){
  
  
  # note: all boottest function arguments are tested in boottest()
  # therefore, only check for supported subset of features
  
  check_arg(bootstrap_type, "charin(fnw11)")
  
  check_arg(full, "logical scalar")
  # => later => extend it to more than one set of vars to agg
  
  # remind packages users to set a global seed
  inform_seed(
    frequency_id = "seed-reminder-bootagg", 
    engine = engine
  )
  
  dots <- list(...)
  from_summary <- isTRUE(dots$from_summary)
  
  no_agg <- FALSE
  agg_rm <- NULL
  check_value_plus(agg, "match(att, period, cohort, TRUE) | scalar")
  if(agg %in% c("att", "period", "cohort", "TRUE")){
    if(isTRUE(x$is_sunab)){
      agg_name <- names(agg)
      if(agg == "att"){
        agg <- x$model_matrix_info$sunab$agg_att
        # we also remove the previous vars
        agg_rm <- gsub("E::", "E::-?", agg, fixed = TRUE)
      } else if(agg == "cohort"){
        agg <- c("cohort" = "::[^-].*:cohort::(.+)")
        agg_rm <- gsub("E::", "E::-?", 
                       x$model_matrix_info$sunab$agg_att, fixed = TRUE)
      } else {
        agg <- x$model_matrix_info$sunab$agg_period
      }
      if(!is.null(agg_name)) names(agg) <- agg_name
    }
  } else if(isFALSE(agg)){
    agg <- c("nothing to remove" = "we want all the coefficients")
  }
  
  is_name <- !is.null(names(agg))
  
  if(!is_name && !grepl("(", agg, fixed = TRUE)){
    rlang::abort(
        c("Argument 'agg' must be a character in
         which the pattern to match must be in between parentheses.
         So far there are no parenthesis: please have a look at the examples."), 
        use_cli_format = TRUE
    )
  }
  
  coef <- x$coefficients
  cname <- names(coef)
  V <- x$cov.scaled
  
  
  qui <- grepl(agg, cname, perl = TRUE)
  if(!any(qui)){
    if(from_summary){
      # We make it silent when aggregate is used in summary
      # => this way we can pool calls to agg even for models that don't have it
      # ==> useful in etable eg
      return(list(coeftable = x$coeftable, 
                  model_matrix_info = x$model_matrix_info))
    } else if(no_agg){
      x <- summary(x, agg = FALSE, ...)
      return(x$coeftable)
    } else {
      rlang::abort(
        "The argument 'agg' does not match any variable.", 
        use_cli_format = TRUE
      )
    }
  }
  
  if(!isTRUE(x$summary)){
    x <- summary(x, ...)
  }
  
  cname_select <- cname[qui]
  if(is_name){
    root <- rep(names(agg), length(cname_select))
    val <- gsub(paste0(".*", agg, ".*"), "\\1", cname_select, perl = TRUE)
  } else {
    root <- gsub(paste0(".*", agg, ".*"), "\\1", cname_select, perl = TRUE)
    val <- gsub(paste0(".*", agg, ".*"), "\\2", cname_select, perl = TRUE)
  }
  
  mm <- model.matrix(x)
  
  cat("Run the wild bootstrap: this might take some time...(but 
      hopefully not too much time =) ).", "\n")
  
  name_df <- unique(data.frame(root, val, stringsAsFactors = FALSE))
  
  nk <- nrow(name_df)
  c_all <- vector(mode = "numeric", length = nk)
  se_all <- vector(mode = "numeric", length = nk)
  pvalues <- c_all 
  conf_int <- matrix(NA, nk, 2)
  
  pb <- txtProgressBar(min = 0, max = nk, initial = 0, style = 3) 
  
  for(i in 1:nk){
    
    setTxtProgressBar(pb,i)
    
    r <- name_df[i, 1]
    v <- name_df[i, 2]
    v_names <- cname_select[root == r & val == v]
    
    if(use_weights && !is.null(x$weights)){
      shares <- colSums(x$weights * sign(mm[, v_names, drop = FALSE]))
    } else {
      shares <- colSums(sign(mm[, v_names, drop = FALSE]))
    }
    
    shares <- shares / sum(shares)
    
    # The coef
    c_value <- sum(shares * coef[v_names])
    
    # The variance
    n <- length(v_names)
    s1 <- matrix(shares, n, n)
    s2 <- matrix(shares, n, n, byrow = TRUE)
    
    s <- s1 * s2
    
    var_value <- sum(s * V[v_names, v_names])
    se_value <- sqrt(var_value)
    
    v_names_pos <- which(names(coef) %in% v_names)
    
    c_all[i] <- c_value
    se_all[i] <- se_value
    
    param <- names(shares)
    R <- shares
    
    boot_fit <- 
      boottest(
        object = x,
        param = param,
        R = shares,
        clustid = clustid,
        B = B,
        bootcluster = bootcluster,
        fe = fe,
        sign_level = sign_level,
        conf_int = TRUE,
        r = 0,
        beta0 = beta0,
        type = type,
        impose_null = impose_null,
        bootstrap_type = bootstrap_type,
        p_val_type = p_val_type,
        #tol = tol,
        #maxiter = maxiter,
        nthreads = nthreads,
        engine = engine,
        floattype = floattype,
        maxmatsize = maxmatsize,
        bootstrapc = bootstrapc,
        getauxweights = getauxweights, 
        sampling = sampling
      )
    
    pvalues[i] <- pval(boot_fit)
    if(!is.null(clustid)){
        conf_int[i,] <- fwildclusterboot:::confint.boottest(boot_fit)
    } else {
      conf_int[i,] <- rep(NA, 2)
    }

  }
  # th z & p values
  zvalue <- c_all/se_all
  
  res <- cbind(
    c_all,
    pvalues, 
    conf_int
  )
  colnames(res) <- c(
    "Estimate",
    "Pr(>|t|)", 
    paste0("[",sign_level / 2, "%"),
    paste0(1 - (sign_level / 2), "%","]")
  )
  
  res
  
}



