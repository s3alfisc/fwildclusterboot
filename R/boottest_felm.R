boottest.felm <- function(object,
                          # clustid,
                          param,
                          B,
                          clustid,
                          bootcluster = "max",
                          fe = NULL,
                          conf_int = NULL,
                          seed = NULL,
                          beta0 = 0,
                          sign_level = NULL,
                          type = "rademacher",
                          impose_null = TRUE,
                          p_val_type = "two-tailed",
                          ...) {


  #' Conducts wild cluster bootstrap inference for object of class felm.
  #' @import data.table
  #' @param object An object of class fixest
  #' @param clustid A vector with the clusters
  #' @param param The univariate coefficients for which a hypothesis is to be tested
  #' @param B number of bootstrap iterations
  #' @param bootcluster A character vector. Sets the cluster used in the boottest. Chooses the largest cluster by default
  #' @param fe A character scalar. Fixed effect to be projected out in the bootstrap
  #' @param sign_level A numeric between 0 and 1. E.g. sign_level = 0.05 returns 0.95% confidence intervals. By default, sign_level = 0.05.
  #' @param conf_int A logical vector. If TRUE, boottest computes confidence intervals by p-value inversion
  #' @param seed An integer. Allows the user to set a random seed
  #' @param beta0 A numeric. Shifts the null hypothesis
  #' @param type character or function. The character string specifies the type of boostrap to use: One of "rademacher", "mammen", "norm" and "webb". Alternatively, type can be a function(n) for drawing wild bootstrap factors. "rademacher" by default.
  #' @param impose_null Logical. Controls if the null hypothesis is imposed on the bootstrap dgp or not. Null imposed - WCR - by default. If FALSE, unrestricted WCU
  #' @param p_val_type type Type of p-value. By default "two-tailed". Other options: "equal-tailed"
  #' @param ... Further arguments passed to or from other methods.
  #' @return An object of class \code{boottest}
  #' \item{p_val}{The bootstrap p-value.}
  #' \item{t_stat}{The bootstrap t-statistic.}
  #' \item{conf_int}{The bootstrap confidence interval.}
  #' \item{param}{The tested parameter.}
  #' \item{N}{Sample size. Might differ from the regression sample size if the cluster variables contain NA values.}
  #' \item{B}{Number of Bootstrap Iterations.}
  #' \item{clustid}{Names of the cluster Variables.}
  #' \item{N_G}{Dimension of the cluster variables as used in boottest.}
  #' \item{sign_level}{Significance level used in boottest.}
  #' \item{type}{Distribution of the bootstrap weights.}
  #' \item{p_test_vals}{All p-values calculated while calculating the confidence interval.}
  #' \item{test_vals}{All t-statistics calculated while calculating the confidence interval.}
  #' \item{regression}{The regression object used in boottest.}
  #' \item{call}{Function call of boottest.}
  #' @export
  #' @method boottest felm
  #' @section Confidence Intervals:
  #' \code{boottest} computes confidence intervals by inverting p-values. In practice, the following procedure is used:
  #' \itemize{
  #' \item Based on an initial guess for starting values, calculate p-values for 26 equal spaced points between the starting values.
  #' \item Out of the 26 calculated p-values, find the two pairs of values x for which the corresponding p-values px cross the significance level sign_level.
  #' \item Feed the two pairs of x into an numerical root finding procedure and solve for the root. boottest currently relies on \code{stats::uniroot} and sets an absolute tolerance of 1e-06 and stops the procedure after 10 iterations.
  #' }
  #' @section Standard Errors:
  #' \code{boottest} does not calculate standard errors.
  #' @references Roodman et al., 2019, "Fast and wild: Bootstrap inference in Stata using boottest", The Stata Journal. (\url{https://journals.sagepub.com/doi/full/10.1177/1536867X19830877})
  #' @examples 
  #' library(fwildclusterboot)
  #' library(lfe)
  #' # voters <- create_data_2(N = 10000, N_G1 = 20, icc1 = 0.91, N_G2 = 10,
  #' #                        icc2 = 0.51, numb_fe1 = 10, numb_fe2 = 10, seed = 12345)
  #' felm_fit <-felm(proposition_vote ~ treatment + ideology1 + log_income
  #'            | Q1_immigration, 
  #'            data = create_data_2(N = 10000, N_G1 = 20, icc1 = 0.91, N_G2 = 10,
  #'                    icc2 = 0.51, numb_fe1 = 10, numb_fe2 = 10, seed = 12345))
  #' boot1 <- boottest(felm_fit, B = 9999, param = "treatment", clustid = "group_id1")
  #' boot2 <- boottest(felm_fit, B = 9999, param = "treatment", clustid = c("group_id1", "group_id2"))
  #' boot3 <- boottest(felm_fit, B = 9999, param = "treatment", clustid = c("group_id1", "group_id2"),
  #'                  fe = "Q1_immigration")
  #' boot4 <- boottest(felm_fit, B = 999, param = "treatment", clustid = c("group_id1", "group_id2"),
  #' fe = "Q1_immigration", sign_level = 0.2, seed = 8, beta0 = 2)
  #' summary(boot1)
  #' tidy(boot1)
  #' plot(boot1)


  call <- match.call()


  # check_arg(clustid, "os formula | data.frame | named list")
  check_arg(param, "scalar character")
  check_arg(B, "scalar integer ")
  check_arg(sign_level, "scalar numeric | NULL")
  check_arg(conf_int, "logical scalar | NULL")
  check_arg(seed, "scalar integer | NULL")
  check_arg(beta0, "numeric scalar | NULL")
  check_arg(fe, "character scalar | NULL")
  check_arg(bootcluster, "character vector")


  if(!(p_val_type %in% c("two-tailed", "equal-tailed"))){
    stop("The function argument p_val_type must be either two-tailed or equal-tailed.")
  }
  
  if ((conf_int == TRUE || is.null(conf_int)) & B <= 100) {
    stop("The function argument B is smaller than 100. The number of bootstrap iterations needs to be 100 or higher in order to guarantee that the root
         finding procudure used to find the confidence set works properly.",
      call. = FALSE
    )
  }
  if (!is.null(sign_level) & (sign_level <= 0 || sign_level >= 1)) {
    stop("The function argument sign_level is outside of the unit interval (0, 1). Please specify sign_level so that it is within the unit interval.",
      call. = FALSE
    )
  }

  if (is.null(sign_level)) {
    sign_level <- 0.05
  }

  if (!(param %in% c(rownames(object$coefficients)))) {
    stop(paste("The parameter", param, "is not included in the estimated model. Maybe you are trying to test for an interaction parameter? To see all model parameter names, run names(coef(model))."))
  }
  # repeat the same: check if fe is in the data.frame

  if (is.null(beta0)) {
    beta0 <- 0
  }

  if (!is.null(fe) && fe %in% clustid) {
    stop(paste("The function argument fe =", fe, "is contained in the clustering variables. This is not allowed. Please set fe to another factor variable or NULL."),
      call. = FALSE
    )
  }

  if (((1 - sign_level) * (B + 1)) %% 1 != 0) {
    message(paste("Note: The bootstrap usually performs best when the confidence level (here,", 1 - sign_level, "%) times the number of replications plus 1 (", B, "+ 1 = ", B + 1, ") is an integer."))
  }

  # throw error if specific function arguments are used in felm() call
  call_object <- names(object$call)[names(object$call) != ""]
  banned_fun_args <- c("contrasts", "subset")
  if (sum(call_object %in% banned_fun_args) > 0) {
    stop(paste(
      "boottest.felm currently does not accept objects of type fixest with function arguments",
      paste0(banned_fun_args[1:(length(banned_fun_args) - 1)], collapse = ", "), "and", banned_fun_args[length(banned_fun_args)], "."
    ),
    call. = FALSE
    )
  }

  # returns function
  # function taken from the sandwich package' vcovBS.lm function
  wild_draw_fun <- switch(type,
    rademacher = function(n) sample(c(-1, 1), n, replace = TRUE),
    mammen = function(n) sample(c(-1, 1) * (sqrt(5) + c(-1, 1)) / 2, n, replace = TRUE, prob = (sqrt(5) + c(1, -1)) / (2 * sqrt(5))),
    norm = function(n) rnorm(n),
    webb = function(n) sample(c(-sqrt((3:1) / 2), sqrt((1:3) / 2)), n, replace = TRUE),
    wild_draw_fun
  )

  # preprocess data: X, Y, weights, fixed effects
  preprocess <- preprocess2(object = object, cluster = clustid, fe = fe, param = param, bootcluster = bootcluster)

  clustid_dims <- preprocess$clustid_dims
  # Invert p-value
  point_estimate <- object$coefficients[param, ]

  N_G_2 <- 2^length(unique(preprocess$bootcluster[, 1]))
  if (type == "rademacher" & N_G_2 < B) {
    warning(paste("There are only", N_G_2, "unique draws from the rademacher distribution for", length(unique(preprocess$bootcluster[, 1])), "clusters. Therefore, 
                  B = ", N_G_2, ". Consider using webb weights instead."),
      call. = FALSE
    )
    B <- N_G_2
  }


  res <- boot_algo2(preprocess,
    boot_iter = B,
    wild_draw_fun = wild_draw_fun,
    point_estimate = point_estimate,
    impose_null = impose_null,
    beta0 = beta0,
    sign_level = sign_level,
    param = param,
    seed = seed,
    p_val_type = p_val_type
  )



  # compute confidence sets
  if (is.null(conf_int) || conf_int == TRUE) {
    
    if(impose_null == TRUE){
      # should always be positive, point_estimate and t_stat need to have same sign
      se_guess <- abs(point_estimate / res$t_stat)
    } else if(impose_null == FALSE){
      se_guess <- abs((point_estimate - beta0) / res$t_stat)
    }

    res_p_val <- invert_p_val2(
      object = res,
      boot_iter = B,
      point_estimate = point_estimate,
      se_guess = se_guess,
      clustid = preprocess$clustid,
      sign_level = sign_level,
      vcov_sign = preprocess$vcov_sign,
      impose_null = impose_null,
      p_val_type = p_val_type
    )
  } else {
    res_p_val <- list(
      conf_int = NA,
      p_test_vals = NA,
      test_vals = NA
    )
  }


  res_final <- list(
    point_estimate = point_estimate,
    p_val = res[["p_val"]],
    conf_int = res_p_val$conf_int,
    p_test_vals = res_p_val$p_test_vals,
    test_vals = res_p_val$test_vals,
    t_stat = res$t_stat,
    regression = res$object,
    param = param,
    N = preprocess$N,
    B = B,
    clustid = clustid,
    # depvar = depvar,
    N_G = preprocess$N_G,
    sign_level = sign_level,
    call = call,
    type = type,
    impose_null = impose_null
  )



  class(res_final) <- "boottest"

  invisible(res_final)
}
