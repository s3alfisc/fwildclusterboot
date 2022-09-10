bootagg = function(
    x,
    agg,
    full = FALSE,
    use_weights = TRUE,
    B = 999, 
    bootstrap_type = "11", 
    clustid,
    nthreads = 1,
    ...){
  
  #' Aggregates the value of coefficients. Inference via a wild cluster
  #' bootstrap 
  #' Most of this function is written by Laurent Bergé and used 
  #' in the fixest package published under GPL-3, 
  #' https://cran.r-project.org/web/packages/fixest/index.html
  #' minor changes by Alexander Fischer
  #' 
  #' @importFrom dreamerr check_value_plus
  #' @examples 
  #' 
  #' #' library(fixest)
  #' #' library(fwildclusterboot)
  #' 
  #' set.seed(123456L)
  #' 
  #' # code by Grant McDermott
  #' # 60 time periods, 30 individuals, and 5 waves of treatment
  #' tmax = 22; imax = 500; nlvls = 5
  #' 
  #' dat =
  #'   expand.grid(time = 1:tmax, id = 1:imax) |>
  #'   within({
  #' 
  #'     cohort      = NA
  #'     effect      = NA
  #'     first_treat = NA
  #' 
  #'     for (chrt in 1:imax) {
  #'       cohort = ifelse(id==chrt, sample.int(nlvls, 1), cohort)
  #'     }
  #' 
  #'     for (lvls in 1:nlvls) {
  #'       effect      = ifelse(cohort==lvls, sample(2:10 , 1), effect)
  #'       first_treat = ifelse(cohort==lvls, sample(1:(tmax+20), 1), first_treat)
  #'     }
  #' 
  #'     first_treat = ifelse(first_treat>tmax, Inf, first_treat)
  #'     treat       = time>=first_treat
  #'     rel_time    = time - first_treat
  #'     y           = id + time + ifelse(treat, effect*rel_time, 0) + rnorm(imax*tmax)
  #' 
  #'     rm(chrt, lvls, cohort, effect)
  #'   })
  #' 
  #' head(dat)
  #' dim(dat)
  #' length(unique(dat$time))
  #' dat <- na.omit(dat)
  #' 
  #' object = feols(
  #'   y ~ sunab(first_treat, rel_time) + id + time,
  #'   data = dat, vcov = ~id
  #' )
  #' 
  #' # run - else error in expand.data.frame
  #' sunab <- fixest:::sunab
  #' res <-
  #'   fwildclusterboot:::bootagg(
  #'     object,
  #'     agg = TRUE,
  #'     full = FALSE,
  #'     use_weights = TRUE,
  #'     B = 9999,
  #'     bootstrap_type = "11",
  #'     clustid = ~ time,
  #'     nthreads = 8
  #'   )
  #' 
  #' 
  #' res
  #' sunab_p <- pvalue(summary(object, agg = TRUE))
  #' nparams <- length(sunab_p)
  #' cbind(
  #'   res,
  #'   round(sunab_p[-nparams][-(nparams-1)][-1], 3)
  #' )


  check_arg(x, "class(fixest) mbt")
  if(isTRUE(x$is_sunab)){
    check_arg(agg, "scalar(character, logical)")
  } else {
    check_arg(agg, "character scalar")
  }

  check_arg(full, "logical scalar")
  # => later => extend it to more than one set of vars to agg

  dots = list(...)
  from_summary = isTRUE(dots$from_summary)

  no_agg = FALSE
  agg_rm = NULL
  check_value_plus(agg, "match(att, period, cohort, TRUE) | scalar")
  if(agg %in% c("att", "period", "cohort", "TRUE")){
    if(isTRUE(x$is_sunab)){
      agg_name = names(agg)
      if(agg == "att"){
        agg = x$model_matrix_info$sunab$agg_att
        # we also remove the previous vars
        agg_rm = gsub("E::", "E::-?", agg, fixed = TRUE)
      } else if(agg == "cohort"){
        agg = c("cohort" = "::[^-].*:cohort::(.+)")
        agg_rm = gsub("E::", "E::-?", x$model_matrix_info$sunab$agg_att, fixed = TRUE)
      } else {
        agg = x$model_matrix_info$sunab$agg_period
      }
      if(!is.null(agg_name)) names(agg) = agg_name
    }
  } else if(isFALSE(agg)){
    agg = c("nothing to remove" = "we want all the coefficients")
  }

  is_name = !is.null(names(agg))

  if(!is_name && !grepl("(", agg, fixed = TRUE)){
    stop("Argument 'agg' must be a character in which the pattern to match must be in between parentheses. So far there are no parenthesis: please have a look at the examples.")
  }

  coef = x$coefficients
  cname = names(coef)

  qui = grepl(agg, cname, perl = TRUE)
  if(!any(qui)){
    if(from_summary){
      # We make it silent when aggregate is used in summary
      # => this way we can pool calls to agg even for models that don't have it
      # ==> useful in etable eg
      return(list(coeftable = x$coeftable, model_matrix_info = x$model_matrix_info))
    } else if(no_agg){
      x = summary(x, agg = FALSE, ...)
      return(x$coeftable)
    } else {
      stop("The argument 'agg' does not match any variable.")
    }
  }

  if(!isTRUE(x$summary)){
    x = summary(x, ...)
  }

  cname_select = cname[qui]
  if(is_name){
    root = rep(names(agg), length(cname_select))
    val = gsub(paste0(".*", agg, ".*"), "\\1", cname_select, perl = TRUE)
  } else {
    root = gsub(paste0(".*", agg, ".*"), "\\1", cname_select, perl = TRUE)
    val = gsub(paste0(".*", agg, ".*"), "\\2", cname_select, perl = TRUE)
  }

  mm = model.matrix(x)
  param = colnames(mm)[1]

  cat("Step 1: run wild bootstrap", "\n")
  boot_fit <-
  boottest(
    object = x,
    param = param,
    clustid = clustid,
    B = B,
    impose_null = FALSE,
    bootstrap_type= bootstrap_type, 
    nthreads = nthreads
  )

  B <- boot_fit$boot_iter
  #V <- boot_fit$vcov_boot
  V_boot <- boot_fit$boot_vcov
  V = x$cov.scaled
  coef <- x$coefficients
  coef_boot <- boot_fit$boot_coef

  #V = x$cov.scaled


  name_df = unique(data.frame(root, val, stringsAsFactors = FALSE))

  nk <- nrow(name_df)
  c_all = vector(mode = "numeric", length = nk)
  se_all = vector(mode = "numeric", length = nk)
  c_all_boot <- matrix(NA, nk, B+1)
  se_all_boot <- matrix(NA, nk, B+1)

  cat("Step 2: compute weighted estimator"," \n")
  for(i in 1:nk){
    r = name_df[i, 1]
    v = name_df[i, 2]
    v_names = cname_select[root == r & val == v]

    if(use_weights && !is.null(x$weights)){
      shares = colSums(x$weights * sign(mm[, v_names, drop = FALSE]))
    } else {
      shares = colSums(sign(mm[, v_names, drop = FALSE]))
    }

    shares = shares / sum(shares)

    # The coef
    c_value = sum(shares * coef[v_names])

    # The variance
    n = length(v_names)
    s1 = matrix(shares, n, n)
    s2 = matrix(shares, n, n, byrow = TRUE)

    s <- s1 * s2
    
    var_value = sum(s * V[v_names, v_names])
    se_value = sqrt(var_value)

    v_names_pos <- which(names(coef) %in% v_names)

    c_all[i] = c_value
    se_all[i] = se_value


    # create weighted bootstrap coefs and ses
   
    c_all_boot[i,] <- get_c_all_boot_cpp(
      coef_boot = coef_boot[v_names_pos,], 
      shares= shares, 
      B = B, 
      cores = nthreads
    )
    # se_all_boot[i,] <- get_se_all_boot_cpp(
    #   V_boot = V_boot[v_names_pos, v_names_pos, ],
    #   s= s,
    #   B = B,
    #   cores = nthreads
    # )
    
    for(b in 1:(B+1)){
      var_value = sum(s * V_boot[v_names_pos, v_names_pos,b])
      se_all_boot[i,b] = sqrt(var_value)
    }
    

  }
    # th z & p values
    zvalue = c_all/se_all
    zvalue_boot <- c_all_boot / se_all_boot

    pvalue <- rowMeans(abs(zvalue) < abs(zvalue_boot))

    pvalue
    #pvalue(summary(x, agg = "TRUE"))
    #summary(x, agg = "TRUE")

    

  # res = cbind(c_all, se_all, zvalue, pvalue)
  # if(max(nchar(val)) == 0){
  #   rownames(res) = name_df[[1]]
  # } else {
  #   rownames(res) = apply(name_df, 1, paste, collapse = "::")
  # }
  # 
  # colnames(res) = c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  # 
  # if(full){
  #   if(!is.null(agg_rm)){
  #     qui = grepl(agg_rm, cname, perl = TRUE)
  #   }
  # 
  #   table_origin = x$coeftable
  #   i_min = min(which(qui)) - 1
  #   before = if(i_min > 0) table_origin[1:i_min, , drop = FALSE] else NULL
  # 
  #   i_after = (1:nrow(table_origin)) > i_min & !qui
  #   after = if(any(i_after)) table_origin[i_after, , drop = FALSE] else NULL
  # 
  #   res = rbind(before, res, after)
  # 
  #   attr(res, "type") = attr(table_origin, "type")
  # }
  # 
  # if(from_summary){
  #   # We add the model_matrix_info needed in iplot()
  #   mm_info = x$model_matrix_info
  #   mm_info_agg = attr(agg, "model_matrix_info")
  #   if(!is.null(mm_info_agg)){
  #     tmp = list(mm_info_agg)
  #     for(i in seq_along(mm_info)){
  #       my_name = names(mm_info)[i]
  #       if(my_name != ""){
  #         tmp[[my_name]] = mm_info[[i]]
  #       } else {
  #         tmp[[1 + i]] = mm_info[[i]]
  #       }
  # 
  #     }
  #     mm_info = tmp
  #   }
  # 
  #   res = list(coeftable = res, model_matrix_info = mm_info)
  # }
  # 
  # 
  # res
}



