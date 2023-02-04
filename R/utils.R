#' Simulate Data
#'
#' Function simulates data for tests and examples with clustering variables
#' and fixed-effects.
#'
#' @param N number of observations
#' @param N_G1 A scalar. number of clusters for clustering variable 1
#' @param icc1 A scalar between 0 and 1. intra-cluster correlation for
#'  clustering variable 1
#' @param N_G2 A scalar. number of clusters for clustering variable 2
#' @param icc2 A scalar between 0 and 1. intra-cluster correlation for
#'  clustering variable 2
#' @param numb_fe1 A scalar. Number of fixed effect for first factor variable
#' @param numb_fe2 A scalar. Number of fixed effect for second factor variable
#' @param seed An integer. Set the random seed
#' @param weights Possible regression weights to be used in estimation
#' @return A simulated `data.frame` with specified numbers of clusters,
#'         intra-cluster correlations and dimensionality of fixed effects.
#' @examples
#' df <- create_data(
#'   N = 1000,
#'   N_G1 = 1000,
#'   icc1 = 0.5,
#'   N_G2 = 10,
#'   icc2 = 0.01,
#'   numb_fe1 = 10,
#'   numb_fe2 = 10,
#'   seed = 2293
#' )
#' @noRd

# This function very closely mirrors an example from the fabricatr package
# website, which can be found under the following link:
# https://declaredesign.org/r/fabricatr/articles/getting_started.html
# https://declaredesign.org/r/fabricatr/articles/getting_started.html
# the fabricatr package can be downloaded from CRAN and is published under
# MIT license.
# Graeme Blair [aut, cre], Jasper Cooper [aut], Alexander Coppock [aut],
# Macartan Humphreys [aut], Aaron Rudkin [aut], Neal Fultz [aut]
# are the authors of the fabricatr package

create_data <-
  function(N,
           N_G1,
           icc1,
           N_G2,
           icc2,
           numb_fe1,
           numb_fe2,
           seed,
           weights) {
    set.seed(seed)
    voters <-
      fabricatr::fabricate(
        N,
        group_id1 = sample(1:N_G1, N, replace = TRUE),
        group_id2 = sample(1:N_G2, N, replace = TRUE),
        ideology1 = fabricatr::draw_normal_icc(
          mean = 0,
          N = N,
          clusters = group_id1,
          ICC = icc1
        ),
        ideology2 = fabricatr::draw_normal_icc(
          mean = 0,
          N = N,
          clusters = group_id2,
          ICC = icc2
        ),
        ideological_label = fabricatr::draw_ordered(
          x = ideology1,
          break_labels = c(
            "Very Conservative",
            "Conservative",
            "Liberal",
            "Very Liberal"
          )
        ),
        income = exp(rlnorm(
          n = N,
          meanlog = 2.4 - (ideology1 * 0.1),
          sdlog = 0.12
        )),
        # Q1_immigration_latent = rnorm(N),
        # Q1_immigration = ifelse(Q1_immigration_latent > 0.5, 1,
        #  ifelse(Q1_immigration_latent <= 0.5 &
        # Q1_immigration_latent > 0, 2, 3)
        # ),
        # Q2_defense_latent = rnorm(N, 0, 3),
        # Q2_defense = ifelse(Q2_defense_latent > 0.5, 1,
        #  ifelse(Q2_defense_latent <= 0.5 & Q2_defense_latent > 0, 2, 3)
        # ),
        Q1_immigration = sample(1:numb_fe1, N, TRUE),
        Q2_defense = sample(1:numb_fe2, N, TRUE),
        treatment = fabricatr::draw_binary(0.5, N = N),
        proposition_vote = fabricatr::draw_binary(
          latent = ideology1 + ideology2 + 0.2 * treatment +
            2 * Q1_immigration + rnorm(N, 0, 3),
          link = "probit"
        ),
        state = rep(1:(N / 20), 20),
        year = sort(rep(1:20, N / 20))
      )
    
    voters$Q1_immigration <- as.factor(voters$Q1_immigration)
    voters$Q2_defense <- as.factor(voters$Q2_defense)
    
    
    voters$log_income <- log(voters$income)
    voters$Q1_immigration <- as.factor(voters$Q1_immigration)
    
    # add weights
    voters$weights <- sample(1:10, N, replace = TRUE) / 10
    
    voters
  }


# gtools_permutations <-
#   function(n,
#            r,
#            v = 1:n,
#            set = TRUE,
#            repeats.allowed = FALSE) {
#     #' copied from the permutations package as long as it is orphaned on CRAN
#     #' @param n size of source vector
#     #' @param r size of target vector
#     #' @param v source vector. defaults to 1:n
#     #' @param set logical flag for duplicates
#     #' @param repeats.allowed logical flag
#     #' @noRd
#     
#     if (mode(n) != "numeric" || length(n) != 1 || n < 1 ||
#         (n %% 1) != 0) {
#       stop("bad value of n")
#     }
#     if (mode(r) != "numeric" || length(r) != 1 || r < 1 ||
#         (r %% 1) != 0) {
#       stop("bad value of r")
#     }
#     if (!is.atomic(v) || length(v) < n) {
#       stop("v is either non-atomic or too short")
#     }
#     if ((r > n) & repeats.allowed == FALSE) {
#       stop("r > n and repeats.allowed=FALSE")
#     }
#     if (set) {
#       v <- unique(sort(v))
#       if (length(v) < n) {
#         stop("too few different elements")
#       }
#     }
#     v0 <- vector(mode(v), 0)
#     if (repeats.allowed) {
#       sub <- function(n, r, v) {
#         if (r == 1) {
#           matrix(v, n, 1)
#         } else if (n == 1) {
#           matrix(v, 1, r)
#         } else {
#           inner <- Recall(n, r - 1, v)
#           cbind(
#             rep(v, rep(nrow(inner), n)),
#             matrix(
#               t(inner),
#               ncol = ncol(inner),
#               nrow = nrow(inner) * n,
#               byrow = TRUE
#             )
#           )
#         }
#       }
#     } else {
#       sub <- function(n, r, v) {
#         if (r == 1) {
#           matrix(v, n, 1)
#         } else if (n == 1) {
#           matrix(v, 1, r)
#         } else {
#           X <- NULL
#           for (i in 1:n) {
#             X <- rbind(X, cbind(v[i], Recall(n -
#                                                1, r - 1, v[-i])))
#           }
#           X
#         }
#       }
#     }
#     sub(n, r, v[1:n])
#   }

setBoottest_engine <- function(engine) {
  
  #' Sets the default bootstrap algo for the current R session 
  #' to be run via `boottest()` and `mboottest()`
  #' 
  #' @param engine Character scalar. Either 'R' or 'WildBootTests.jl'.
  #'  Default is 'R'
  #' @return No return value
  #' 
  #' @export
  #' 
  #' @examples
  #' \dontrun{
  #' setBoottest_engine(engine = "R")
  #' setBoottest_engine(engine = "WildBootTests.jl")
  #' }
  
  if (missing(engine) || is.null(engine)) {
    # New default: one cores used
    engine <- "R"
  }
  
  engine <- set_engine(engine)
  
  options("boottest_engine" = engine)
  
  invisible()
}

getBoottest_engine <- function() {
  #' get the bootstrap algorithm to be run via `boottest()` and `waldboottest()`
  #' @return The number of threads currently used by boottest as set in options
  #' @noRd
  
  x <- getOption("boottest_engine")
  if (!(x %in% c("R", "WildBootTests.jl"))) {
    stop(
      "The value of getOption(\"boottest_engine\") is currently not legal.
      Please use function setBoottest_engine to set it to an appropriate
      value. "
    )
  }
  x
}

set_engine <- function(engine) {
  
  #' check the bootstrap algo
  #' @param engine character scalar
  #' @noRd
  
  dreamerr::check_value(engine, "charin(R, WildBootTests.jl)")
  
  engine
}


# The original source of all three included funcitons is Laurent Berge's fixest
# code in https://github.com/lrberge/fixest
# The original code was distributed under GPL-3 license


# changes to these functions by Alexander Fischer
# 1. functions are renamed to _Boottest_
# 2. the default number of threads in check_set_nthreads is set to 1

setBoottest_nthreads <- function(nthreads) {
  
  #' Set the number of threads for use with open mp via options
  #' By default, only one thread is used
  #' @param nthreads Integer. Number of threads to be used
  #' @return No return value
  #' @noRd
  #' @importFrom parallel detectCores
  
  max_CRAN <- as.numeric(Sys.getenv("OMP_THREAD_LIMIT"))
  max_CRAN[is.na(max_CRAN)] <- 1000
  
  max_threads <- min(cpp_get_nb_threads(), 1000, max_CRAN)
  # we cap at 1k nthreads
  
  if (missing(nthreads) || is.null(nthreads)) {
    # New default: one cores used
    nthreads <- 1
  }
  
  nthreads <- check_set_nthreads(nthreads)
  
  options("boottest_nthreads" = nthreads)
  
  invisible()
}


getBoottest_nthreads <- function() {
  #' get the number of threads for use with open mp
  #' @return The number of threads currently used by boottest as set in options
  #' @noRd
  
  x <- getOption("boottest_nthreads")
  if (length(x) != 1 || !is.numeric(x) || is.na(x) || x %% 1 != 0 || x < 0) {
    stop("The value of getOption(\"boottest_nthreads\") is currently not legal.
         Please use function setBoottest_nthreads to set it to an appropriate
         value. ")
  }
  # cat("getBoottest nr threads \n")
  # print(x)
  x
}

check_set_nthreads <- function(nthreads) {
  #' Simple function that checks that the nber of threads is valid
  #' @param nthreads Integer. Number of threads to be used
  #' @importFrom dreamerr set_up check_value warn_up
  #' @return Integer. The number of threads to be used.
  #' @noRd
  
  
  dreamerr::set_up(1)
  
  dreamerr::check_value(nthreads,
                        "integer scalar GE{0} | numeric scalar GT{0} LT{1}",
                        .message = paste0(
                          "The argument 'nthreads' must be an integer
                          lower or equal to the number of threads available (",
                          max(cpp_get_nb_threads(), 1), ").
                          It can be equal to 0 which means all threads.
                          Alternatively, if equal to a number strictly between
                          0 and 1, it represents the fraction of all
                          threads to be used."
                        )
  )
  
  # max_threads <- parallel::detectCores()
  max_threads <- cpp_get_nb_threads()
  # cat("max_threads \n")
  # print(max_threads)
  
  # # To add later
  # if(cpp_is_in_fork()) return(1)
  
  if (nthreads == 0) {
    nthreads <- max(max_threads, 1)
  } else if (nthreads < 1) {
    nthreads <- max(ceiling(max_threads * nthreads), 1)
  } else if (nthreads > 1) {
    if (max_threads == 0) {
      dreamerr::warn_up(
        "OpenMP not detected: cannot use ", nthreads,
        " threads, single-threaded mode instead."
      )
      nthreads <- 1
    } else if (nthreads > max_threads) {
      dreamerr::warn_up(
        "Asked for ", nthreads,
        " threads while the maximum is ",
        max_threads,
        ". Set to ",
        max_threads,
        " threads instead."
      )
      nthreads <- max_threads
    }
  }
  # cat("nthreads set_get \n")
  # print(nthreads)
  nthreads
}

get_seed <- function() {
  #' creates an integer based on the global random seed set via set.seed()
  #' for using set.seed() for controlling rcpp's seed, see this
  #' blog post http://rorynolan.rbind.io/2018/09/30/rcsetseed/
  #' @noRd
  
  # max_int <- .Machine$integer.max
  max_int <- 2147483647L
  x <- sample.int(max_int, 1)
  x
}

set_seed <- function(seed, engine, type) {
  
  #' @importFrom JuliaConnectoR juliaEval
  #' @noRd
  
  if (!is.null(seed)) {
    if (engine %in% c("R", "WCR33", "WCR13", "WCU33", "WCU13", 
                         "WCR31", "WCR11", "WCU31", "WCU11")) {
      if (type %in% c("rademacher", "webb", "norm")) {
        dqrng::dqset.seed(seed)
        internal_seed <- NULL
      } else if (type == "mammen") {
        set.seed(seed)
        internal_seed <- NULL
      }
    } else if (engine == "R-lean") {
      set.seed(seed)
      internal_seed <- NULL
    } else if (engine == "WildBootTests.jl") {
      JuliaConnectoR::juliaEval("using StableRNGs")
      set.seed(seed)
      seed <- get_seed()
      internal_seed <-
        JuliaConnectoR::juliaEval(paste0("rng = StableRNG(", seed, ")"))
    }
  } else if (is.null(seed)) {
    if (engine == "WildBootTests.jl") {
      seed <- get_seed()
      JuliaConnectoR::juliaEval("using StableRNGs")
      internal_seed <-
        JuliaConnectoR::juliaEval(paste0("rng = StableRNG(", seed, ")"))
    } else {
      internal_seed <- NULL
    }
  }
  
  internal_seed
}


to_integer <- function(vec) {
  
  #' Transform vectors of all types safely to integer vectors
  #' @param vec A vector
  #' @return An integer vector
  #' @noRd
  
  dreamerr::check_arg(vec, "MBT vector")
  
  unique_vec <- unique(vec)
  int_vec <- rep(NA, length(vec))
  for (x in seq_along(unique_vec)) {
    int_vec[which(vec == unique_vec[x])] <- x
  }
  as.integer(int_vec)

}


# functions taken from 'clubSandwich' package
matrix_split <- function (x, fac, dim) {
  # if (is.vector(x)) {
  #   if (dim != "both") 
  #     stop(paste0("Object must be a matrix in order to subset by ", 
  #                 dim, "."))
  #   x_list <- split(x, fac)
  #   lapply(x_list, function(x) diag(x, nrow = length(x)))
  # }
  # else {
    lapply(levels(fac), sub_f(x, fac, dim))
  #}
}

sub_f <- function (x, fac, dim){
  function(f) switch(dim, 
                     row = x[fac == f, , drop = FALSE], 
                     col = x[, fac == f, drop = FALSE],
                     both = x[fac == f, 
                     fac == f, drop = FALSE])
}



#' reformat a vector by a 'by' group
#' @param x A vector
#' @param group_id a grouping vector of integers
#' @return 
#' A matrix of dimension N x G, where G the number of obs and 
#' G the number of groups
#' @examples 
#' if(requireNamespace("collapse")){
#' N <- 100
#' cluster <- sample(letters, N, TRUE)
#' g <- collapse::GRP(cluster, call = FALSE)
#' vec2mat(x = rnorm(N), group_id = g$group.id)
#' }
#' @noRd
vec2mat <- function(x, group_id){
  
  N <- length(x)
  G <- length(unique(group_id))
  mat <- matrix(0, N, G)
  index <- 1:N
  idx <- index + N * (group_id - 1)
  mat[idx] <- x 
  mat
  
}


find_proglang <- function(lang){
  
  #' Check if julia or python are installed / 
  #' can be found on the PATH. 
  #' 
  #' Based on Mauro Lepore's great suggestion
  #' https://github.com/ropensci/software-review/issues/546#issuecomment-1416728843
  #' 
  #' @param lang which language to check. Either 'julia' or 'python'
  #' 
  #' @return logical. TRUE if lang is found on path, FALSE if not
  #' 
  #' @examples
  #' 
  #' \dontrun{
  #' find_proglang(lang = "julia")
  #' }
  
  dreamerr::check_arg(lang, "charin(julia, python)")
  
  language_found <- nzchar(Sys.which(lang))

  language_found
  
}

