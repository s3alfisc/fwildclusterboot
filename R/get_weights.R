get_weights <- function(type,
                        full_enumeration,
                        N_G_bootcluster,
                        boot_iter,
                        sampling) {
  #' draw bootstrap weights
  #' @param type the type of the weights distribution. Either 'rademacher',
  #' 'mammen', 'norm' or 'webb
  #' @param full_enumeration Logical. Should (deterministic) full enumeration be
  #' employed
  #' @param N_G_bootcluster Integer. The number of bootstrap clusters
  #' @param boot_iter The number of bootstrap iterations
  #' @param sampling 'fast' or 'standard'. If 'fast', the 'dqrng' package is used
  #' for random number generation. If 'standard', functions from the 'stats'
  #' package are used when available. This argument is mostly a convenience for
  #' a wrapper package around fwildclusterboot, wildrwolf. I recommend to use the
  #' 'fast' option.
  #' @return A matrix of dimension N_G_bootcluster x (boot_iter + 1)
  #' @importFrom gtools permutations
  #' @noRd


  if (sampling == "dqrng") {
    wild_draw_fun <- switch(type,
      # note: for randemacher, create integer matrix
      # (uses less memory
      # than numeric)
      rademacher = function(n) {
        dqrng::dqrrademacher(n)
      },
      mammen = function(n) {
        sample(
          c(-1, 1) * (sqrt(5) + c(-1, 1)) / 2,
          n,
          replace = TRUE,
          prob = (sqrt(5) + c(1, -1)) / (2 * sqrt(5))
        )
      },
      norm = function(n) {
        dqrng::dqrnorm(n = n)
      },
      webb = function(n) {
        dqrng::dqsample(
          x = c(-sqrt((3:1) / 2), sqrt((1:3) / 2)),
          size = n,
          replace = TRUE
        )
      },
      wild_draw_fun
    )
  } else if (sampling == "standard") {
    wild_draw_fun <- switch(type,
      # note: for randemacher, create integer matrix
      # (uses less memory
      # than numeric)
      rademacher = function(n) {
        sample(
          x = c(-1L, 1L),
          size = n,
          replace = TRUE
        )
      },
      mammen = function(n) {
        sample(
          c(-1, 1) * (sqrt(5) + c(-1, 1)) / 2,
          n,
          replace = TRUE,
          prob = (sqrt(5) + c(1, -1)) / (2 * sqrt(5))
        )
      },
      norm = function(n) {
        rnorm(n = n)
      },
      webb = function(n) {
        sample(
          x = c(-sqrt((3:1) / 2), sqrt((1:3) / 2)),
          size = n,
          replace = TRUE
        )
      },
      wild_draw_fun
    )
  }

  # do full enumeration for rademacher weights if bootstrap iterations
  # B exceed number of possible permutations else random sampling

  # full_enumeration only for rademacher weights (set earlier)
  if (full_enumeration) {
    v0 <-
      # gtools_permutations(
      permutations(
        n = 2,
        r = N_G_bootcluster,
        v = c(1, -1),
        repeats.allowed = TRUE
      )
    v <- cbind(1, t(v0))
  } else {
    # else: just draw with replacement - by chance, some permutations
    # might occur more than once
    v <- wild_draw_fun(n = N_G_bootcluster * (boot_iter + 1))
    dim(v) <- c(N_G_bootcluster, boot_iter + 1)
    v[, 1] <- 1
  }

  v
}
