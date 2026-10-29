test_that("bootstrap output and RNG state do not depend on thread count", {
  skip_if(fwildclusterboot:::cpp_get_nb_threads() < 2,
          "At least two OpenMP threads are required")
  limit <- suppressWarnings(as.integer(Sys.getenv("OMP_THREAD_LIMIT")))
  skip_if(!is.na(limit) && limit < 2, "OpenMP thread limit is less than two")

  set.seed(42)
  d <- data.frame(x = rnorm(140), g = rep(1:7, each = 20))
  d$y <- d$x + rnorm(140)
  fit <- lm(y ~ x, d)
  cases <- list()
  for (type in c("rademacher", "webb")) {
    for (bt in c("11", "21", "31")) {
      cases[[paste("HC", type, bt)]] <-
        list(type = type, bootstrap_type = bt, engine = "R")
    }
    cases[[paste("CL", type)]] <- list(type = type, clustid = "g")
  }
  cases$enumerated <- list(type = "rademacher", clustid = "g", B = 999)

  # 67 sampled draws cross a batch boundary and leave a partial final batch;
  # 7 clusters give 128 fully enumerated draws across multiple batches.
  for (args in cases) {
    run <- function(threads) {
      set.seed(987)
      dqrng::dqset.seed(654)
      call <- modifyList(list(object = fit, param = "x", B = 67,
        engine = "R-lean", conf_int = FALSE, nthreads = threads), args)
      result <- suppressWarnings(do.call(boottest, call))
      list(t_stat = result$t_stat, t_boot = result$t_boot,
           p_val = result$p_val, seed = .Random.seed)
    }
    serial <- run(1)
    expect_true(all(is.finite(c(serial$t_stat, serial$t_boot, serial$p_val))))
    expect_identical(run(2), serial)
    if (fwildclusterboot:::cpp_get_nb_threads() >= 4 &&
        (is.na(limit) || limit >= 4)) {
      expect_identical(run(4), serial)
    }
  }
})

test_that("large weight batches preserve the RNG sequence", {
  # This exceeds the 8 MiB staging budget for 64 draws, so the memory cap
  # rather than the maximum iteration count determines the batch boundaries.
  n <- 20000L
  B <- 67L
  x <- seq(-1, 1, length.out = n)
  X <- cbind(1, x)
  y <- x + sin(seq_len(n))
  R <- matrix(c(0, 1), ncol = 1)
  run <- function(cores) {
    set.seed(981)
    result <- fwildclusterboot:::wildboottestHC(y, X, R, 0, B, n,
      cores, 0L, (n - 1) / (n - 2), 1L)
    list(t = result$t_boot, seed = .Random.seed)
  }
  serial <- run(1L)
  set.seed(981)
  invisible(runif(n * B))
  expect_identical(serial$seed, .Random.seed)
  expect_true(all(is.finite(serial$t)))
  if (fwildclusterboot:::cpp_get_nb_threads() >= 2) {
    expect_identical(run(2L), serial)
  }
})

test_that("worker exceptions return to R without aborting the process", {
  x <- seq(-1, 1, length.out = 20)
  X <- cbind(1, x)
  y <- x + sin(seq_len(20))

  # Two restrictions cause scalar conversion to fail inside a worker, after
  # valid setup algebra. The exception must be rethrown on the calling thread.
  expect_error(fwildclusterboot:::wildboottestHC(
    y, X, diag(2), 0, 3L, 20L, 2L, 0L, 1, 1L
  ), "as_scalar")

  result <- fwildclusterboot:::wildboottestHC(
    y, X, matrix(c(0, 1), ncol = 1), 0, 3L, 20L, 2L, 0L, 1, 1L
  )
  expect_true(all(is.finite(result$t_boot)))
})
