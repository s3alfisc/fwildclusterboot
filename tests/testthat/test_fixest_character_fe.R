test_that("boottest works with character fixed effects in fixest models (#161)", {
  # get_cluster() used to rebuild a model frame from `y ~ 1 | fe1 + fe2`;
  # stats::model.frame() then evaluated `1 | fe1 + fe2` as an expression, which
  # fails for character fixed effects ("non-numeric argument to binary
  # operator") and only warns for factors.
  set.seed(42)
  N <- 1000
  data_chr <<- data.frame(
    y = rnorm(N),
    x = rnorm(N),
    unit = paste0("u", sample(1:40, N, TRUE)),
    year = as.character(sample(2010:2019, N, TRUE)),
    clustid = sample(1:25, N, TRUE)
  )
  data_chr$unit[c(3, 17, 250)] <- NA # dropped by feols
  data_fct <<- data_chr
  data_fct$unit <- factor(data_fct$unit)
  data_fct$year <- factor(data_fct$year)

  fit_chr <- fixest::feols(y ~ x | unit + year, data = data_chr)
  fit_fct <- fixest::feols(y ~ x | unit + year, data = data_fct)
  fit_chr_sub <- fixest::feols(y ~ x | unit + year, data = data_chr, subset = ~ clustid > 3)
  fit_fct_sub <- fixest::feols(y ~ x | unit + year, data = data_fct, subset = ~ clustid > 3)

  boot <- function(fit, ...) {
    dqrng::dqset.seed(1)
    boottest(fit, param = "x", clustid = "clustid", B = 999, ...)
  }
  for (fe in list(NULL, "unit")) {
    b_chr <- boot(fit_chr, fe = fe)
    b_fct <- boot(fit_fct, fe = fe)
    expect_equal(b_chr$t_stat, b_fct$t_stat)
    expect_equal(b_chr$p_val, b_fct$p_val)
    expect_equal(nobs(fit_chr), b_chr$N)

    b_chr_sub <- boot(fit_chr_sub, fe = fe)
    b_fct_sub <- boot(fit_fct_sub, fe = fe)
    expect_equal(b_chr_sub$t_stat, b_fct_sub$t_stat)
    expect_equal(nobs(fit_chr_sub), b_chr_sub$N)
  }
})
