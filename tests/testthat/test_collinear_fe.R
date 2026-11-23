test_that("boottest works when the added fixed effects are collinear with the projected one", {
  # Units nested in countries, with unit and country-by-year fixed effects: the
  # country-by-year dummies added to X, minus a single reference level, are
  # collinear with the unit fixed effect, so crossprod(X) was singular
  # ("system is computationally singular") both with fe = "unit" (projected
  # out) and with fe = NULL (all dummies added).
  set.seed(7)
  units <- 60
  years <- 2014:2021
  data_nested <<- expand.grid(unit = seq_len(units), year = years)
  data_nested$country <- (data_nested$unit - 1) %/% 20 + 1
  data_nested$cy <- factor(paste(data_nested$country, data_nested$year))
  data_nested$unit <- factor(data_nested$unit)
  data_nested$clustid <- (as.integer(data_nested$unit) - 1) %/% 3 + 1
  data_nested$treat <- as.integer(data_nested$clustid %% 3 == 0 & data_nested$year >= 2018)
  data_nested$x <- rnorm(nrow(data_nested))
  data_nested$y <- 0.2 * data_nested$treat + 0.5 * data_nested$x + rnorm(nrow(data_nested))

  fit_feols <- fixest::feols(y ~ treat + x | unit + cy, data = data_nested)
  fit_lm <- lm(y ~ treat + x + unit + cy, data = data_nested)

  # without small sample corrections, which count the dummies of lm but not
  # the fixed effects of fixest (fixef.K = "none")
  boot <- function(fit, ...) {
    dqrng::dqset.seed(1)
    boottest(fit, param = "treat", clustid = "clustid", B = 999,
             ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), ...)
  }
  b_lm <- boot(fit_lm)
  b_fe <- boot(fit_feols, fe = "unit")
  b_nofe <- boot(fit_feols)

  expect_equal(unname(b_fe$point_estimate), unname(coef(fit_feols)["treat"]))
  expect_equal(b_fe$t_stat, b_lm$t_stat)
  expect_equal(b_nofe$t_stat, b_lm$t_stat)
  expect_equal(b_fe$p_val, b_lm$p_val)
  expect_equal(b_nofe$p_val, b_lm$p_val)
})
