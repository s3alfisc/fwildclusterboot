test_that("test sunab", {
  skip_on_cran()
  skip_if_not(
    find_proglang("julia"),
    message = "skip test as julia installation not found."
  )

  library(fixest)
  # Simple DiD example
  base_stagg <- fixest::base_stagg

  # The DiD estimation
  res_sunab <- fixest::feols(
    y ~ x1 + fixest:::sunab(year_treated, year) | id + year, base_stagg
  )

  res_sunab_3ref <- fixest::feols(
    y ~ x1 + fixest:::sunab(year_treated, year, ref.p = c(.F + 0:2, -1)) |
      id + year,
    cluster = "id", base_stagg,
    ssc = fixest::ssc(adj = FALSE, cluster.adj = FALSE)
  )

  fixest_att <- aggregate(res_sunab, agg = "ATT")
  fixest_ci <- as.matrix(confint(summary(res_sunab, agg = "ATT")))
  fixest_pval <- as.vector(pvalue(summary(res_sunab, agg = "ATT")))


  # test ATT equivalence
  boot_att <-
    boot_aggregate(
      res_sunab,
      B = 99999,
      agg = "ATT",
      clustid = "id",
      ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE)
    )

  expect_equal(
    fixest_ci["ATT", ],
    boot_att[, 3:4],
    ignore_attr = TRUE,
    tolerance = 0.03
  )

  expect_equal(
    fixest_pval[2],
    boot_att[, 2],
    ignore_attr = TRUE,
    tolerance = 0.01
  )


  # effects aggregated by year
  fixest_ci <- as.matrix(confint(summary(res_sunab, agg = TRUE)))
  fixest_pval <- as.vector(pvalue(summary(res_sunab, agg = TRUE)))

  boot_year <-
    boot_aggregate(
      res_sunab,
      B = 99999,
      agg = TRUE,
      clustid = "id",
      ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE)
    )

  expect_equal(
    fixest_ci[-1, ],
    boot_year[, 3:4],
    ignore_attr = TRUE,
    tolerance = 0.05
  )

  expect_equal(
    fixest_pval[-1],
    boot_year[, 2],
    ignore_attr = TRUE,
    tolerance = 0.05
  )
})
