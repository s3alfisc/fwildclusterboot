test_that("test plm method", {
  
  
  library(fwildclusterboot)
  library("plm")
  data("Grunfeld", package="plm")
  
  #Grunfeld <- pdata.frame(Grunfeld, index=c("firm","year"), drop.index=TRUE, row.names=TRUE)
  #head(Grunfeld)
  
  
  # -------------------------------------------------------------- # 
  # within estimation 
  
  # model = within, effect = individual 
  
  plm_fit <- plm(
    inv~value+capital,
    data = Grunfeld, 
    model = "within", 
    effect = "individual"
  )
  broom::tidy(plm_fit)
  lm_fit <- lm(inv~value+capital + as.factor(firm), data = Grunfeld)
  broom::tidy(lm_fit)[c(1, 2, 3),]
  
  dqrng::dqset.seed(233)
  lm_boot <- boottest(
    lm_fit,
    param = "value", 
    clustid = "firm", 
    B = 999, 
    ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE)
  )
  
  dqrng::dqset.seed(233)
  plm_boot <- boottest(
    plm_fit,
    param = "value", 
    clustid = "firm", 
    B = 999,
    ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE)
  )
  
  expect_equal(coef(lm_fit)[c("value", "capital")], coef(plm_fit))
  expect_equal(pval(lm_boot), pval(plm_boot))
  expect_equal(teststat(lm_boot), teststat(plm_boot))
  expect_equal(confint(lm_boot), confint(plm_boot))
  
  dqrng::dqset.seed(233)
  plm_boot <- boottest(
    plm_fit,
    param = "value", 
    clustid = "firm", 
    fe = "firm",
    B = 999,
    ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE)
  )
  
  expect_equal(coef(lm_fit)[c("value", "capital")], coef(plm_fit))
  expect_equal(pval(lm_boot), pval(plm_boot))
  expect_equal(teststat(lm_boot), teststat(plm_boot))
  expect_equal(confint(lm_boot), confint(plm_boot))
  
  
  # model = within, effect = time
  
  plm_fit <- plm(
    inv~value+capital,
    data = Grunfeld, 
    model = "within", 
    effect = "time"
  )
  
  broom::tidy(plm_fit)
  lm_fit <- lm(inv ~ value + capital + as.factor(year), data = Grunfeld)
  broom::tidy(lm_fit)[c(1, 2, 3),]
  
  dqrng::dqset.seed(233)
  lm_boot <- boottest(
    lm_fit,
    param = "value", 
    clustid = "firm", 
    B = 999, 
    ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE)
  )
  
  dqrng::dqset.seed(233)
  plm_boot <- boottest(
    plm_fit,
    param = "value", 
    clustid = "firm", 
    B = 999,
    ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE)
  )
  
  expect_equal(coef(lm_fit)[c("value", "capital")], coef(plm_fit))
  expect_equal(pval(lm_boot), pval(plm_boot))
  expect_equal(teststat(lm_boot), teststat(plm_boot))
  expect_equal(confint(lm_boot), confint(plm_boot))
  
  dqrng::dqset.seed(233)
  plm_boot <- boottest(
    plm_fit,
    param = "value", 
    clustid = "firm", 
    fe = "year",
    B = 999,
    ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE)
  )
  
  expect_equal(coef(lm_fit)[c("value", "capital")], coef(plm_fit))
  expect_equal(pval(lm_boot), pval(plm_boot))
  expect_equal(teststat(lm_boot), teststat(plm_boot))
  expect_equal(confint(lm_boot), confint(plm_boot))
  
  # model = within, effect = "twoway"
  
  plm_fit <- plm(
    inv~value+capital,
    data = Grunfeld, 
    model = "within", 
    effect = "twoway"
  )
  
  broom::tidy(plm_fit)
  lm_fit <- lm(inv ~ value + capital + as.factor(firm) + as.factor(year), data = Grunfeld)
  broom::tidy(lm_fit)[c(1, 2, 3),]
  
  dqrng::dqset.seed(233)
  lm_boot <- boottest(
    lm_fit,
    param = "value", 
    clustid = "firm", 
    B = 999, 
    ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE)
  )
  
  dqrng::dqset.seed(233)
  plm_boot <- boottest(
    plm_fit,
    param = "value", 
    clustid = "firm", 
    B = 999,
    ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE)
  )
  
  expect_equal(coef(lm_fit)[c("value", "capital")], coef(plm_fit))
  expect_equal(pval(lm_boot), pval(plm_boot))
  expect_equal(teststat(lm_boot), teststat(plm_boot))
  expect_equal(confint(lm_boot), confint(plm_boot))
  
  dqrng::dqset.seed(233)
  plm_boot <- boottest(
    plm_fit,
    param = "value", 
    clustid = "firm", 
    fe = "year",
    B = 999,
    ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE)
  )
  
  expect_equal(coef(lm_fit)[c("value", "capital")], coef(plm_fit))
  expect_equal(pval(lm_boot), pval(plm_boot))
  expect_equal(teststat(lm_boot), teststat(plm_boot))
  expect_equal(confint(lm_boot), confint(plm_boot))
  
  
  # -------------------------------------------------------------- # 
  # pooling estimation 
  
  plm_fit <- plm(inv~value+capital, data = Grunfeld, model = "pooling")
  summary(plm_fit)
  lm_fit <- lm(inv~value+capital , data = Grunfeld)
  summary(lm_fit)
  
  dqrng::dqset.seed(233)
  lm_boot <- boottest(
    lm_fit,
    param = "value", 
    clustid = c("year", "firm"), 
    B = 999, 
    ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE)
  )
  
  dqrng::dqset.seed(233)
  plm_boot <- boottest(
    plm_fit,
    param = "value", 
    clustid = c("year", "firm"), 
    B = 999,
    ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE)
  )
  
  expect_equal(coef(lm_fit)[c("(Intercept)","value", "capital")], coef(plm_fit))
  expect_equal(pval(lm_boot), pval(plm_boot))
  expect_equal(teststat(lm_boot), teststat(plm_boot))
  expect_equal(confint(lm_boot), confint(plm_boot))
  

  
  
  
  
  
  
  
})
