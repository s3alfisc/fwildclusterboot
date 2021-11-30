# test if reported bootstrap t-statistic is equal to fixest t-statistic with 
# dof = dof(adj = FALSE, cluster.adj = TRUE) -> this is the type of "small 
# sample adjustment" employed by fwildclusterboot::boottest
# see fixest vignette: https://cran.r-project.org/web/packages/fixest/vignettes/standard_errors.html

runThisTest <- FALSE

if (runThisTest) {

library(plm)
library(fixest)
  
data(EmplUK)
EmplUK$firm <- as.factor(EmplUK$firm)

feols_fit <- fixest::feols(emp ~ wage + capital + output + as.factor(firm) + as.factor(year), data = EmplUK, 
                           cluster = ~firm, 
                           ssc = ssc(adj = FALSE,cluster.adj = TRUE))
lm_fit <- lm(emp ~ wage + capital + output + as.factor(firm) + as.factor(year), data = EmplUK)

dof_tstat <- fixest::coeftable(feols_fit)[c("wage", "capital", "output"), 3]

B <- 999
y <- 1
boot1 <- boottest(lm_fit, clustid = c("firm"), B = B, param = "wage", nthreads = 1, seed = y, impose_null = FALSE)
boot2 <- boottest(lm_fit, clustid = c("firm"), B = B, param = "capital", nthreads = 1, seed = y, impose_null = FALSE)
boot3 <- boottest(lm_fit, clustid = c("firm"), B = B, param = "output", nthreads = 1, seed = y, impose_null = FALSE)

# boottest returns absolute values of t-stats

expect_equivalent(abs(boot1$t_stat), abs(dof_tstat[1]))
expect_equivalent(abs(boot2$t_stat), abs(dof_tstat[2]))
expect_equivalent(abs(boot3$t_stat), abs(dof_tstat[3]))

# note that these results should be independent of the number of 
# bootstrap draws - in the "first" ob B + 1 bootstrap draws, all weights are set to 1

B <- 123
y <- 2
boot1 <- boottest(lm_fit, clustid = c("firm"), B = B, param = "wage", nthreads = 1, seed = y, impose_null = FALSE)
boot2 <- boottest(lm_fit, clustid = c("firm"), B = B, param = "capital", nthreads = 1, seed = y, impose_null = FALSE)
boot3 <- boottest(lm_fit, clustid = c("firm"), B = B, param = "output", nthreads = 1, seed = y, impose_null = FALSE)

expect_equivalent(abs(boot1$t_stat), abs(dof_tstat[1]))
expect_equivalent(abs(boot2$t_stat), abs(dof_tstat[2]))
expect_equivalent(abs(boot3$t_stat), abs(dof_tstat[3]))

# twoway clustering 

feols_fit <- fixest::feols(emp ~ wage + capital + output, data = EmplUK, 
                           cluster = ~firm + year, 
                           ssc = ssc(adj = FALSE,
                                     cluster.adj = TRUE, 
                                     cluster.df = "conventional"))

lm_fit <- lm(emp ~ wage + capital + output, data = EmplUK)
dof_tstat <- fixest::coeftable(feols_fit)[c("wage", "capital", "output"), 3]


B <- 123
y <- 2
boot1 <- boottest(lm_fit, clustid = c("firm", "year"), B = B, param = "wage", nthreads = 1, seed = y, impose_null = FALSE)
boot2 <- boottest(lm_fit, clustid = c("firm", "year"), B = B, param = "capital", nthreads = 1, seed = y, impose_null = FALSE)
boot3 <- boottest(lm_fit, clustid = c("firm", "year"), B = B, param = "output", nthreads = 1, seed = y, impose_null = FALSE)

expect_equivalent(abs(boot1$t_stat), abs(dof_tstat[1]))
expect_equivalent(abs(boot2$t_stat), abs(dof_tstat[2]))
expect_equivalent(abs(boot3$t_stat), abs(dof_tstat[3]))

# twoway and 'min' 

# feols_fit <- fixest::feols(emp ~ wage + capital + output, data = EmplUK, 
#                            cluster = ~firm + year, 
#                            ssc = ssc(adj = FALSE,
#                                      cluster.adj = TRUE, 
#                                      cluster.df = "min"))
# 
# lm_fit <- lm(emp ~ wage + capital + output, data = EmplUK)
# dof_tstat <- fixest::coeftable(feols_fit)[c("wage", "capital", "output"), 3]
# 
# 
# B <- 123
# y <- 2
# boot1 <- boottest(lm_fit, clustid = c("firm", "year"), B = B, param = "wage", nthreads = 1, seed = y, impose_null = FALSE, bootcluster = "min")
# boot2 <- boottest(lm_fit, clustid = c("firm", "year"), B = B, param = "capital", nthreads = 1, seed = y, impose_null = FALSE, bootcluster = "min")
# boot3 <- boottest(lm_fit, clustid = c("firm", "year"), B = B, param = "output", nthreads = 1, seed = y, impose_null = FALSE, bootcluster = "min")
# 
# expect_equivalent(abs(boot1$t_stat), abs(dof_tstat[1]))
# expect_equivalent(abs(boot2$t_stat), abs(dof_tstat[2]))
# expect_equivalent(abs(boot3$t_stat), abs(dof_tstat[3]))


}