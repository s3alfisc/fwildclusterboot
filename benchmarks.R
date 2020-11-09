library(rbenchmark)
library(microbenchmark)
library(boot)
library(fwildclusterboot)
library(fixest)
library(multiwayvcov)
options(boot.ncpus = 4)


benchmark_boottest <- function(n, b, n_g, times){
  voters <- create_data_1(N = n, N_G = n_g, icc = 0.01)
  lm_fit <- lm(proposition_vote ~ treatment + ideology + log_income + Q1_immigration , weights = NULL, data = voters)
  feols_fit <- feols(proposition_vote ~ treatment + ideology + log_income , fixef = c("Q1_immigration"), weights = NULL, data = voters)
  bench <- 
    microbenchmark(
      multiway = cluster.boot(lm_fit, cluster = voters$group_id, parallel = FALSE, R = b, wild_type = "rademacher"),
      multiway_parallel = cluster.boot(lm_fit, cluster = voters$group_id, parallel = TRUE, R = b, wild_type = "rademacher"),
      boottest_lm = boottest(lm_fit, clustid = voters$group_id, B = b, param = "treatment"), 
      boottest_feols = boottest(feols_fit, clustid = voters$group_id, B = b, param = "treatment"),
      #boottest_lm_p_val = boottest(lm_fit, clustid = voters$group_id, B = b, param = "treatment", conf_int = FALSE), 
      #boottest_feols_p_val = boottest(feols_fit, clustid = voters$group_id, B = b, param = "treatment", conf_int = FALSE),
      
      times = times
    )
  bench
}  

set.seed(768)

bench_boottest <- benchmark_boottest(n = 5000, b = 1000, n_g = 40, times = 10)
bench_boottest_plot <- boxplot(bench_boottest)
bench_boottest_plot

saveRDS(bench_boottest, "C:/Users/alexa/Dropbox/fwildclusterboot/benchmarks/bench_boottest.rds")



