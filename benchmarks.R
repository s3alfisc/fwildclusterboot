library(rbenchmark)
library(microbenchmark)
library(boot)
library(fwildclusterboot)
library(fixest)
library(multiwayvcov)
options(boot.ncpus = 4)

seed <- 123
set.seed(seed)
voters <- create_data_1(N = 10000, N_G = 40, icc = 0.01)

benchmark_boottest <- function(b, times){
  lm_fit <- lm(proposition_vote ~ treatment + ideology + log_income + Q1_immigration , weights = NULL, data = voters)
  feols_fit <- feols(proposition_vote ~ treatment + ideology + log_income , fixef = c("Q1_immigration"), weights = NULL, data = voters)
  bench <- 
    microbenchmark(
      #multiway = cluster.boot(lm_fit, cluster = voters$group_id, parallel = FALSE, R = b, wild_type = "rademacher"),
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

bench_boottest <- benchmark_boottest(b = 10000, times = 10)
bench_boottest_plot <- boxplot(bench_boottest, log = FALSE)
bench_boottest_plot

png(file="C:/Users/alexa/Dropbox/fwildclusterboot/benchmarks/bench_boottest.png",
    width=5, height=2)
bench_boottest_plot <- boxplot(bench_boottest, log = FALSE)
dev.off()



