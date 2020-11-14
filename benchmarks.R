library(rbenchmark)
library(microbenchmark)
library(boot)
library(fwildclusterboot)
library(fixest)
library(multiwayvcov)
library(profvis)

#profvis::profvis(boottest(feols_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE, demean = TRUE)
#)

options(boot.ncpus = 4)

seed <- 123
set.seed(seed)
voters <- create_data_1(N = 10000, N_G = 40, icc = 0.01)
B <- 10000

lm_fit <- lm(proposition_vote ~ treatment + ideology + log_income + Q1_immigration , weights = NULL, data = voters)
feols_fit <- feols(proposition_vote ~ treatment + ideology + log_income , fixef = c("Q1_immigration"), weights = NULL, data = voters)

bench <- 
    microbenchmark(
      #multiway = cluster.boot(lm_fit, cluster = voters$group_id, parallel = FALSE, R = B, wild_type = "rademacher"),
      multiway_parallel = cluster.boot(lm_fit, cluster = voters$group_id, parallel = TRUE, R = B, wild_type = "rademacher"),
      boottest_lm = boottest(lm_fit, clustid = voters$group_id, B = B, param = "treatment"), 
      boottest_feols = boottest(feols_fit, clustid = voters$group_id, B = B, param = "treatment"),
      #boottest_lm_p_val = boottest(lm_fit, clustid = voters$group_id, B = b, param = "treatment", conf_int = FALSE), 
      #boottest_feols_p_val = boottest(feols_fit, clustid = voters$group_id, B = b, param = "treatment", conf_int = FALSE),
      times = 10
)

bench

saveRDS(bench, "C:/Users/alexa/Dropbox/fwildclusterboot/benchmarks/bench_boottest.rds")
# set.seed(768)
# 
# bench_boottest <- benchmark_boottest(b = 10000, times = 10)
# bench_boottest_plot <- boxplot(bench_boottest, log = FALSE)
# bench_boottest_plot
# 
# png(file="C:/Users/alexa/Dropbox/fwildclusterboot/benchmarks/bench_boottest.png", 
#     width = 400, height = 350)
# bench_boottest_plot <- boxplot(bench_boottest, log = FALSE)
# dev.off()



