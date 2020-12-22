library(rbenchmark)
library(microbenchmark)
library(fwildclusterboot)
library(fixest)
library(lfe)
library(sandwich)

# benchmark 1

seed <- 123
set.seed(seed)
voters <- create_data_2(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 20, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = seed)
B <- 10000

lm_fit <- lm(proposition_vote ~ treatment  + log_income + Q1_immigration , weights = NULL, data = voters)
feols_fit <- feols(proposition_vote ~ treatment  + log_income , fixef = c("Q1_immigration"), weights = NULL, data = voters)
felm_fit <- felm(proposition_vote ~ treatment  + log_income | Q1_immigration | 0 | 0 , data = voters)


bench_oneway_1 <- 
    microbenchmark(
      sw_1 = vcovBS(lm_fit, cluster = voters$group_id1, cores = 1, R = B, wild_type = "rademacher"),
      sw_2 = vcovBS(lm_fit, cluster = voters$group_id1, cores = 2, R = B, wild_type = "rademacher"),
      sw_3 = vcovBS(lm_fit, cluster = voters$group_id1, cores = 4, R = B, wild_type = "rademacher"),
      fwc_1 = boottest(lm_fit, clustid = "group_id1", B = B, param = "treatment"), 
      # fwc_2 = boottest(feols_fit, clustid = "group_id1", B = B, param = "treatment"),
      # fwc_3 = boottest(felm_fit, clustid = "group_id1", B = B, param = "treatment"),
      times = 1
)

plot_bench_oneway_1 <- 
boxplot(bench_oneway_1, unit = "s", log = TRUE)


# benchmark 2

seed <- 123
set.seed(seed)
voters <- create_data_2(N = 10000, N_G1 = 60, icc1 = 0.01, N_G2 = 20, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = seed)
B <- 10000

lm_fit <- lm(proposition_vote ~ treatment  + log_income + Q1_immigration , weights = NULL, data = voters)
feols_fit <- feols(proposition_vote ~ treatment  + log_income , fixef = c("Q1_immigration"), weights = NULL, data = voters)
felm_fit <- felm(proposition_vote ~ treatment  + log_income | Q1_immigration | 0 | 0 , data = voters)

bench_oneway_2 <- 
  microbenchmark(
    sw_1 = vcovBS(lm_fit, cluster = voters$group_id1, cores = 1, R = B, wild_type = "rademacher"),
    sw_2 = vcovBS(lm_fit, cluster = voters$group_id1, cores = 2, R = B, wild_type = "rademacher"),
    sw_3 = vcovBS(lm_fit, cluster = voters$group_id1, cores = 4, R = B, wild_type = "rademacher"),
    fwc_1 = boottest(lm_fit, clustid = "group_id1", B = B, param = "treatment"), 
    # fwc_2 = boottest(feols_fit, clustid = "group_id1", B = B, param = "treatment"),
    # fwc_3 = boottest(felm_fit, clustid = "group_id1", B = B, param = "treatment"),
    times = 1
  )

plot_bench_oneway_2 <- 
boxplot(bench_oneway_2, unit = "s", log = TRUE)


# benchmark 3: twoway

seed <- 123
set.seed(seed)
voters <- create_data_2(N = 10000, N_G1 = 30, icc1 = 0.01, N_G2 = 40, icc2 = 0.01, numb_fe1 = 40, numb_fe2 = 10, seed = seed)
B <- 10000

lm_fit <- lm(proposition_vote ~ treatment  + log_income + Q1_immigration , weights = NULL, data = voters)

bench_twoway_1 <- 
  microbenchmark(
    sw_1 = vcovBS(lm_fit, cluster = ~ group_id1 + group_id2, cores = 1, R = B, wild_type = "rademacher"),
    sw_2 = vcovBS(lm_fit, cluster = ~ group_id1 + group_id2, cores = 2, R = B, wild_type = "rademacher"),
    sw_3 = vcovBS(lm_fit, cluster = ~ group_id1 + group_id2, cores = 4, R = B, wild_type = "rademacher"),
    fwc_1 = boottest(lm_fit, clustid = c("group_id1", "group_id2"), B = B, param = "treatment"), 
    # fwc_2 = boottest(feols_fit, clustid = c("group_id1", "group_id2"), B = B, param = "treatment"),
    # fwc_3 = boottest(felm_fit, clustid = c("group_id1", "group_id2"), B = B, param = "treatment"),
    times = 1
)

plot_bench_twoway_1 <- 
boxplot(bench_twoway_1, unit = "s", log = TRUE) 


time_1 <- plot_bench_oneway_1$stats[1, ]
time_2 <- plot_bench_oneway_2$stats[1, ]

res <- 
data.frame(experiment = c(rep("Experiment 1", 4), rep("Experiment 2", 4)),
           time = c(time_1, time_2), 
           N_G = c(rep(20, 4), rep(60, 4)), 
           N = rep(10000, 8), 
           B = rep(10000, 8), 
           est = rep(c("sandwich", 
                       "sandwich, 2 clust", 
                       "sandwich, 4 clust", 
                       "boottest"), 2))

res$est <- factor(res$est, levels = c("sandwich", 
                                        "sandwich, 2 clust", 
                                        "sandwich, 4 clust", 
                                        "boottest"))


library(ggplot2)

final <- 
ggplot(aes(x = est, y = time), data = res) + 
  facet_wrap(~ experiment) + 
  geom_boxplot() + 
  scale_y_log10() + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90, hjust=1)) 
final

ggsave( "C:/Users/alexa/Dropbox/fwildclusterboot/benchmarks/bench_boottest.png")
# ---------------------------------------------------------------------- # 

saveRDS(bench, "C:/Users/alexa/Dropbox/fwildclusterboot/benchmarks/bench_boottest.rds")
# set.seed(768)
# 
# bench_boottest <- benchmark_boottest(b = 10000, times = 1)
# bench_boottest_plot <- boxplot(bench_boottest, log = FALSE)
# bench_boottest_plot
# 
# png(file="C:/Users/alexa/Dropbox/fwildclusterboot/benchmarks/bench_boottest.png", 
#     width = 400, height = 350)
# bench_boottest_plot <- boxplot(bench_boottest, log = FALSE)
# dev.off()



