# -------------------------------------------------------------- #
# Linear Regression Example

library(data.table)
library(magrittr)
library(broom)
library(estimatr)

N <- 1000
x <- rnorm(N)
error <- rnorm(N)
beta <- 2
y = 1 + beta * rnorm(N) + error
data <- data.table(y = y, x = x)
lm_fit <- lm_robust(y ~ x) 
lm_tidy <- lm_fit %>% tidy()

# how to get confidence interval? 
p_value <- function(beta_0, data = data){
  lm_fit <- lm(y ~ x, data) 
  lm_tidy <- tidy(lm_fit)[2, ]
  lm_t_stat <- (lm_tidy$estimate - beta_0) / lm_tidy$std.error
  2 - 2*pnorm(abs(lm_t_stat))
  #1 - pnorm(abs(lm_t_stat)) - pnorm(-abs(lm_t_stat))
}


p_value(data = data, beta_0 = conf_int_beta[1])
p_value(data = data, beta_0 = conf_int_beta[2])

# should both be around 0.05
p_value(data = data, beta_0 = lm_tidy[2,"conf.low"])
p_value(data = data, beta_0 = lm_tidy[2,"conf.high"])

# starting value: estimate +/- 3 times standard dev (does not really make sense as st dev unknown)
starting_vals <- as.numeric(lm_tidy[2, "estimate"] + c(-3,3) * lm_tidy[2, "std.error"])

p_value(data = data, beta_0 = as.numeric(starting_vals[1])) < 0.05
p_value(data = data, beta_0 = as.numeric(starting_vals[2])) < 0.05

ind_conf_int <- function(data = data, starting_vals){
  
  
  test_vals <- seq(starting_vals[1], starting_vals[2], (starting_vals[2] - starting_vals[1])/ 25)      
  p_val <- vector("numeric", length(test_vals))
  for(i in 1:length(test_vals)){
    
    p_val[i] <- p_value(data = data, beta_0 = test_vals[i])
    
  }
  
  
  #for(x in c(test_vals_more, test_vals_lower)){
  p_val_newtonRaphson <- function(test_value, data1 = data){
    p_value(data = data1, beta_0 = test_value) - 0.05
  }
  
  crossings <-  (p_val < 0.05) - (p_val > 0.05)
  x_crossings <- rep(NA, 25)
  for(i in 1:25){
    x_crossings[i] <- ifelse(crossings[i] + crossings[i + 1] == 0 || crossings[i] + crossings[i - 1] == 0, 1, 0)
  }
  p_val[which(x_crossings == 1)]
  test_vals[which(x_crossings == 1)]
  
  test_vals_higher <- (test_vals[which(x_crossings == 1)])[3:4]  
  test_vals_higher_max <- test_vals_higher[which.max(c(abs(test_vals_higher - 0.05), abs(test_vals_higher[2] - 0.05)))]
  test_vals_lower <- (test_vals[which(x_crossings == 1)])[1:2]  
  test_vals_lower_max <- test_vals_lower[which.max(c(abs(test_vals_lower - 0.05), abs(test_vals_lower[2] - 0.05)))]
  
  res <- lapply(list(test_vals_lower_max, test_vals_higher_max), function(x){
    tmp <- pracma::newtonRaphson(p_val_newtonRaphson, x0 =  x, dfun = NULL, maxiter = 500, tol = 1e-12)
    tmp$root
  })
  
  conf_int <- unlist(res)
 
  conf_int
  
}

summary(lm_fit)

ind_conf_int(data = data, starting_vals = starting_vals)



