create_data_1 <- 
  function(N, N_G, icc, numb_fe1, numb_fe2){
    
    #' Function creates data for tests and examples
    #' @import data.table
    #' @import fabricatr
    #' @param N number of observations
    #' @param N_G number of clusters
    #' @param icc intra-cluster correlation 
    #' @param numb_fe1 A scalar. Number of fixed effect for first factor variable
    #' @param numb_fe2 A scalar. Number of fixed effect for second factor variable
    #' @export
    
    voters <- 
      fabricatr::fabricate(
        N = N,
        group_id = rep(1:N_G, N / N_G),
        ideology = fabricatr::draw_normal_icc(mean = 0, N = N, clusters = group_id, ICC = icc),
        ideological_label = fabricatr::draw_ordered(
          x = ideology,
          break_labels = c(
            "Very Conservative", "Conservative",
            "Liberal", "Very Liberal"
          )
        ),
        income = exp(rlnorm(n = N, meanlog = 2.4 - (ideology * 0.1), sdlog = 0.12)),
        Q1_immigration = factor(sample(1:numb_fe1, N, TRUE)),
        Q2_defence = factor(sample(1:numb_fe2, N, TRUE)),
        #Q2_defence = draw_likert(x = ideology + 0.5, type = 7),
        treatment = fabricatr::draw_binary(0.5, N = N),
        proposition_vote = fabricatr::draw_binary(latent = ideology + 0.1 * treatment, link = "probit")
      )
    
    setDT(voters)
    voters[, log_income := log(income)]
    voters[, Q1_immigration := as.factor(Q1_immigration) ]
    #voters[, Q2_defence := as.factor(Q2_defence)]
  }

create_data_2 <- 
  function(N, N_G1, icc1, N_G2, icc2, numb_fe1, numb_fe2, seed){
    
    #' Function creates data for tests and examples
    #' @import data.table
    #' @import fabricatr
    #' @param N number of observations
    #' @param N_G1 A scalar. number of clusters for clustering variable 1
    #' @param icc1 A scalar between 0 and 1. intra-cluster correlation for clustering variable 1
    #' @param N_G2 A scalar. number of clusters for clustering variable 2
    #' @param icc2 A scalar between 0 and 1. intra-cluster correlation for clustering variable 2
    #' @param numb_fe1 A scalar. Number of fixed effect for first factor variable
    #' @param numb_fe2 A scalar. Number of fixed effect for second factor variable
    #' @param seed An integer. Set the random seed
    #' @importFrom stats rlnorm
    #' @export
    
    set.seed(seed)
    voters <- 
      fabricatr::fabricate(
        N = N,
        group_id1 = sample(1:N_G1, N, replace = TRUE),
        group_id2 = sample(1:N_G2, N , replace = TRUE),
        ideology1 = fabricatr::draw_normal_icc(mean = 0, N = N, clusters = group_id1, ICC = icc1),
        ideology2 = fabricatr::draw_normal_icc(mean = 0, N = N, clusters = group_id2, ICC = icc2),
        
        ideological_label = fabricatr::draw_ordered(
          x = ideology1,
          break_labels = c(
            "Very Conservative", "Conservative",
            "Liberal", "Very Liberal"
          )
        ),
        income = exp(rlnorm(n = N, meanlog = 2.4 - (ideology1 * 0.1), sdlog = 0.12)),
        Q1_immigration = factor(sample(1:numb_fe1, N, TRUE)),
        Q2_defence = factor(sample(1:numb_fe2, N, TRUE)),   
        treatment = fabricatr::draw_binary(0.5, N = N),
        proposition_vote = fabricatr::draw_binary(latent = ideology1 + ideology2 + 0.1 * treatment, link = "probit")
      )
    
    setDT(voters)
    voters[, log_income := log(income)]
    voters[, Q1_immigration := as.factor(Q1_immigration) ]
    #voters[, Q2_defence := as.factor(Q2_defence)]
  }
