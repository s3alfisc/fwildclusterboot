create_data_1 <- 
  function(N, N_G, icc){
    
    #' Function creates data for tests and examples
    #' @import data.table
    #' @import fabricatr
    #' @param N number of observations
    #' @param N_G number of clusters
    #' @param icc intra-cluster correlation 
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
        Q1_immigration = fabricatr::draw_likert(x = ideology, type = 7),
        #Q2_defence = draw_likert(x = ideology + 0.5, type = 7),
        treatment = fabricatr::draw_binary(0.5, N = N),
        proposition_vote = fabricatr::draw_binary(latent = ideology + 0.01 * treatment, link = "probit")
      )
    
    setDT(voters)
    voters[, log_income := log(income)]
    voters[, Q1_immigration := as.factor(Q1_immigration) ]
    #voters[, Q2_defence := as.factor(Q2_defence)]
  }

create_data_2 <- 
  function(N, N_G1, icc1, N_G2, icc2){
    
    #' Function creates data for tests and examples
    #' @import data.table
    #' @import fabricatr
    #' @param N number of observations
    #' @param N_G number of clusters
    #' @param icc intra-cluster correlation 
    #' @export
    
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
        Q1_immigration = fabricatr::draw_likert(x = ideology2, type = 7),
        #Q2_defence = draw_likert(x = ideology + 0.5, type = 7),
        treatment = fabricatr::draw_binary(0.5, N = N),
        proposition_vote = fabricatr::draw_binary(latent = ideology1 + ideology2 + 0.01 * treatment, link = "probit")
      )
    
    setDT(voters)
    voters[, log_income := log(income)]
    voters[, Q1_immigration := as.factor(Q1_immigration) ]
    #voters[, Q2_defence := as.factor(Q2_defence)]
  }
