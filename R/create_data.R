create_data <- 
  function(N, N_G, icc){
    voters <- 
      fabricate(
        N = N,
        group_id = rep(1:N_G, N / N_G),
        ideology = draw_normal_icc(mean = 0, N = N, clusters = group_id, ICC = icc),
        ideological_label = draw_ordered(
          x = ideology,
          break_labels = c(
            "Very Conservative", "Conservative",
            "Liberal", "Very Liberal"
          )
        ),
        income = exp(rlnorm(n = N, meanlog = 2.4 - (ideology * 0.1), sdlog = 0.12)),
        Q1_immigration = draw_likert(x = ideology, type = 7),
        #Q2_defence = draw_likert(x = ideology + 0.5, type = 7),
        treatment = draw_binary(0.5, N = N),
        proposition_vote = draw_binary(latent = ideology + 0.01 * treatment, link = "probit")
      )
    
    setDT(voters)
    voters[, log_income := log(income)]
    voters[, Q1_immigration := as.factor(Q1_immigration) ]
    #voters[, Q2_defence := as.factor(Q2_defence)]
  }
