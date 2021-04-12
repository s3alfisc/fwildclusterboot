create_data <-
  function(N, N_G1, icc1, N_G2, icc2, numb_fe1, numb_fe2, seed, weights) {

    #' Simulate Data 
    #' 
    #' Function simulates data for tests and examples with clustering variables
    #' and fixed-effects.
    #' 
    #' @param N number of observations
    #' @param N_G1 A scalar. number of clusters for clustering variable 1
    #' @param icc1 A scalar between 0 and 1. intra-cluster correlation for clustering variable 1
    #' @param N_G2 A scalar. number of clusters for clustering variable 2
    #' @param icc2 A scalar between 0 and 1. intra-cluster correlation for clustering variable 2
    #' @param numb_fe1 A scalar. Number of fixed effect for first factor variable
    #' @param numb_fe2 A scalar. Number of fixed effect for second factor variable
    #' @param seed An integer. Set the random seed
    #' @param weights Possible regression weights to be used in estimation
    #' @return A simulated \code{data.frame} with specified numbers of clusters, 
    #'         intra-cluster correlations and dimensionality of fixed effects. 
    set.seed(seed)
    voters <-
      fabricatr::fabricate(
        N,
        group_id1 = sample(1:N_G1, N, replace = TRUE),
        group_id2 = sample(1:N_G2, N, replace = TRUE),
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
        #Q1_immigration_latent = rnorm(N),
        #Q1_immigration = ifelse(Q1_immigration_latent > 0.5, 1,
        #  ifelse(Q1_immigration_latent <= 0.5 & Q1_immigration_latent > 0, 2, 3)
        #),
        #Q2_defense_latent = rnorm(N, 0, 3),
        #Q2_defense = ifelse(Q2_defense_latent > 0.5, 1,
        #  ifelse(Q2_defense_latent <= 0.5 & Q2_defense_latent > 0, 2, 3)
        #),
        Q1_immigration = sample(1:numb_fe1, N, TRUE),
        Q2_defense = sample(1:numb_fe2, N, TRUE),
        treatment = fabricatr::draw_binary(0.5, N = N),
        proposition_vote = fabricatr::draw_binary(latent = ideology1 + ideology2 + 0.2 * treatment + 2 * Q1_immigration + rnorm(N, 0, 3), link = "probit")
      )

    voters$Q1_immigration <- as.factor(voters$Q1_immigration)
    voters$Q2_defense <- as.factor(voters$Q2_defense)


    voters$log_income <- log(voters$income)
    voters$Q1_immigration <- as.factor(voters$Q1_immigration)

    # add weights
    voters$weights <- sample(1:10, N, replace = TRUE) / 10

    voters
  }
