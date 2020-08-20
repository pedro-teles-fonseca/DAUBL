
# Binomial-Beta Model -- BL1

rm(list = ls())

source("R/read-data.r")
source("R/auxiliary-functions.r")

bf_bi_beta <- function(data, a = rep(1, 9), b = rep(1, 9), log10 = TRUE, pi_0 = 0.5, bl = 1, bf.round = 2, probs.round = 3){
  
  if(bl == 1){
    theta_null <- theta_benford(1)
    x <- as.numeric(table(msdigit(data)))
    n <- length(msdigit(data))
    digits <- 1:9
    p_values <- round(bl1_analysis(data)$z_test$results$p_value, probs.round)
  } else {
    theta_null <- theta_benford(2)
    x <- as.numeric(table(smsdigit(data)))
    n <- length(smsdigit(data))
    digits <- 0:9
    p_values <- round(bl2_analysis(data)$z_test$results$p_value, probs.round)
  }
    
  bf <- function(x, theta_null, a, b){
    exp(
      x * log(theta_null) + (n - x) * log(1 - theta_null) + lbeta(a, b) - lbeta(x + a, n + b - x)
    )
  }
  
  
  bfs <- mapply(bf, x, theta_null, a, b)
  
  result <- data.frame(
    "Digit" = digits,
    "BayesFactors" = round(if(log10 == FALSE){bfs} else{log10(bfs)}, bf.round),
     "Evidence" = ifelse(bfs < 1, "Negative",
                         ifelse(bfs < 3.2, "Weak",
                                ifelse(bfs < 10, "Substantial",
                                       ifelse(bfs < 100, "Strong",
                                              "Decisive")))
                         ),
    "PostProb.H0" = round(((1 + ((1 - pi_0) / pi_0) * (1 / bfs)))^(-1), probs.round),
    "LB.PostProb.H0" = round(pcal(p_values), probs.round),
    "P.value" = p_values
    )

  if(log10 == TRUE){names(result) <- gsub("BayesFactors", "log10(BayesFactors)", names(result))}
  return(result)

}  

bin_beta_prior_var <- function(a, b, sd = FALSE, round.var = 3){
  
  var <- (a * b) / (((a + b)^2) * (a + b + 1))
  
  ifelse(sd == FALSE,
         return(round(var, round.var)),
         return(round(sqrt(var), round.var))
  )
}

# a = 1, b = 1 --> Beta(1, 1) priors --> Uniform Prior

# Table 8
results_bi_beta_unif_austria_bl1 <- bf_bi_beta(data = Austria, a = rep(1, 9), b = rep(1, 9), bl = 1) 

# Table 9
results_bi_beta_unif_belgium_bl1 <- bf_bi_beta(data = Belgium, a = rep(1, 9), b = rep(1, 9), bl = 1) 

# Table 10
results_bi_beta_unif_ireland_bl1 <- bf_bi_beta(data = Ireland, a = rep(1, 9), b = rep(1, 9), bl = 1) 

# Table 11
results_bi_beta_unif_Luxembourg_bl1  <- bf_bi_beta(data = Luxembourg, a = rep(1, 9), b = rep(1, 9), bl = 1) 

# Table 12
results_bi_beta_unif_Portugal_bl1 <- bf_bi_beta(data = Portugal, a = rep(1, 9), b = rep(1, 9), bl = 1) 

# c = 1
# a = \theta_{0}, b = 1-\theta_{0} --> Beta(\theta_{0}, 1-\theta_{0})

# (not in the article)
results_bi_beta_dir_c1_austria_bl1 <- bf_bi_beta(data = Austria, a = theta_benford(1), b = 1-theta_benford(1), bl = 1) 

# (not in the article)
results_bi_beta_dir_c1_belgium_bl1 <- bf_bi_beta(data = Belgium, a = theta_benford(1), b = 1-theta_benford(1), bl = 1) 

# (not in the article)
results_bi_beta_dir_c1_ireland_bl1 <- bf_bi_beta(data = Ireland, a = theta_benford(1), b = 1-theta_benford(1), bl = 1) 

# (not in the article)
results_bi_betadir_c1_Luxembourg_bl1  <- bf_bi_beta(data = Luxembourg, a = theta_benford(1), b = 1-theta_benford(1), bl = 1) 

# (not in the article)
results_bi_beta_dir_c1_Portugal_bl1 <- bf_bi_beta(data = Portugal, a = theta_benford(1), b = 1-theta_benford(1), bl = 1) 

# c = 22 for BL1
# a = 22 * \theta_{0}, b = 22-22*\theta_{0} --> Beta(22*\theta_{0}, 22-22*\theta_{0})

# Table 13
results_bi_beta_dir_austria_bl1 <- bf_bi_beta(data = Austria, a = 22*theta_benford(1), b = 22-22*theta_benford(1), bl = 1) 
# Table 14
results_bi_beta_dir_belgium_bl1 <- bf_bi_beta(data = Belgium, a = 22*theta_benford(1), b = 22-22*theta_benford(1), bl = 1) 
# Table 15
results_bi_beta_dir_ireland_bl1 <- bf_bi_beta(data = Ireland, a = 22*theta_benford(1), b = 22-22*theta_benford(1), bl = 1) 
# Table 16
results_bi_beta_dir_Luxembourg_bl1  <- bf_bi_beta(data = Luxembourg, a = 22*theta_benford(1), b = 22-22*theta_benford(1), bl = 1) 
# Table 17
results_bi_beta_dir_Portugal_bl1 <- bf_bi_beta(data = Portugal, a = 22*theta_benford(1), b = 22-22*theta_benford(1), bl = 1) 

var_sd_uniform <- cbind( # Table 21
  rbind(c(NA, NA),
        cbind(
          mapply(bin_beta_prior_var,  rep(1, times = 9), rep(1, times = 9)),
          mapply(bin_beta_prior_var,  rep(1, times = 9), rep(1, times = 9), MoreArgs = list(sd = TRUE))
        )
  ),
  mapply(bin_beta_prior_var,  rep(1, times = 10), rep(1, times = 10)),
  mapply(bin_beta_prior_var,  rep(1, times = 10), rep(1, times = 10), MoreArgs = list(sd = TRUE))
)

rownames(var_sd_uniform) <- 0:9
colnames(var_sd_uniform) <- c("var.bl1", "sd.bl1", "var.bl2", "sd.bl2")

var_sd_22_12theta0 <- cbind( # Table 22
  rbind(c(NA, NA),
        cbind(
          mapply(bin_beta_prior_var, 22 * theta_benford(1), 22 - 22 * theta_benford(1)),
          mapply(bin_beta_prior_var,  22 * theta_benford(1), 22 - 22 * theta_benford(1), MoreArgs = list(sd = TRUE))
        )
  ),
  mapply(bin_beta_prior_var,  12 * theta_benford(2), 12 - 12 * theta_benford(2)),
  mapply(bin_beta_prior_var,  12 * theta_benford(2), 12 - 12 * theta_benford(2), MoreArgs = list(sd = TRUE))
)

rownames(var_sd_22_12theta0) <- 0:9
colnames(var_sd_22_12theta0) <- c("var.bl1", "sd.bl1", "var.bl2", "sd.bl2")

# Export tables to .tex files
# 
library("Hmisc")

latex(results_bi_beta_unif_austria_bl1, file = "tables/bin-beta-model/results_bi_beta_unif_austria_bl1.tex")
latex(results_bi_beta_unif_belgium_bl1, file = "tables/bin-beta-model/results_bi_beta_unif_belgium_bl1.tex")
latex(results_bi_beta_unif_ireland_bl1, file = "tables/bin-beta-model/results_bi_beta_unif_ireland_bl1.tex")
latex(results_bi_beta_unif_Luxembourg_bl1, file = "tables/bin-beta-model/results_bi_beta_unif_Luxembourg_bl1.tex")
latex(results_bi_beta_unif_Portugal_bl1, file = "tables/bin-beta-model/results_bi_beta_unif_Portugal_bl1.tex")

latex(results_bi_beta_dir_austria_bl1, file = "tables/bin-beta-model/results_bi_beta_dir_austria_bl1.tex")
latex(results_bi_beta_dir_belgium_bl1, file = "tables/bin-beta-model/results_bi_beta_dir_belgium_bl1.tex")
latex(results_bi_beta_dir_ireland_bl1, file = "tables/bin-beta-model/results_bi_beta_dir_ireland_bl1.tex")
latex(results_bi_beta_dir_Luxembourg_bl1, file = "tables/bin-beta-model/results_bi_beta_dir_Luxembourg_bl1.tex")
latex(results_bi_beta_dir_Portugal_bl1, file = "tables/bin-beta-model/results_bi_beta_dir_Portugal_bl1.tex")

latex(var_sd_uniform, file = "tables/bin-beta-model/vars_uniform_prior.tex")
latex(var_sd_22_12theta0, file = "tables/bin-beta-model/vars_centered_unimodal_dir.tex")
