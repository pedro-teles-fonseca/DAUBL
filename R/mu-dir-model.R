
# Multinomial-Dirichlet Model -- BL1 or BL2

rm(list = ls())

source("R/read-data.r")
source("R/auxiliary-functions.r")

mu_dir_model <- function(data, alpha, log10 = TRUE, pi_0 = 0.5, bf.round = 2, probs.round = 3){
  
  sample_name <- deparse(substitute(data))
  theta_null_bl1 <- theta_benford(1)
  theta_null_bl2 <- theta_benford(2)
  
  x_bl1 <- as.numeric(table(msdigit(data)))
  p_value_bl1 <-  bl1_analysis(data)$chi_test$p_value
  alpha_bl1 <- alpha[[1]]
  
  x_bl2 <- as.numeric(table(smsdigit(data)))
  p_value_bl2 <-  bl2_analysis(data)$chi_test$p_value
  alpha_bl2 <- alpha[[2]]
  
  bf_fun <- function(theta_null, x, alpha){
    exp(
      sum(x * log(theta_null)) + multi_lbeta(alpha) - multi_lbeta(alpha + x)
    )
  }
  
  bf_bl1 <- bf_fun(theta_null_bl1, x_bl1, alpha_bl1)
  bf_bl2 <- bf_fun(theta_null_bl2, x_bl2, alpha_bl2)
  
  result <- function(bf, p_value){
    r <- data.frame(
      "BayesFactor" = round(ifelse(log10 == FALSE, bf, log10(bf)), bf.round),
      "Evidence" = ifelse(bf < 1, "Negative", 
                          ifelse(bf < 3.2, "Weak", 
                                 ifelse(bf < 10, "Substantial", 
                                        ifelse(bf < 100, "Strong",
                                               "Decisive")))
      ),
      "PostProb.H0" = round(((1 + ((1 - pi_0) / pi_0) * (1 / bf)))^(-1), probs.round),
      "LB.PostProb.H0" = round(pcal(p_value), probs.round), 
      "P.value" = round(p_value, probs.round)
    )
    
    rownames(r) <- if(bf == bf_bl1){
      paste0(sample_name, "-BL1")
    } else {
      paste0(sample_name, "-BL2")
    }
    
    if(log10 == TRUE){names(r) <- gsub("BayesFactor", "log10(BayesFactor)", names(r))}
    
    return(r)
  }
  
  return(rbind(
    result(bf_bl1, p_value_bl1), 
    result(bf_bl2, p_value_bl2))
  )
} 

mu_dir_prior_var <- function(alpha, sd = FALSE, round.var = 3) {
  
  alpha_s <- sum(alpha) 
  var <- (alpha * (alpha_s - alpha)) / (alpha_s^2 * (alpha_s + 1))
                                        
  ifelse(sd == FALSE,
         return(round(var, round.var)),
                return(round(sqrt(var), round.var))
                )
  
}

results_uniform_prior <- rbind( # Table 5
  
  mu_dir_model(data = Austria, alpha = list(rep(1, times = 9), rep(1, times = 10))),
  mu_dir_model(data = Belgium, alpha = list(rep(1, times = 9), rep(1, times = 10))),
  mu_dir_model(data = Finland, alpha = list(rep(1, times = 9), rep(1, times = 10))),
  mu_dir_model(data = France, alpha = list(rep(1, times = 9), rep(1, times = 10))),
  mu_dir_model(data = Germany, alpha = list(rep(1, times = 9), rep(1, times = 10))),
  mu_dir_model(data = Greece, alpha = list(rep(1, times = 9), rep(1, times = 10))),
  mu_dir_model(data = Ireland, alpha = list(rep(1, times = 9), rep(1, times = 10))),
  mu_dir_model(data = Italy, alpha = list(rep(1, times = 9), rep(1, times = 10))),
  mu_dir_model(data = Luxembourg, alpha = list(rep(1, times = 9), rep(1, times = 10))),
  mu_dir_model(data = Netherlands, alpha = list(rep(1, times = 9), rep(1, times = 10))),
  mu_dir_model(data = Portugal, alpha = list(rep(1, times = 9), rep(1, times = 10))),
  mu_dir_model(data = Spain, alpha = list(rep(1, times = 9), rep(1, times = 10))),
  mu_dir_model(data = Pooled.sample, alpha = list(rep(1, times = 9), rep(1, times = 10)))
  
)

results_centered_dir <- rbind( # Table 6
  
  mu_dir_model(data = Austria, alpha = list(theta_benford(1), theta_benford(2))),
  mu_dir_model(data = Belgium, alpha = list(theta_benford(1), theta_benford(2))),
  mu_dir_model(data = Finland, alpha = list(theta_benford(1), theta_benford(2))),
  mu_dir_model(data = France, alpha = list(theta_benford(1), theta_benford(2))),
  mu_dir_model(data = Germany, alpha = list(theta_benford(1), theta_benford(2))),
  mu_dir_model(data = Greece, alpha = list(theta_benford(1), theta_benford(2))),
  mu_dir_model(data = Ireland, alpha = list(theta_benford(1), theta_benford(2))),
  mu_dir_model(data = Italy, alpha = list(theta_benford(1), theta_benford(2))),
  mu_dir_model(data = Luxembourg, alpha = list(theta_benford(1), theta_benford(2))),
  mu_dir_model(data = Netherlands, alpha = list(theta_benford(1), theta_benford(2))),
  mu_dir_model(data = Portugal, alpha = list(theta_benford(1), theta_benford(2))),
  mu_dir_model(data = Spain, alpha = list(theta_benford(1), theta_benford(2))),
  mu_dir_model(data = Pooled.sample, alpha = list(theta_benford(1), theta_benford(2)))
  
)

results_centered_unimodal_dir <- rbind( # Table 7
  
  mu_dir_model(data = Austria, alpha = list(22*theta_benford(1), 12*theta_benford(2))),
  mu_dir_model(data = Belgium, alpha = list(22*theta_benford(1), 12*theta_benford(2))),
  mu_dir_model(data = Finland, alpha = list(22*theta_benford(1), 12*theta_benford(2))),
  mu_dir_model(data = France, alpha = list(22*theta_benford(1), 12*theta_benford(2))),
  mu_dir_model(data = Germany, alpha = list(22*theta_benford(1), 12*theta_benford(2))),
  mu_dir_model(data = Greece, alpha = list(22*theta_benford(1), 12*theta_benford(2))),
  mu_dir_model(data = Ireland, alpha = list(22*theta_benford(1), 12*theta_benford(2))),
  mu_dir_model(data = Italy, alpha = list(22*theta_benford(1), 12*theta_benford(2))),
  mu_dir_model(data = Luxembourg, alpha = list(22*theta_benford(1), 12*theta_benford(2))),
  mu_dir_model(data = Netherlands, alpha = list(22*theta_benford(1), 12*theta_benford(2))),
  mu_dir_model(data = Portugal, alpha = list(22*theta_benford(1), 12*theta_benford(2))),
  mu_dir_model(data = Spain, alpha = list(22*theta_benford(1), 12*theta_benford(2))),
  mu_dir_model(data = Pooled.sample, alpha = list(22*theta_benford(1), 12*theta_benford(2)))
  
)

var_sd_alpha_1 <- cbind( # Table 18
  rbind(c(NA, NA),
      cbind(
        mu_dir_prior_var(rep(1, 9)),
        mu_dir_prior_var(rep(1, 9), sd = TRUE)
        )
      ),
  mu_dir_prior_var(rep(1, 10)),
  mu_dir_prior_var(rep(1, 10), sd = TRUE)
)

rownames(var_sd_alpha_1) <- 0:9 
colnames(var_sd_alpha_1) <- rep(c("var.bl1", "sd.bl1", "var.bl2", "sd.bl2"))

var_sd_alpha_theta <- cbind( # Table 19
  rbind(c(NA, NA),
        cbind(
          mu_dir_prior_var(theta_benford(1)),
          mu_dir_prior_var(theta_benford(1), sd = TRUE)
        )
  ),
  mu_dir_prior_var(theta_benford(2)),
  mu_dir_prior_var(theta_benford(2), sd = TRUE)
)

rownames(var_sd_alpha_theta) <- 0:9 
colnames(var_sd_alpha_theta) <- rep(c("var.bl1", "sd.bl1", "var.bl2", "sd.bl2"))

var_sd_alpha_theta_22_12 <- cbind( # Table 20
  rbind(c(NA, NA),
        cbind(
          mu_dir_prior_var(22 * theta_benford(1)),
          mu_dir_prior_var(22 * theta_benford(1), sd = TRUE)
        )
  ),
  mu_dir_prior_var(12 * theta_benford(2)),
  mu_dir_prior_var(12 * theta_benford(2), sd = TRUE)
)

rownames(var_sd_alpha_theta_22_12) <- 0:9 
colnames(var_sd_alpha_theta_22_12) <- rep(c("var.bl1", "sd.bl1", "var.bl2", "sd.bl2"))

# Export tables to .tex files

library("Hmisc")

latex(results_uniform_prior, file = "tables/mu-dir-model/results_uniform_prior.tex")
latex(results_centered_dir, file = "tables/mu-dir-model/results_centered_dir.tex")
latex(results_centered_unimodal_dir, file = "tables/mu-dir-model/results_centered_unimodal_dir.tex")

latex(var_sd_alpha_1, file = "tables/mu-dir-model/vars_uniform_prior.tex")
latex(var_sd_alpha_theta, file = "tables/mu-dir-model/vars_centered_dir.tex")
latex(var_sd_alpha_theta_22_12, file = "tables/mu-dir-model/vars_centered_unimodal_dir.tex")


