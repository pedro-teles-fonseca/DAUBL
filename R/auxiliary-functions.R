
## Auxiliary functions

mantissa <- function(x){ # returns the mantissae of the input numbers
  
  x <- abs(x)
  e <- ifelse(x == 0, 0, floor(log10(x)))
  m <- x / 10^e
  round(m, 10)
}

msdigit <- function(x){ # returns the most significant digit for each element in the input vector
  
  x <- x[x != 0]
  x <- floor(mantissa(x))
  return(x)
}

smsdigit <- function(x){ # returns the second most significant digit for each element in the input vector
  
  x <- x[x != 0 & (x %% 10 == 0 | mantissa(x) != floor(mantissa(x)))]
  x <- floor((mantissa(x)*10)) %% 10
  return(x)
}

bl_length_without_NAs <- function(v){ # returns vector length excluding missing values
  
  return(
    c(
      length(!is.na(msdigit(v))),
      length(!is.na(smsdigit(v)))
    )
  )
}

theta_benford <- function(d){ # Benford's law digit probabilities (BL1 and BL2)
  
  if (d == 1){
    theta_benford <- sapply(1:9, function(x){log10(1+1/(x))})
  } else if (d == 2){
    theta_benford <- sapply(0:9, function(x){sum(log10(1+1/(10*(1:9)+x)))})
  }
  return(theta_benford)
}

bcal <- function(p, l = NULL) { # P-Value Calibration from Selke, Bayarri & Berger (2001) 
# Calibrates p-values into lower bounds for Bayes Factors in favour of the null
  
  if (missing(l)){  
    
    ifelse(p>1, stop("p>1", call. = FALSE),
           ifelse(p<0, stop("p<0", call. = FALSE),
                  ifelse(p==0, 0, 
                         ifelse(p < (1 / exp(1)), -1*exp(1)*p*log(p), 1)
                  )
           )
    )
    
  } else  {
    
    ifelse(p>1, stop("p>1", call. = FALSE),
           ifelse(p<0, stop("p<0", call. = FALSE),
                  ifelse(p==0, log(0, base=l), 
                         ifelse(p < (1 / exp(1)), log(-1*exp(1)*p*log(p),base = l), log(1, base=l))
                  )
           )    
    )
    
  }
}

pcal <- function(p, prob = 0.5){ # P-Value Calibrations from Selke, Bayarri & Berger (2001) 
# Calibrates p-values into lower bounds for posterior probability of the null. 
  
  ifelse(p > 1 , stop("p>1 -- Invalid p-value.", call. = FALSE),
         ifelse(p < 0, stop("p<0 -- invalid p-value.", call. = FALSE),
                ifelse(p == 0, 0, 
                       ifelse(p<(1/exp(1)), return((1+((1-prob)/prob)*(1/bcal(p)))^(-1)), 
                              return((1+((1-prob)/prob)*(1/bcal(p)))^(-1)))
                )
         )	
  )
}

bl1_analysis <- function(x){ # Benford's law for first digit analysis
  # Returns sample size, observed and expected counts and proportions, chi-squared test, and z-test
  
  n <- length(msdigit(x))
  dims = c(0.1, 0.05, 0.01, 0.005, 0.001)
  
  out <-  data.frame(
    digit = 1:9,
    x_observed = as.vector(table(msdigit(x))),
    x_expected = n*theta_benford(1),
    theta_observed = as.vector(table(msdigit(x))/n),
    theta_expected = theta_benford(1)
  )
  
  test <- chisq.test(out$x_observed, p = theta_benford(1))
  
  z_stats <- vector(length = 9)
  p_ztest <- vector(length = 9)
  
  for(i in 1:9){
    z_stats[i] <- (abs(out$theta_observed[i]-theta_benford(1)[i])-(1/(2*n)))/sqrt((theta_benford(1)[i]*(1-theta_benford(1)[i]))/n)
    p_ztest[i] <- 2* (1-pnorm(abs(z_stats[i]), 0, 1))
  } 
  
  list(
    n = n,
    table = out, 
    chi_test = list(
      H0 = "Conformance to Benford's law",
      method = test$method,
      statistic = test$statistic,
      p_value = test$p.value,
      results = data.frame(
        dimension = dims,
        decision = sapply(dims, 
                           function(d, pv){if(pv<d){"Reject H0"} else {"Don't reject H0"}},
                           pv = test$p.value
        )
      )
    ),
    z_test = list(
      H0 = "Conformance to Benford's law",
      results = data.frame(
        digit = 1:9,
        z_stat = z_stats,
        p_value = p_ztest
      )
    )
  )
} 

bl2_analysis <- function(x){ # Benford's law for second digit analysis
  # Returns sample size, observed and expected counts and proportions, chi-squared test, and z-test
  
  n <- length(smsdigit(x))
  dims = c(0.1, 0.05, 0.01, 0.005, 0.001)
  
  out <-  data.frame(
    digit = 0:9,
    x_observed = as.vector(table(smsdigit(x))),
    x_expected = n*theta_benford(2),
    theta_observed = as.vector(table(smsdigit(x))/n),
    theta_expected = theta_benford(2)
  )
  
  test <- chisq.test(out$x_observed, p = theta_benford(2))
  
  z_stats <- vector(length = 10)
  p_ztest <- vector(length = 10)
  
  for(i in 0:9){
    z_stats[i] <- (abs(out$theta_observed[i]-theta_benford(2)[i])-(1/(2*n)))/sqrt((theta_benford(2)[i]*(1-theta_benford(2)[i]))/n)
    p_ztest[i] <- 2* (1-pnorm(abs(z_stats[i]), 0, 1))
  } 
  
  list(
    n = n,
    table = out, 
    chi_test = list(
      H0 = "Conformance to Benford's law",
      method = test$method,
      statistic = test$statistic,
      p_value = test$p.value,
      results = data.frame(
        dimension = dims,
        decision = sapply(dims, 
                           function(d, pv){if(pv<d){"Reject H0"} else {"Don't reject H0"}},
                           pv = test$p.value
        )
      )
    ),
    z_test = list(
      H0 = "Conformance to Benford's law",
      results = data.frame(
        digit = 0:9,
        z_stat = z_stats,
        p_value = p_ztest
      )
    )
  )
} 

multi_lbeta <- function(a){ # Multivariate log Beta Function
  
  sum(lgamma(a)) - lgamma(sum(a))
}
