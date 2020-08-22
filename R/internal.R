
blanks_rm <- function(x) {
  gsub(" ", "", x)
}

na_rm <- function(x){
  x[!is.na(x)]
  }

substr_last <- function(x, n){
  n_char <- nchar(x)
  substr(x, n_char - n + 1, n_char)
}

#' The mantissae of the input numbers

mantissa <- function(x){

  x <- abs(x)
  e <- ifelse(x == 0, 0, floor(log10(x)))
  m <- x / 10^e
  round(m, 10)
}

#' The most significant digit for each element in the input vector

msdigit <- function(x){

  x <- x[x != 0]
  x <- floor(mantissa(x))
  return(x)
}

#' Returns the second most significant digit for each element in the input vector

smsdigit <- function(x){

  x <- x[x != 0 & (x %% 10 == 0 | mantissa(x) != floor(mantissa(x)))]
  x <- floor((mantissa(x)*10)) %% 10
  return(x)
}

theta_benford <- function(d){

  if (d == 1){
    theta_benford <- sapply(1:9, function(x){log10(1+1/(x))})
  } else if (d == 2){
    theta_benford <- sapply(0:9, function(x){sum(log10(1+1/(10*(1:9)+x)))})
  }

  theta_benford
}

#' Multivariate Beta Function

mbeta <- function(x){

  exp(sum(lgamma(x)) - lgamma(sum(x)))
}

#' Multivariate log Beta Function

lmbeta <- function(x){

  sum(lgamma(x)) - lgamma(sum(x))
}





