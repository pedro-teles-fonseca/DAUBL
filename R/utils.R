
#' @export

mantissa <- function(x){

  x <- abs(x)
  e <- ifelse(x == 0, 0, floor(log10(x)))
  m <- x / 10^e
  round(m, 10)
}

#' The most significant digit for each element in the input vector
#' @export
#'
msdigit <- function(x){

  x <- x[x != 0]
  x <- floor(mantissa(x))
  return(x)
}

#' @export

smsdigit <- function(x){

  x <- x[x != 0 & (x %% 10 == 0 | mantissa(x) != floor(mantissa(x)))]
  x <- floor((mantissa(x)*10)) %% 10
  return(x)
}

#' @export

theta_benford <- function(d){

  if (d == 1){
    theta_benford <- sapply(1:9, function(x){log10(1+1/(x))})
  } else if (d == 2){
    theta_benford <- sapply(0:9, function(x){sum(log10(1+1/(10*(1:9)+x)))})
  }

  theta_benford
}
