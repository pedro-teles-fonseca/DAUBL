
#' @title The most significant digits of numbers
#'
#' @description Obtain the most significant digits of numbers in numeric vectors or matrices.
#'
#' @param x A numeric vector or matrix.
#'
#' @details For each non-zero, non-missing element of `x`, `msdigit` returns the first digit of that number's floating point representation of base 10. `NA` values in `x` generate `NA` values in the output of `msdigit`.
#'
#' @return `msdigit` returns a numeric vector with {\link[base]{length}} equal to the number of non-zero elements of `x` plus the number of `NA` values in `x`.
#'
#' @seealso
#' * \code{\link[daubl]{smsdigit}} to obtain the second most significant digits of numbers.
#'
#' @examples
#' msdigit(451)
#' msdigit(0.00451)
#' msdigit(-0.00451)
#' msdigit(c(-31.12, -0.23, 0.1234, 5.32))
#'
#' x <- c(1, 2, 3, NA)
#' length(x)
#' length(msdigit(x)) # equal to length(x)
#'
#' x <- c(0, 1, 2)
#' length(x)
#' length(msdigit(x)) # not equal to length(x)
#'
#' @export

msdigit <- function(x) {

  x <- x[x != 0]
  x <- floor(mantissa(x))
  return(x)
}

#' @title The second most significant digits of numbers
#'
#' @description Obtain the second most significant digits of numbers in numeric vectors or matrices.
#'
#' @param x A numeric vector or matrix.
#'
#' @details For each non-zero, non-missing element of `x` that is greater than or equal to 10, `smsdigit` returns the second digit of that number's floating point representation of base 10. `NA` values in `x` generate `NA` values in the output of `smsdigit`.
#'
#' @return `smsdigit` returns a numeric vector with {\link[base]{length}} equal to the number of elements of `x` that are greater than or equal to 10 plus the number of `NA` values in `x`.
#'
#' @seealso
#' * \code{\link[daubl]{msdigit}} to obtain the most significant digits of numbers.
#'
#' @examples
#' smsdigit(451)
#' smsdigit(0.00451)
#' smsdigit(-0.00451)
#' smsdigit(c(-31.12, -0.23, 0.1234, 5.32))
#'
#' x <- c(11, 22, 33, NA)
#' length(x)
#' length(smsdigit(x)) # equal to length(x)
#'
#' x <- c(0, 1, 2,  10, 20)
#' length(x)
#' length(smsdigit(x)) # not equal to length(x)
#'
#' @export

smsdigit <- function(x) {

  x <- x[x != 0 & (x %% 10 == 0 | mantissa(x) != floor(mantissa(x)))]
  x <- floor((mantissa(x)*10)) %% 10
  return(x)
}

#' @title Benford's law for first and second digits
#'
#' @description First and second digit probabilities, as postulated by Benford's law.
#'
#' @param d Numeric vector of {\link[base]{length}} 1. Admissible values: 1 or 2.
#'
#' @details Benford’s law is an empirical regularity regarding the occurrence of digits in numbers that was first introduced by \insertCite{newcomb1881note;textual}{daubl} and later popularized and supported with empirical evidence by \insertCite{benford1938;textual}{daubl}. The formulas that give Benford's law first and second digit probabilities can be consulted in \insertCite{hill1995derivation;textual}{daubl}.
#'
#' @return
#' * `benford(1)` returns a numeric vector of {\link[base]{length}} 9.
#' * `benford(2)` returns a numeric vector {\link[base]{length}} 10.
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso
#' * \code{\link[daubl]{msdigit}} to obtain the most significant digits of numbers.
#' * \code{\link[daubl]{smsdigit}} to obtain the second most significant digits of numbers.
#'
#' @examples
#' benford(1)
#' benford(2)
#'
#' @export

benford <- function(d) {

  if (d == 1) {
    log10(1 + 1/1:9)
  } else if (d == 2) {
    sapply(0:9, function(x) {sum(log10(1+1/(10*(1:9)+x)))})
  } else {
    stop("Invalid argument: 'd' must be either 1 or 2.")
  }
}