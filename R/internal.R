
blanks_rm <- function(x) {
  gsub(" ", "", x)
}

na_rm <- function(x) {
  x[!is.na(x)]
}

substr_last <- function(x, n) {
  n_char <- nchar(x)
  substr(x, n_char - n + 1, n_char)
}

mantissa <- function(x) {

  x <- abs(x)
  e <- ifelse(x == 0, 0, floor(log10(x)))
  m <- x / 10^e
  round(m, 10)
}

mbeta <- function(x) { # Multivariate Beta Function

  exp(sum(lgamma(x)) - lgamma(sum(x)))
}

lmbeta <- function(x) { # Multivariate log Beta Function

  sum(lgamma(x)) - lgamma(sum(x))
}

# nocov start

release_questions <- function() {

  c(
    "Have you updated the version number in inst/CITATION (two fields)?",
    "Have you run all the tests listed on the cran-comments.md?"
  )
}

# nocov end