
context("msdigit")

test_that("msdigit 1", {
  expect_equal(
    msdigit(c(-31.122, -0.00023, 0.1234, -325.32, 0.21, 123)),
    c(3, 2, 1, 3, 2, 1)
  )
}
)

test_that("msdigit 2", {
  expect_length(
    msdigit(c(-31.122, -0.00023, 0.1234, -325.32, 0.21, 123)),
    6
  )
}
)

test_that("smsdigit 3", {
  expect_length(
    msdigit(c(0, -1, 22, 33, 44, NA)),
    5
  )
}
)

test_that("smsdigit 4", {
  expect_equal(
    msdigit(c(12, NA)),
    c(1, NA)
  )
}
)

context("smsdigit")

test_that("smsdigit 1", {
  expect_equal(
    smsdigit(c(-31.122, -0.00023, 0.1234, -325.32, 0.21, 123)),
    c(1, 3, 2, 2, 1, 2)
  )
}
)

test_that("smsdigit 2", {
  expect_length(
    smsdigit(c(-31.122, -0.00023, 0.1234, -325.32, 0.21, 123)),
    6
  )
}
)

test_that("smsdigit 3", {
  expect_length(
    smsdigit(c(0, -1, 22, 33, 44, NA)),
    4
  )
}
)

test_that("smsdigit 4", {
  expect_equal(
    smsdigit(c(12, NA)),
    c(2, NA)
  )
}
)

context("benford")

test_that("benford(1) 1", {
  expect_equal(
    benford(1),
    sapply(1:9, function(x) {log10(1+1/(x))})
  )
}
)

test_that("benford(1) 2", {
  expect_equal(
    round(benford(1), 3),
    c(0.301, 0.176, 0.125, 0.097, 0.079, 0.067, 0.058, 0.051, 0.046)
  )
}
)

test_that("benford(2) 1", {
  expect_equal(
    benford(2),
    sapply(0:9, function(x) {sum(log10(1+1/(10*(1:9)+x)))})
  )
}
)

test_that("benford(2) 2", {
  expect_equal(
    round(benford(2), 3),
    c(0.120, 0.114, 0.109, 0.104, 0.100, 0.097, 0.093, 0.090, 0.088, 0.085)
  )
}
)



