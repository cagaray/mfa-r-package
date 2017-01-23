context("mfa object class and dimensions")

ncomp <- 5
test.wines <- mfa(wines, sets, ncomp = ncomp)
var.indices <- unlist(sets)
wines.data <- wines[,var.indices]

test_that("mfa function returns mfa object", {
  expect_that(test.wines, is_a("mfa"))
})

test_that("proper number of eigenvalues",{
  expect_that(test.wines$eig, is_a("numeric"))
  expect_that(length(test.wines$eig), equals(nrow(wines)))
})

test_that("dimensions of factor scores matrix",{
  expect_that(test.wines$factor.scores, is_a("matrix"))
  expect_that(dim(test.wines$factor.scores), equals(c(nrow(wines), ncomp)))
})

test_that("dimensons of partial factor scores matrix",{
  expect_that(test.wines$partial.factor.scores, is_a("list"))
  expect_that(test.wines$partial.factor.scores[[1]], is_a("matrix"))
  for (i in 1:length(test.wines$partial.factor.scores)){
    expect_that(dim(test.wines$partial.factor.scores[[i]]),
                equals(dim(test.wines$factor.scores)))
  }
})

test_that("dimension of loadings matrix",{
  expect_that(test.wines$loadings, is_a("matrix"))
  expect_that(dim(test.wines$loadings), equals(c(ncol(wines.data), ncomp)))
})

test_that("proper number of weights",{
  expect_that(test.wines$weights, is_a("numeric"))
  expect_that(length(test.wines$weights), equals(ncol(wines.data)))
})
