context("mfa and contribution properties")

mfa.wines <- mfa(wines, sets)

test_that("Original Data can be obtained from Factor Scores and Loadings",{
  var.indeces <- unlist(sets)
  scaled.wines <- scale(wines[ , var.indeces]) / sqrt(nrow(wines) - 1)
  expect_that(mfa.wines$factor.scores %*% t(mfa.wines$loadings),
              is_equivalent_to(scaled.wines))
})

test_that("Loadings and Weights can be used to get the identity matrix",{
  Q <- t(mfa.wines$loadings)
  A <- diag(mfa.wines$weights)
  expect_that(Q %*% A %*% t(Q), is_equivalent_to(diag(nrow(Q))))
})

test_that("Squared factor squared divided by mass gives the eigen values",{
  Factor.Eig <- apply(mfa.wines$factor.scores ^ 2 / nrow(mfa.wines$factor.scores),
                      MARGIN = 2, FUN = sum)
  expect_that(Factor.Eig, equals(mfa.wines$eig))
})

test_that("Averaged partial factor scores give the factor scores",{
  Wines.Factor <- matrix(0, nrow=nrow(mfa.wines$factor.scores),
                        ncol=ncol(mfa.wines$factor.scores))
  for (i in 1:length(mfa.wines$partial.factor.scores)) {
    Wines.Factor <- Wines.Factor + mfa.wines$partial.factor.scores[[i]] /
                     length(mfa.wines$partial.factor.scores)
  }
  expect_that(Wines.Factor, equals(mfa.wines$factor.scores))
})

test_that("Sum of contributions by the dimensions gives 1",{
  ctr.obs <- ContribObs(mfa.wines)
  ctr.var <- ContribVar(mfa.wines)
  ctr.table <- ContribTable(mfa.wines)
  expect_that(apply(ctr.obs, MARGIN = 2, FUN = sum), equals(rep(1, ncol(ctr.obs))))
  expect_that(apply(ctr.var, MARGIN = 2, FUN = sum), equals(rep(1, ncol(ctr.var))))
  expect_that(apply(ctr.table, MARGIN = 2, FUN = sum), equals(rep(1, ncol(ctr.table))))
})

test_that("Diagonal of RV Table is one",{
  RV.table <- RV_table(wines, sets)
  expect_that(apply(RV.table, MARGIN = 2, FUN = max), equals(rep(1, nrow(RV.table))))
  expect_that(diag(RV.table), equals(rep(1, nrow(RV.table))))
})
