context("mfa and contributions functions output with wines dataset")

mfa.wines <- mfa(wines, sets)

test_that("Factor Scores from mfa function", {
  Factor.Scores=t(matrix(c(-0.980, 0.163, -0.809, 0.033, -0.761, -0.454, -1.115,
                            -0.166, 1.373, -0.128,1.264, -0.108, 0.808, 0.205,
                            0.925, 0.408, -0.669, 0.369, 0.073, -0.757, -0.476,
                            0.513, 0.367,-0.076), nrow=2, ncol=12))
  expect_that(mfa.wines$factor.scores[,1:2], equals(Factor.Scores, tolerance = 1e-3))
})

test_that("Partial Factor Scores from mfa function", {
  Partial.Factor.Score = t(matrix(c(-1.037, 0.155, -1.179, 0.596, -0.213, -0.104,
                                     -0.946, 0.446, 1.546, -0.676, 1.176, -0.747,
                                     0.698, 0.166, 1.006, -0.063, -0.922, 0.486, 
                                     0.189, -0.936, -0.643, 0.640, 0.323, 0.036),
                                  nrow=2, ncol=12))
  expect_that(mfa.wines$partial.factor.scores[[1]][,1:2],
              equals(Partial.Factor.Score, tolerance = 1e-3))
})

test_that("Eigenvalues from mfa function", {
  eigen = c(0.770, 0.123, 0.091, 0.076, 0.060, 0.039, 0.031, 0.025, 0.019, 0.013, 0.011)
  expect_that(mfa.wines$eig[1:11], equals(eigen, tolerance = 1e-3))
})


test_that("Loadings from mfa function", {
  Loadings = matrix(c(-0.294, -0.267, -0.260, 0.241, 0.286, -0.233, -0.297, -0.296,
                       -0.267, 0.256, -0.238, -0.222, -0.305, -0.136, -0.258, 0.203,
                       -0.277, 0.267, -0.313, -0.261, -0.303, 0.230, -0.205, -0.296,
                       -0.213, -0.268,  0.124, -0.259, 0.177, -0.302, -0.277, -0.265,
                       0.231, -0.205, -0.275, -0.246, -0.277, 0.180, -0.276, -0.247,
                       -0.235, 0.138, -0.286, .239, -0.303, -0.235, -0.287, 0.251,
                       -0.296, -0.323, -0.274, -0.286, 0.282, 0.318, -0.248, 0.396,
                       -0.184, 0.161, 0.129, 0.183, -0.178, 0.200, -0.240, -0.113, 
                       -0.333, 0.234, -0.228, 0.379, -0.365, -0.297, 0.283, 0.082,
                       -0.353, -0.169, 0.066, -0.117, 0.201, -0.249, 0.258, 0.132,
                       -0.144, .019, 0.215, -0.274, 0.328, 0.031, -0.340, 0.380,
                       -0.410, 0.290, 0.376, 0.309, -0.376, 0.231, -0.219, -0.261,
                       0.293, 0.241, -0.221, 0.226, -0.083, -0.188, 0.080, -0.262,
                       0.187, 0.272), nrow=53, ncol=2)
  expect_that(mfa.wines$loadings[,1:2], equals(Loadings, tolerance = 1e-3))
})

test_that("weights from mfa function", {
  weights = c(0.241, 0.239, 0.275, 0.273, 0.307, 
              0.302, 0.417, 0.272, 0.264, 0.309)
  expect_that(unique(mfa.wines$weights), equals(weights, tolerance = 1e-3))
})

test_that("Output from ContribTable", {
  Table.Contributions = t(matrix(c(0.101, 0.095, 0.100, 0.068, 0.101, 0.152, 
                                   0.096, 0.049, 0.098, 0.063, 0.101, 0.104, 0.102, 
                                   0.224, 0.096, 0.134, 0.100, 0.053, 0.105, 0.057),
                                 ncol=10, nrow=2))
  expect_that(ContribTable(mfa.wines)[,1:2], equals(Table.Contributions, tolerance = 1e-3))
})

test_that("Output from ContribVar", {
  Var.Contributions <- matrix(c(21, 17, 16, 14, 20, 13, 21, 21, 17, 16, 14, 12, 26,
                                5, 18, 11, 21, 20,27, 19, 25, 14, 12, 27, 14, 22,
                                5, 20, 10, 28, 23, 21, 16, 13, 31, 25, 32, 14, 21,
                                17, 15, 5, 22, 16, 24, 14, 22, 17, 23, 32, 23, 25, 25,
                                24, 15, 38, 8, 6, 4, 8, 8, 10, 14, 3, 27, 15, 14, 39,
                                37, 24, 22, 2, 34, 8, 1, 4, 12, 19, 20, 5, 6, 0, 14,
                                23, 32, 0, 35, 60, 70, 35, 59, 26, 39, 15, 13, 19,
                                23, 15, 13, 13, 2, 9, 2, 21, 11, 23), nrow=53, ncol=2)
  expect_that(round(ContribVar(mfa.wines)[,1:2]*1000), is_identical_to(Var.Contributions))
})
