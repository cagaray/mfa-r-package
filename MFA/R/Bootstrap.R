### Test the length to be 'times'
BootstrapFactorScores <- function(mfa, times) {
    Fs <- mfa$partial.factor.scores
    K <- length(Fs)
    Fs.star <- list()
    for (i in 1:times) {
        indeces <- sample(K, K, replace = TRUE)
        F.star <- Reduce("+", Fs[indeces]) / K
        Fs.star[[i]] <- F.star
    }
    Fs.star
}

#' @title Bootstrap Ratios for Factor Scores
#' @param mfa a "mfa" object returned by function mfa
#' @param times the size of bootstrap sample
#' @return a matrix of bootstrap ratios for factor scores
#'
#' @export
BootstrapRatios <- function(mfa, times = 1000) {
    BFs <- BootstrapFactorScores(mfa, times)
    BF.mean <- Reduce("+", BFs) / times
    sigma <- sqrt(
        Reduce("+",
               Map(function(F) (F - BF.mean) ^ 2, BFs)
               ) / times
    )
    BF.mean / sigma
}
