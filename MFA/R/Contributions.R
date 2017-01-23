#' The contributions of an Observation to a Dimension for a given mfa object.
#'
#' @param mfa Object of class 'mfa', which should be output from mfa function.
#' @return A matrix of the contributions of each Observation to each
#'     Dimension, with rows corresponding to Observations, and columns
#'     corresponding to Dimensions.
#' 
#' @export
ContribObs <- function(mfa) {
  factor.squared <- mfa$factor.scores ^ 2 / nrow(mfa$factor.scores)
  contributions <- factor.squared %*% diag(1 / mfa$eig)
  return(contributions)
}

#' The contributions of a Variable to a Dimension for a given mfa object.
#'
#' @param mfa Object of class 'mfa', which should be output from mfa function.
#' @return A matrix of the contributions of each Variable to each
#'     Dimension, with rows corresponding to Variables, and columns
#'     corresponding to Dimensions.
#' 
#' @export
ContribVar <- function(mfa)
  diag(mfa$weights) %*% mfa$loadings ^ 2

#' The contributions of a Table to a Dimension for a given mfa object.
#'
#' @param mfa Object of class 'mfa', which should be output from mfa function.
#' @return A matrix of the contributions of each Table to each
#'     Dimension, with rows corresponding to Tables, and columns
#'     corresponding to Dimensions.
#' 
#' @export
ContribTable <- function(mfa) {
  ## Retrieve the sets from the weights in the mfa.
  count <- 0
  sets <- list()
  for (i in 1:length(unique(mfa$weights))) {
    length <- length(mfa$weights[mfa$weights == mfa$weights[count + 1]])
    sets[[i]] <- (count + 1):(count + length)
    count <- count + length
  }
  
  ## Compute the Contributions of a Table
  tables <- list()
  ctr.var <- t(ContribVar(mfa))
  for (i in 1:length(sets)) {
    tables[[i]] <- ctr.var[, sets[[i]]]
    tables[[i]] <- apply(tables[[i]], FUN = sum, MARGIN = 1)
  }
  return(t(do.call(cbind, tables)))
}
