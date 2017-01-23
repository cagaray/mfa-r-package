### Create a list of *matrices* from the 'sets' variable
SplitTable <- function(data, sets) {
    tables <- list()
    for (i in 1:length(sets)) {
        tables[[i]] <- data.matrix(data[, sets[[i]], drop = FALSE])
    }
    tables
}

### MFA on normalised tables
NormalizeAndGSVD <- function(tables) {
    svs <- numeric(0)
    uniq.svs <- numeric(0)
    
    for (i in 1:length(tables)) {
        ## Accumulate a list of singluar values
        sv <- svd(tables[[i]])$d[1]
        svs <- c(svs, rep(sv, ncol(tables[[i]])))
        uniq.svs <- c(uniq.svs, sv)
    }

    ## Obtain the matrices needed for GSVD
    n <- nrow(tables[[1]])
    m <- rep(1 / n, n)
    a <- svs ^ (-2)
    uniq.a <- uniq.svs ^ (-2)
    X <- do.call(cbind, tables)
    X.tilde <- diag(sqrt(m)) %*% X %*% diag(sqrt(a))
    X.tilde.svd <- svd(X.tilde)
    P <- diag(1 / sqrt(m)) %*% X.tilde.svd$u
    d <- X.tilde.svd$d
    Q <- diag(1 / sqrt(a)) %*% X.tilde.svd$v

    list(P = P,
         d = d,
         Q = Q,
         a = a,
         uniq.a = uniq.a)
}

#' Multiple factor analysis on a data frame or matrix.
#'
#' @param data the data set, a data frame or matrix
#' @param sets a list of numeric or character vectors indicating the sets of
#'     variables
#' @param ncomps integer indicating how many number of components
#'     (i.e. factors) are to be extracted
#' @param center either a logical value or a numeric vector of length
#'     equal to the number of active variables in the analysis
#' @param scale either a logical value or a numeric vector of length
#'     equal to the number of active variables in the analysis
#' @return an object of class "mfa", containing eigenvalues, factor
#'     scores, partial factor scores, factor loadings, and other data
#'     (e.g. weights) for related functions.
#' 
#' @export
mfa <- function(data, sets, ncomp = NULL, center = TRUE, scale = TRUE) {
    var.indeces <- unlist(sets)
    data[ , var.indeces] <-
        scale(data[ , var.indeces], center, scale)
    if (scale[1] != FALSE)
        data[ , var.indeces] <- data[ , var.indeces] / sqrt(nrow(data) - 1)
    tables <- SplitTable(data, sets)
    gsvd <- NormalizeAndGSVD(tables)

    if (is.null(ncomp) || ncomp > length(gsvd$d)) ncomp = length(gsvd$d)

    ## Compute the partial factor scores
    nvar <- vapply(tables, ncol, FUN.VALUE = 0) # num of vars in each table
    positions <- c(0, cumsum(nvar))
    pfscores <- list()
    k <- length(tables)
    for (i in 1:k) {
        pfscores[[i]] <- k * gsvd$uniq.a[i] *
            tables[[i]] %*%
            gsvd$Q[(positions[i] + 1):(positions[i + 1]), 1:ncomp]
    }
    
    return(
        structure(
            list(eig = (gsvd$d ^ 2),
                factor.scores = (gsvd$P %*% diag(gsvd$d))[ , 1:ncomp],
                loadings = (gsvd$Q)[, 1:ncomp],
                partial.factor.scores = pfscores,
                weights = gsvd$a
                ),
            class = "mfa"
        )
    )
}
