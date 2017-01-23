tr <- function(m) sum(diag(m))

#' @title RV coefficient
#' @param table1 a data frame or matrix
#' @param table2 a data frame or matrix
#' @return the RV coefficient
#' @export
RV <- function(table1, table2) {
    table1 <- data.matrix(table1)
    table2 <- data.matrix(table2)
    tr(tcrossprod(table1) %*% tcrossprod(table2)) /
        sqrt(tr(tcrossprod(table1) %*% tcrossprod(table1)) *
             tr(tcrossprod(table2) %*% tcrossprod(table2)))
}

#' @title RV coefficient of subsets of a table
#' @param data a data frame or matrix
#' @param sets a list of numeric or character vectors indicating the sets of
#'     variables
#' @return a symmetric matrix containing the RV coefficients
#' @export
RV_table <- function(data, sets) {
    tables <- SplitTable(data, sets)
    n <- length(sets)
    result <- matrix(, nrow = n, ncol = n)
    for (i in 1:n) {
        for (j in 1:n) {
            result[i, j] <- RV(tables[[i]], tables[[j]])
        }
    }
    result
}
