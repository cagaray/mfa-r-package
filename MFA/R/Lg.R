#' @title Lg coefficient
#' @param table1 a data frame or matrix.
#' @param table2 a data frame or matrix.
#' @return Lg coefficient of the two tables.
#' @export
Lg <- function(table1, table2) {
  table1 <- data.matrix(table1)
  table2 <- data.matrix(table2)
  tr(tcrossprod(table1) %*% tcrossprod(table2)) /
    (svd(table1)$d[1]^2 * svd(table2)$d[1]^2)
}

#' @title Lg coefficients of subsets of a table
#' @param data A data frame or matrix made of multiple tables
#' @param sets a list of numeric or character vectors indicating the sets of
#'     variables
#' @return a symmetric matrix containing the Lg coefficients
#' @export
Lg_table <- function(data, sets) {
  tables <- SplitTable(data, sets)
  n <- length(sets)
  result <- matrix(nrow = n, ncol = n)
  for (i in 1:n) {
    for (j in 1:n) {
      result[i, j] <- Lg(tables[[i]], tables[[j]])
    }
  }
  result
}
