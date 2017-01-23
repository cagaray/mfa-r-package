#' Prints a table with the singular values (i.e. square root of eigenvalues), the eigenvalues, cumulative,
#' percentage of intertia, cumulative percentage of inertia, for all the extracted components.
#'
#' @param x object of class mfa.
#' @return None
#'
#' @export
EigSummary <- function(x){
  width_first_col <- 15
  width_col <- 7
  float_precision <- 6
  num_cols = length(x$eig)

  for(i in 1:width_first_col){
    cat("=", sep = "")
  }
  for(i in 1:num_cols){
    for(j in 1:width_col){
      cat("=", sep = "")
    }
  }
  cat("\n", sep = "")

  for(i in 1:width_first_col){
    cat(" ", sep = "")
  }
  cat("Component\n")

  for(i in 1:width_first_col){
    cat(" ", sep = "")
  }
  for(i in 1:num_cols){
    for(j in 1:width_col){
      cat("-", sep = "")
    }
  }
  cat("\n", sep = "")

  for(i in 1:width_first_col){
    cat(" ", sep = "")
  }
  for(i in 1:length(x$eig)){
    cat(sprintf("%*d", width_col, i), sep = "")
  }
  cat("\n", sep = "")

  for(i in 1:width_first_col){
    cat("-", sep = "")
  }
  for(i in 1:num_cols){
    for(j in 1:width_col){
      cat("-", sep = "")
    }
  }
  cat("\n", sep = "")

  cat(sprintf("%-*s", width_first_col, "Singular value"), sep = "")
  for(i in 1:length(x$eig)){
    cat(sprintf("%*.3f", width_col, sqrt(x$eig[i])), sep = "")
  }
  cat("\n")

  cat(sprintf("%-*s", width_first_col, "Eigenvalue"), sep = "")
  for(i in 1:length(x$eig)){
    cat(sprintf("%*.3f", width_col, x$eig[i]), sep = "")
  }
  cat("\n")

  cat(sprintf("%-*s", width_first_col, "cumulative"), sep = "")
  for(i in 1:length(x$eig)){
    cat(sprintf("%*.3f", width_col, sum(x$eig[0:i])), sep = "")
  }
  cat("\n")

  sum_eigenvalues = sum(x$eig)
  inertias <- numeric(num_cols)
  cat(sprintf("%-*s", width_first_col, "% Inertia"), sep = "")
  for(i in 1:length(x$eig)){
    inertias[i] <- (x$eig[i]/sum_eigenvalues)*100
    cat(sprintf("%*d", width_col, round(inertias[i])), sep = "")
  }
  cat("\n")

  cat(sprintf("%-*s", width_first_col, "cumulative"), sep = "")
  for(i in 1:length(x$eig)){
    cat(sprintf("%*d", width_col, round(sum(inertias[0:i]))), sep = "")
  }
  cat("\n")

  for(i in 1:width_first_col){
    cat("-", sep = "")
  }
  for(i in 1:num_cols){
    for(j in 1:width_col){
      cat("-", sep = "")
    }
  }
  cat("\n", sep = "")

  invisible(x)
}
