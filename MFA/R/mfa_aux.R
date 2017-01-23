#' @export
print.mfa <- function(x) {
    cat("Result of MFA performed on",
        nrow(x$factor.scores),
        "individuals, with",
        nrow(x$loadings),
        "variables. \nThe result included the first",
        length(x$eig),
        "components.\n")
    invisible(x)
}
