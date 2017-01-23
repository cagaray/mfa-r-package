#' @import ggplot2
PlotHelper <- function(data, group, xlab, ylab, title, label, fix.ratio = TRUE) {
    data <- data.frame(data)
    if (!is.null(group)) data <- cbind(data, group)

    if (is.null(label)) label <- rownames(data)
    plot <- ggplot(data = data, aes(x = X1, y = X2, label = label)) +
        labs(x = 'Dimension 1',
             y = 'Dimension 2',
             title = title) +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0)

    if (fix.ratio) plot <- plot + coord_fixed(1) 

    if (!is.null(group))
        plot <- plot + geom_text(aes(colour = group))
    else
        plot <- plot + geom_text()
    plot
}

PlotRawFactorScores <- function(fs, group, title, label) {
    PlotHelper(fs, group,
               'Dimension 1', 'Dimension 2', title, label)
}

CheckArg <- function(mfa, dim) {
    if (class(mfa) != 'mfa') stop('Object not of class mfa.')
    if (length(dim) != 2) stop('Plot only works with 2 dimensions.')
}    

#' @title Plot Factor Score
#' @param mfa an object of class "mfa"
#' @param group a factor vector, indicating which group each row
#'     belongs to
#' @param label a character vector, will be used to plot the points,
#'     default to row numbers when NULL
#' @param dim a integer vector indicating the dimensions to plot, the
#'     first element being x and second being y
#' @export
PlotFactorScores <- function(mfa, group = NULL, label = NULL, dim = c(1, 2)) {
    CheckArg(mfa, dim)
    fs <- mfa$factor.scores[ , dim]
    PlotRawFactorScores(fs, group, 'Factor Scores', label)
}

#' @title Plot Partial Factor Scores
#' @param mfa an object of class "mfa"
#' @param k the partial factor score of which table
#' @param group a factor vector, indicating which group each row
#'     belongs to
#' @param label a character vector, will be used to plot the points,
#'     default to row numbers when NULL
#' @param dim a integer vector indicating the dimensions to plot, the
#'     first element being x and second being y
#' @export
PlotPartialFactorScores <- function(mfa, k,
                                    group = NULL, label = NULL,
                                    dim = c(1, 2)) {
    CheckArg(mfa, dim)
    fs <- mfa$partial.factor.scores[[k]][ , dim]
    title <- paste('Partial factor scores of table', k)
    PlotRawFactorScores(fs, group, title, label)
}

#' @title Plot Variable Loadings
#' @param mfa an object of class "mfa"
#' @param var.group a factor vector, indicating which group each row
#'     (variable) belongs to
#' @param label a character vector, will be used to plot the points,
#'     default to row numbers when NULL
#' @param dim a integer vector indicating the dimensions to plot, the
#'     first element being x and second being y
#' @export
PlotLoadings <- function(mfa, var.group = NULL, label = NULL, dim = c(1, 2)) {
    CheckArg(mfa, dim)
    ld <- mfa$loadings[ , dim]
    PlotHelper(ld, var.group,
               'Dimension 1', 'Dimension 2',
               'Variable Loadings',
               label,
               fix.ratio = FALSE)
}

#' @title Plot Eigenvalues
#' @param mfa an object of class "mfa"
#' @export
PlotEig <- function(mfa) {
    barplot(mfa$eig,
            main = 'Eigenvalues of dimensions',
            names.arg = 1:length(mfa$eig))
}
