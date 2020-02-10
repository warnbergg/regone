#' CreateQQPlot
#'
#' Simple wrapper for making a qq-plot of the residuals with ggplot.
#' @param residuals.df data.frame. Data frame with one column: the residuals from the multiple regression. No default.
#' @param save.plot Logical vector of length 1. If TRUE the plot is saved to disk. Defaults to TRUE. 
#' @export
CreateQQPlot <- function(residuals.df, save.plot = TRUE) {
    plt <- ggplot2::ggplot(residuals.df, ggplot2::aes(sample = residuals)) +
        ggplot2::stat_qq() +
        ggplot2::stat_qq_line() +
        ggplot2::ylab("Studentized residuals") +
        ggplot2::xlab("Theoretical quantiles")
    if (save.plot)
        suppressMessages({
            ggplot2::ggsave("qqplot.png", plt)
        })
    return (plt)
}
