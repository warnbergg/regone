#' CreateQQPlot
#'
#' Simple wrapper for making a qq-plot of the residuals with ggplot.
#' @param data data.frame object. Data as prepared in RunProject.R. Regressor values, predicted response, and residuals. No default.
#' @param dir Character vector of lenght 1. Directory in which to store the plot. Ignored if save.plot is FALSE. Defaults to "."
#' @param save.plot Logical vector of length 1. If TRUE the plot is saved to disk. Defaults to TRUE. 
#' @export
CreateQQPlot <- function(data, dir = ".", save.plot = TRUE) {
    plt <- data %>%
        ggplot2::ggplot(ggplot2::aes(sample = residuals)) +
        ggplot2::stat_qq() +
        ggplot2::stat_qq_line() +
        ggplot2::ylab("Studentized residuals") +
        ggplot2::xlab("Theoretical quantiles")
    if (save.plot)
        suppressMessages({
            ggplot2::ggsave(paste0(dir, "qqplot.png"), plt)
        })
    return (plt)
}
