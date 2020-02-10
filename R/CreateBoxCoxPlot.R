#' CreateBoxCoxPlot
#'
#' Simple wrapper for the lindia::gg_boxcox() function to maintain consistency in function layout. Adds option to save.plot.
#' @param fit lm object. Model fit to the data. No default.
#' @param save.plot Logical vector of length 1. If TRUE then the plot is saved to disk. Defaults to TRUE 
#' @param ... Additional arguments for the lindia::gg_boxcox() function.
#' @export
CreateBoxCoxPlot <- function(fit, save.plot = TRUE, ...) {
    ## Error handling
    plt <- lindia::gg_boxcox(fit, ...)
    if (save.plot)
        suppressMessages({
            ggplot2::ggsave("boxcox.png", plt)
        })
    return (plt)
}
