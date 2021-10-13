#' CreateBoxCoxPlot
#'
#' Simple wrapper for the lindia::gg_boxcox() function to maintain consistency in function layout. Adds option to save.plot.
#' @param fit lm object. Model fit to the data. No default.
#' @param dir Character vector of lenght 1. Directory in which to store the plot. Ignored if save.plot is FALSE. Defaults to "."
#' @param save.plot Logical vector of length 1. If TRUE then the plot is saved to disk. Defaults to TRUE 
#' @param ... Additional arguments for the lindia::gg_boxcox() function.
#' @export
CreateBoxCoxPlot <- function(fit, dir = "./", save.plot = TRUE, ...) {
    ## Error handling
    message("hej")
    plt <- lindia::gg_boxcox(fitted.lm = fit)
    if (save.plot)
        suppressMessages({
            ggplot2::ggsave(paste0(dir, "boxcox.pntg"), plt)
        })
    return (plt)
}
