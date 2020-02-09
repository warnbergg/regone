#' CreateFittedAgainstResidualspLot
#'
#' Creates and returns a plot with fitted against residuals plot.
#' @param data data.frame. Containing the externally studentized residuals, and the fitted response varibles. No default.
#' @param save.plot Logical vector of length 1. If TRUE then the plot is saved to disk. Defaults to TRUE
#' @importFrom magrittr "%>%"
#' @export
CreateFittedAgainstResidualsPlot <- function(data, save.plot = TRUE) {
    ## Error handling
    if (!all(c("r.student", "predicted") %in% names(data)))
        stop("Columns r.student and predicted representing the externally studentized residuals and the fitted values, respectively, must be in plot.data.")
    plt <- data %>%
        ggplot2::ggplot(ggplot2::aes(x = predicted, y = r.student)) +
        ggplot2::geom_point() +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed")
    if (save.plot)
        suppressMessages({
            ggplot2::ggsave("far.png", plt, width = 5, height = 5)  
        })
    return(plt)
}
