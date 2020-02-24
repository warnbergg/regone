#' CreateCooksDistancePlot
#'
#' Simple wrapper to Compute Cook's distance and generate plot for it.
#' @param data data.frame object. Data as prepared in RunProject.R. Regressor values, predicted response, and residuals. No default.
#' @param dir Character vector of lenght 1. Directory in which to store the plot. Ignored if save.plot is FALSE. Defaults to "."
#' @param save.plot Logical vector of length 1. If TRUE then the plot is saved to disk. Defaults to TRUE 
#' @export
CreateCooksDistancePlot <- function(fit, dir = ".", save.plot = TRUE) {
    ## Error handling
    cd <- cooks.distance(fit)
    plot.data <- data.frame(cd = cd, observation = seq_along(cd))
    plt <- plot.data %>%
        ggplot2::ggplot(ggplot2::aes(x = observation, y = cd, label = observation)) +
        ggplot2::geom_bar(stat = "identity", width = 0.1, color = "black") +
        ggplot2::geom_label(data = plot.data %>% dplyr::top_n(n = 3, wt = cd))
    if (save.plot)
        suppressMessages({
            ggplot2::ggsave(paste0(dir, "cd.png"), plt)
        })
    return (plt)
}
