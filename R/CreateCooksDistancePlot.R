#' CreateCooksDistancePlot
#'
#' Simple wrapper to Compute Cook's distance and generate plot for it.
#' @param data data.frame object. Data as prepared in RunProject.R. Regressor values, predicted response, and residuals. No default.
#' @param save.plot Logical vector of length 1. If TRUE then the plot is saved to disk. Defaults to TRUE 
#' @export
CreateCooksDistancePlot <- function(fit, save.plot = TRUE) {
    ## Error handling
    cd <- cooks.distance(fit)
    plt <- data.frame(cd = cd, observation = seq_along(cd)) %>%
        ggplot2::ggplot(ggplot2::aes(x = observation, y = cd)) +
        ggplot2::geom_bar(stat = "identity", width = 0.1, color = "black")
    if (save.plot)
        suppressMessages({
            ggplot2::ggsave("cd.png", plt)
        })
    return (plt)
}
