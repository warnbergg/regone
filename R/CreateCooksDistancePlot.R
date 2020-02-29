#' CreateCooksDistancePlot
#'
#' Simple wrapper to Compute Cook's distance and generate plot for it.
#' @param data data.frame object. Data as prepared in RunProject.R. Regressor values, predicted response, and residuals. No default.
#' @param critical.value Numeric vector of length 1. Plotted as a dashed line, as a indicator of influential points. Defaults to NULL, in which case the value is not plotted. If not NULL, the observations whose corresponding values exceed the critical value is labelled in the plot. Else, the observations with the top 3 highest values are labelled.
#' @param return.inf Logical vector of length 1. If TRUE the points that are "influential" are returned. Depends on the critical.value param: If NULL, then the top three points are returned, and if not null then the points whose values exceed the critical.value is returned. Defaults to TRUE.
#' @param dir Character vector of lenght 1. Directory in which to store the plot. Ignored if save.plot is FALSE. Defaults to "."
#' @param save.plot Logical vector of length 1. If TRUE then the plot is saved to disk. Defaults to TRUE 
#' @export
CreateCooksDistancePlot <- function(fit, critical.value = NULL,
                                    return.inf = TRUE, dir = ".",
                                    save.plot = TRUE) {
    ## Error handling
    cd <- cooks.distance(fit)
    plot.data <- data.frame(cd = cd, observation = seq_along(cd))
    plt <- plot.data %>%
        ggplot2::ggplot(ggplot2::aes(x = observation, y = cd, label = observation)) +
        ggplot2::geom_bar(stat = "identity", width = 0.1, color = "black")
    label.data <- plot.data %>% dplyr::top_n(n = 3, wt = cd)
    if (!is.null(critical.value)) {
        label.data <- plot.data %>% dplyr::filter(cd > critical.value)
        plt <- plt + ggplot2::geom_hline(yintercept = c(critical.value, -critical.value),
                                         linetype = "dashed")
    }
    plt <- plt + ggplot2::geom_label(data = label.data)  
    if (save.plot)
        suppressMessages({
            ggplot2::ggsave(paste0(dir, "cd.png"), plt)
        })
    return.object <- plt
    if (return.inf)
        return.object <- list(plt = plt,
                              influential.obs = label.data)
    return (return.object)
}
