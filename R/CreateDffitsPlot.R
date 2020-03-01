#' CreateDffitsPlot
#'
#' Computes the DFFITS statstic for the fitted model, plots them, and optionally saves the to disk.
#' @param fit lm object. Linear Model fit to the data. No default.
#' @param critical.value Numeric vector of length 1. Plotted as a dashed line, as a indicator of influential points. Defaults to NULL, in which case the value is not plotted. If not NULL, the observations whose corresponding values exceed the critical value is labelled in the plot. Else, the observations with the top 3 highest values are labelled.
#' @param return.inf Logical vector of length 1. If TRUE the points that are "influential" are returned. Depends on the critical.value param: If NULL, then the top three points are returned, and if not null then the points whose values exceed the critical.value is returned. Defaults to TRUE.
#' @param dir Character vector of lenght 1. Directory in which to store the plot. Ignored if save.plot is FALSE. Defaults to "."
#' @param save.plot Logical vector of length 1. If TRUE the DFFITS plot are saved to disk. Defaults to TRUE.
#' @export
CreateDffitsPlot <- function(fit, critical.value = NULL,
                             return.inf = TRUE, dir = ".",
                             save.plot = TRUE) {
    di <- dffits(fit)
    plot.data <- data.frame(DFFITS = di, Observation = seq_along(di))
    plt <- plot.data %>%
        ggplot2::ggplot(ggplot2::aes(x = Observation, y = DFFITS, label = Observation)) +
        ggplot2::geom_bar(stat = "identity", width = 0.1, color = "black")
    label.data <- plot.data %>%
        dplyr::top_n(n = 3, wt = abs(DFFITS))
    if (!is.null(critical.value)) {
        label.data <- plot.data %>% dplyr::filter(abs(DFFITS) > critical.value)
        plt <- plt +
            ggplot2::geom_hline(yintercept = c(critical.value, -critical.value),
                                linetype = "dashed")
    }
    plt <- plt +
        ggplot2::geom_label(data = label.data %>%
                                dplyr::filter(DFFITS >= 0),
                            nudge_y = 0.1) +
        ggplot2::geom_label(data = label.data %>%
                                dplyr::filter(DFFITS < 0),
                            nudge_y = -0.1) + 
        ggplot2::geom_bar(data = label.data, stat = "identity", width = 0.1, color = "blue") +
        ggplot2::xlab("Observation") +
        ggplot2::ylab("DFFITS")
    if (save.plot)
        suppressMessages({
            ggplot2::ggsave(paste0(dir, "dffits.png"), plt)
        })
    return.object <- plt
    if (return.inf)
        return.object <- list(plt = plt,
                              influential.obs = label.data)
    return (return.object)
}
