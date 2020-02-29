#' CreateDfbetaPlot
#'
#' Computes DFBETA for each parameter of the fitted model, plots the dfbetas as specified in nms, and optionally saves them to disk.
#' @param nms Character vector. Regresors from which to calculate dfbeta. No default.
#' @param fit lm object. Linear Model fit to the data. No default.
#' @param critical.value Numeric vector of length 1. Plotted as a dashed line, as a indicator of influential points. Defaults to NULL, in which case the value is not plotted. If not NULL, the observations whose corresponding values exceed the critical value is labelled in the plot. Else, the observations with the top 3 highest values are labelled.
#' @param n.largets Numeric vector of length 1. Number of values to consider when labelling the n largest points. Depends on the setting of critical.value: If null, then used; if not, then ignored. Defaults to 3. 
#' @param return.inf Logical vector of length 1. If TRUE the points that are "influential" are returned. Depends on the critical.value param: If NULL, then the top three points are returned, and if not null then the points whose values exceed the critical.value is returned. Defaults to TRUE.
#' @param dir Character vector of lenght 1. Directory in which to store the plot. Ignored if save.plot is FALSE. Defaults to "."
#' @param save.plot Logical vector of length 1. If TRUE the DFBETA plots are saved to disk. Defaults to TRUE.
#' @export
CreateDfbetaPlot <- function(nms, fit, critical.value = NULL,
                             n.largest = 3, return.inf = TRUE,
                             dir = ".", save.plot = TRUE) {
    db <- dfbeta(fit)
    plot.data <- reshape2::melt(db[, nms])
    colnames(plot.data) <- c("Observation", "Regressor", "Value")
    plt <- plot.data  %>%
        ggplot2::ggplot(ggplot2::aes(x = Observation, y = Value, label = Observation)) +
        ggplot2::geom_bar(stat = "identity", width = 0.1, color = "black")
    label.data <- plot.data %>%
        dplyr::group_by(Regressor) %>%
        dplyr::top_n(n = n.largest, wt = abs(Value))
    if (!is.null(critical.value)) {
        label.data <- plot.data %>%
            dplyr::group_by(Regressor) %>%
            dplyr::filter(abs(Value) > critical.value)
        plt <- plt +
            ggplot2::geom_hline(yintercept = c(critical.value, -critical.value),
                                linetype = "dashed")
    }
    plt <- plt +
        ggplot2::geom_label(data = label.data %>%
                                dplyr::filter(Value >= 0),
                            nudge_y = max(label.data$Value)*0.05) + 
        ggplot2::geom_label(data = label.data %>%
                                dplyr::filter(Value < 0),
                            nudge_y = min(label.data$Value)*0.05) + 
        ggplot2::facet_wrap(~Regressor)
    if (save.plot)
        suppressMessages({
            ggplot2::ggsave(paste0(dir, paste0(nms, collapse = "_"), "_dfbeta.png"), plt)
        })
    return.object <- plt
    if (return.inf)
        return.object <- list(plt = plt,
                              influential.obs = label.data)
    return (return.object)
}
