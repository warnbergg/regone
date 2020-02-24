#' CreateDfbetaPlot
#'
#' Computes DFBETA for each parameter of the fitted model, plots the dfbetas as specified in nms, and optionally saves them to disk.
#' @param nms Character vector. Regresors from which to calculate dfbeta. No default.
#' @param fit lm object. Linear Model fit to the data. No default.
#' @param critical.value Numeric vector of length 1. Cutoff/critical value for DFBETA plots. Defaults to NULL, in which case no cut-off value is plotted.
#' @param dir Character vector of lenght 1. Directory in which to store the plot. Ignored if save.plot is FALSE. Defaults to "."
#' @param save.plot Logical vector of length 1. If TRUE the DFBETA plots are saved to disk. Defaults to TRUE.
#' @export
CreateDfbetaPlot <- function(nms, fit, critical.value = NULL,
                             dir = ".", save.plot = TRUE) {
    db <- dfbeta(fit)
    plot.data <- reshape2::melt(db[, nms])
    colnames(plot.data) <- c("Observation", "Regressor", "Value")
    plt <- plot.data  %>%
        ggplot2::ggplot(ggplot2::aes(x = Observation, y = Value, label = Observation)) +
        ggplot2::geom_bar(stat = "identity", width = 0.1, color = "black") +
        ggplot2::geom_label(data = plot.data %>%
                                dplyr::group_by(Regressor) %>%
                                dplyr::top_n(n = 3, wt = abs(Value))) + 
        ggplot2::facet_wrap(~Regressor)
    if (!is.null(critical.value))
        plt <- plt + ggplot2::geom_hline(yintercept = c(critical.value, -critical.value),
                                         linetype = "dashed")
    if (save.plot)
        suppressMessages({
            ggplot2::ggsave(paste0(dir, paste0(nms, collapse = "_"), "_dfbeta.png"), plt)
        })
    return (plt)
}
