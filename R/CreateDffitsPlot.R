#' CreateDffitsPlot
#'
#' Computes the DFFITS statstic for the fitted model, plots them, and optionally saves the to disk.
#' @param fit lm object. Linear Model fit to the data. No default.
#' @param critical.value Numeric vector of length 1. Cutoff/critical value for DFFITS plot. Defaults to NULL, in which case no critical values are plotted.
#' @param save.plot Logical vector of length 1. If TRUE the DFFITS plot are saved to disk. Defaults to TRUE.
#' @export
CreateDffitsPlot <- function(fit, critical.value = NULL, save.plot = TRUE) {
    di <- dffits(fit)
    plot.data <- data.frame(DFFITS = di, Observation = seq_along(di))
    plt <- plot.data %>%
        ggplot2::ggplot(ggplot2::aes(x = Observation, y = DFFITS, label = Observation)) +
        ggplot2::geom_bar(stat = "identity", width = 0.1, color = "black") + 
        ggplot2::geom_label(data = plot.data %>%
                                dplyr::top_n(n = 3, wt = abs(DFFITS)) %>%
                                dplyr::filter(DFFITS >= 0),
                            nudge_y = 0.1) + 
        ggplot2::geom_label(data = plot.data %>%
                                dplyr::top_n(n = 3, wt = abs(DFFITS)) %>%
                                dplyr::filter(DFFITS < 0),
                            nudge_y = -0.1)
    if (!is.null(critical.value)) {
        plt <- plt + ggplot2::geom_hline(yintercept = c(critical.value, -critical.value),
                                         linetype = "dashed")  +
            ggplot2::geom_label(data = plot.data %>%
                                    dplyr::filter(abs(DFFITS) > critical.value) %>%
                                    dplyr::filter(DFFITS >= 0),
                                nudge_y = 0.1) + 
            ggplot2::geom_label(data = plot.data %>%
                                    dplyr::filter(abs(DFFITS) > critical.value) %>%
                                    dplyr::filter(DFFITS < 0),
                                nudge_y = -0.1)
    }
    if (save.plot)
        suppressMessages({
            ggplot2::ggsave("dffits.png", plt)
        })
    return (plt)
}
