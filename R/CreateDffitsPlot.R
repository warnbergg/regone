#' CreateDffitsPlot
#'
#' Computes the DFFITS statstic for the fitted model, plots them, and optionally saves the to disk.
#' @param fit lm object. Linear Model fit to the data. No default.
#' @param critical.value Numeric vector of length 1. Cutoff/critical value for DFFITS plot. Defaults to NULL, in which case no critical values are plotted.
#' @param save.plot Logical vector of length 1. If TRUE the DFFITS plot are saved to disk. Defaults to TRUE.
#' @export
CreateDffitsPlot <- function(fit, critical.value = NULL, save.plot = TRUE) {
    di <- dffits(fit)
    plot.data <- data.frame(di = di, obs = seq_along(di))
    plt <- plot.data %>%
        ggplot2::ggplot(ggplot2::aes(x = obs, y = di)) +
        ggplot2::geom_bar(stat = "identity", width = 0.1, color = "black")
    if (!is.null(critical.value))
        plt <- plt + ggplot2::geom_hline(yintercept = c(critical.value, -critical.value),
                                         linetype = "dashed")
    if (save.plot)
        suppressMessages({
            ggplot2::ggsave("dffits.png", plt)
        })
    return (plt)
}
