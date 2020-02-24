#' CreateRegressorAgainstResidualsPlot 
#'
#' Prepares data to plottable format, and plots as well as optionally saves faceted plot of regressors against externally studentized residuals.
#' @param data data.frame object. Data as prepared in "make.project.R". Regressor values, predicted response, and residuals. No default.
#' @param nms Character vector. Regressors to be plotted against residuals. Each regressor is individually plotted against the externally studentized residuals. No default
#' @param dir Character vector of lenght 1. Directory in which to store the plot. Ignored if save.plot is FALSE. Defaults to "."
#' @param save.plot Logical vector of length 1. If TRUE then the plot is saved to disk. Defaults to TRUE 
#' @export
CreateRegressorAgainstResidualsPlot <- function(data, nms, dir = ".", save.plot = TRUE) {
    ## Error handling
    plot.data <- suppressMessages({reshape2::melt(data[, nms], id = ) %>%
        dplyr::mutate(r.student = rep(data$r.student, length(nms)))})
    plt <- plot.data %>%
        ggplot2::ggplot(ggplot2::aes(x = value, y = r.student)) +
        ggplot2::geom_point() +
        ggplot2::geom_hline(yintercept = 0) +
        ggplot2::facet_wrap(~variable, scale = "free", ncol = 1)
    if (save.plot)
        suppressMessages({
            ggplot2::ggsave(paste0(paste0(nms, collapse = "_"), "_far.png"), plt)
        })
    return(plt)
}
