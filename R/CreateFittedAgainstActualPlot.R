#' CreateFittedAgainstActualPlot
#'
#' Plot  Much credit to: https://drsimonj.svbtle.com/visualising-residuals
#' @param nms Character vector. Regressors to be plotted against residuals. Each regressor is individually plotted against the externally studentized residuals. No default
#' @param data data.frame object. Data as prepared in "make.project.R". Regressor values, predicted response, and residuals. No default.
#' @param save.plot Logical vector of length 1. If TRUE then the plot is saved to disk. Defaults to TRUE 
#' @export
CreateFittedAgainstActualPlot <- function(nms, data, save.plot = TRUE) {
    plt <- data[, c(nms, "density", "predicted", "residuals")] %>% 
        tidyr::gather(key = "iv", value = "x", -density, -predicted, -residuals) %>%
        ggplot2::ggplot(ggplot2::aes(x = x, y = density)) + 
        ggplot2::geom_segment(ggplot2::aes(xend = x, yend = predicted), alpha = .2) +
        ggplot2::geom_point(ggplot2::aes(color = residuals)) +
        ggplot2::scale_color_gradient2(low = "blue", mid = "white", high = "red") +
        ggplot2::guides(color = FALSE) +
        ggplot2::geom_point(ggplot2::aes(y = predicted), shape = 1) +
        ggplot2::facet_grid(~ iv, scales = "free_x")
    if (save.plot)
        ggplot2::ggsave(paste0(paste0(nms, collapse = "_"), ".png"), plt)
    return (plt)
}
