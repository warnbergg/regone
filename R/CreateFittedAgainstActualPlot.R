#' CreateFittedAgainstActualPlot
#'
#' Plot  Much credit to: https://drsimonj.svbtle.com/visualising-residuals
#' @param nms Character vector. Regressors to be plotted against residuals. Each regressor is individually plotted against the externally studentized residuals. No default
#' @param dv Character vector of length 1. Dependent variable. Defaults to "density" 
#' @param data data.frame object. Data as prepared in "make.project.R". Regressor values, predicted response, and residuals. No default.
#' @param dir Character vector of lenght 1. Directory in which to store the plot. Ignored if save.plot is FALSE. Defaults to "."
#' @param save.plot Logical vector of length 1. If TRUE then the plot is saved to disk. Defaults to TRUE 
#' @export
CreateFittedAgainstActualPlot <- function(nms, dv = "density", data,dir = ".", save.plot = TRUE) {
    d <- data[, c(nms, dv, "predicted", "residuals")]
    cols <- c(dv, "predicted", "residuals")
    mc <- match(cols, names(d))
    plt <- data[, c(nms, dv, "predicted", "residuals")] %>%
        dplyr::mutate(across(where(is.numeric), as.numeric)) %>%
        tidyr::gather(key = "iv", value = "x", -dplyr::all_of(mc)) %>%
        ggplot2::ggplot(ggplot2::aes_string(x = "x", y = dv)) + 
        ggplot2::geom_segment(ggplot2::aes(xend = x, yend = predicted), alpha = .2) +
        ggplot2::geom_point(ggplot2::aes(color = residuals)) +
        ggplot2::scale_color_gradient2(low = "blue", mid = "white", high = "red") +
        ggplot2::guides(color = FALSE) +
        ggplot2::geom_point(ggplot2::aes(y = predicted), shape = 1) +
        ggplot2::facet_grid(~ iv, scales = "free_x")
    if (save.plot)
        suppressMessages({
            ggplot2::ggsave(paste0(dir, paste0(nms, collapse = "_"), ".png"), plt)
        })
    return (plt)
}
