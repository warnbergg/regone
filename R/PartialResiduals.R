#' Partial residuals
#'
#' Plot partial residuals with ggplot::facet_grid()
#' @param nms Character vector. Regressors to plot from data. No default.
#' @param data data.frame. Data for prediction. No default.
#' @export
PartialResiduals <- function(nms, data) {
    plt <- data[, c(nms, "density", "predicted", "residuals")] %>% 
        tidyr::gather(key = "iv", value = "x", -density, -predicted, -residuals) %>%
        ggplot2::ggplot(ggplot2::aes(x = x, y = density)) + 
        ggplot2::geom_segment(ggplot2::aes(xend = x, yend = predicted), alpha = .2) +
        ggplot2::geom_point(ggplot2::aes(color = residuals)) +
        ggplot2::scale_color_gradient2(low = "blue", mid = "white", high = "red") +
        ggplot2::guides(color = FALSE) +
        ggplot2::geom_point(ggplot2::aes(y = predicted), shape = 1) +
        ggplot2::facet_grid(~ iv, scales = "free_x")
    ggplot2::ggsave(paste0(paste0(nms, collapse = "_"), ".png"), plt) 
}
