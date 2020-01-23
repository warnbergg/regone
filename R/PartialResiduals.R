#' Partial residuals
#'
#' Plot partial residuals with ggplot::facet_grid()
#' @param nms Character vector. Regressors to plot from data. No default.
#' @param data data.frame. Data for prediction. No default.
#' @export
PartialResiduals <- function(nms, data) {
    plt <- data[, c(nms, "density", "predicted", "residuals")] %>% 
        gather(key = "iv", value = "x", -density, -predicted, -residuals) %>%
        ggplot(aes(x = x, y = density)) + 
        geom_segment(aes(xend = x, yend = predicted), alpha = .2) +
        geom_point(aes(color = residuals)) +
        scale_color_gradient2(low = "blue", mid = "white", high = "red") +
        guides(color = FALSE) +
        geom_point(aes(y = predicted), shape = 1) +
        facet_grid(~ iv, scales = "free_x")
    ggsave(paste0(paste0(nms, collapse = "_"), ".png"), plt) 
}
