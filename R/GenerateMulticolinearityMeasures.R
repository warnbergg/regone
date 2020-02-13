#' GenerateMulticolinearityMeasures
#'
#' Produces variance inflation factors, eigenvalue system analysis for the fitted model.
#' @param data data.frame Data used to fit the model. Used for pair-wise correlation analysis and eigenvalue system analysis. No default.
#' @param fit lm object. Fitted model. No default
#' @param save.corr.heatmap Logical vector of length 1. If TRUE creates and saves a heatmap of the regressor correlation matrix. Defaults to TRUE.
#' @export
GenerateMulticolinearityMeasures<- function(data, fit, create.corr.heatmap = TRUE) {
    d <- data[, !names(data) %in% c("predicted", "residuals", "r.student")]
    c <- cor(d)
    e <- eigen(c)$values
    k <- max(e)/min(e)
    v <- car::vif(fit)
    return.object <- setNames(list(e = e, k = k, v = v),
                              c("Eigen values", "Condition number",
                                "Variance Inflation Factors"))
    if (create.corr.heatmap) {
        ## http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-
        ## heatmap-r-software-and-data-visualization
        cormat <- reshape2::melt(c)
        hm <- cormat %>%
            ggplot2::ggplot(ggplot2::aes(x = Var2, y = Var1, fill = value, label = value),
                            color = "black", size = 4) +
            ggplot2::geom_tile() +
            ggplot2::geom_text(ggplot2::aes(Var2, Var1,
                                            label = round(value, 2)), color = "black", size = 4) +
            ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                           axis.title.y = ggplot2::element_blank())
        suppressMessages({
            ggplot2::ggsave("hm.png", hm)  
        })
    }       
    return (return.object)
}
