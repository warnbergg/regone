#' GenerateMulticolinearityMeasures
#'
#' Produces variance inflation factors, eigenvalue system analysis for the fitted model.
#' @param data data.frame Data used to fit the model. Used for pair-wise correlation analysis and eigenvalue system analysis. No default.
#' @param dv Character vector of length 1. Dependent variable. No default. 
#' @param fit lm object. Fitted model. No default
#' @param dir Character vector of lenght 1. Directory in which to store the plot. Ignored if save.plot is FALSE. Defaults to "."
#' @param save.plots Logical vector of length 1. If TRUE the VIF plot and correlation heatmap are saved to disk. Defaults to TRUE.
#' @export
GenerateMulticolinearityMeasures <- function(data, dv, fit, dir = "./",
                                             save.plots = TRUE) {
    d <- data[, !names(data) %in% c(dv, "predicted", "residuals", "r.student")]
    c <- cor(d)
    e <- eigen(c)$values
    k <- max(e)/min(e)
    v <- car::vif(fit)
    plot.data <- suppressMessages({
        reshape2::melt(data.frame(Regressor = names(d), vif = v, eigen = e))
    })
    plt <- PlotVar(plot.data)
    ## Save table for multicolinearity measures
    knitr::opts_current$set(label = paste0("mc"))
    tbl <- round(t(data.frame(Eigen = e, VIF = v)), 2) %>%
        kableExtra::kable(format = "latex", booktabs = TRUE,
                          caption = "Multicolinearity measures.") %>%
        kableExtra::kable_styling(latex_options = c("scale_down", "HOLD_position")) %>%
        write(paste0(dir, "mc.tex"))
    ## http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-
    ## heatmap-r-software-and-data-visualization
    cormat <- reshape2::melt(c)
    hm <- cormat %>%
        ggplot2::ggplot(ggplot2::aes(x = Var2, y = Var1, fill = value, label = value),
                        color = "white", size = 4) +
        ggplot2::geom_tile() +
        ggplot2::geom_text(ggplot2::aes(Var2, Var1,
                                        label = round(value, 2)), color = "white", size = 4) + 
        ggplot2::theme(legend.title = ggplot2::element_blank(),
                       axis.text.x = ggplot2::element_text(angle=30,hjust=1,vjust=1.0),
                       axis.text.y = ggplot2::element_text(size = 12))
    plts <- list(hm = hm, vif = plt)
    return.object <- setNames(list(e = e, k = k, v = v, plts = plts),
                              c("Eigen values", "Condition number",
                                "Variance Inflation Factors", "plts"))
    if (save.plots)
        for (nm in names(plts))
            suppressMessages({
                ggplot2::ggsave(paste0(dir, nm, ".png"), plts[[nm]])
            })
    
    return (return.object)
}
#' PlotVar
#'
#' Helper function to plot multicolinearity measures.
#' @param plot.data data.frame Use to plot the multicolinearity measures. No default.
#' @param critical.value Numeric vector. Values that define whether there is possibly multicolinearity. Defautls to c(5, 10)
#' @param var Character vector of length 1. The variable to plot. Defaults to "vif".
PlotVar <- function(plot.data, var = "vif") {
    ## Error handling
    if (!var %in% c("vif", "eigen"))
        stop ("Parameter var must be either vif or eigen")
    plot.data <- plot.data %>%
        dplyr::filter(variable == var)
    plt <- plot.data %>%
        ggplot2::ggplot(ggplot2::aes(x = Regressor, y = value)) +
        ggplot2::geom_bar(stat = "identity", width = 0.1, color = "black")+
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45, hjust = 0.3, vjust = 0.3)) +
        ggplot2::ylab("Variance Inflation Factor")
    if (var == "vif")
        plt <- plt + ggplot2::geom_hline(yintercept = c(5, 10), linetype = "dashed")
    else
        plt <- plt + ggplot2::scale_y_reverse(lim = c(9, 0))

    return (plt)
}
