#' CreateDfbetaPlot
#'
#' Computes dfbeta for regressors and intercept, creates plot, and optionally saves to disk.
#' @param nms Character vector. Regresors from which to calculate dfbeta. No default.
#' @param fit lm object. Linear Model fit to the data. No default.
#' @param save.plot Logical vector of length 1. If TRUE the dfbeta plots are saved to disk. Defaults to TRUE.
#' @export
CreateDfbetaPlot <- function(nms, fit, save.plot = TRUE) {
    db <- dfbeta(fit)
    plt <- reshape2::melt(db[, nms]) %>%
        ggplot2::ggplot(ggplot2::aes(x = Var1, y = value)) +
        ggplot2::geom_bar(stat = "identity", width = 0.1, color = "black") +
        ggplot2::facet_wrap(~Var2)
    if (save.plot)
        suppressMessages({
            ggplot2::ggsave(paste0(paste0(nms, collapse = "_"), "_dfbeta.png"), plt)
        })
    return (plt)
}
