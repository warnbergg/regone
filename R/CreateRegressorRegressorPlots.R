#' CreateRegressorRegressorsPlots
#'
#' Simple wrapper for GGally::ggscatmat in order to save plots.
#' @param data data.frame. Data as prepared in RunProject.R. No default.
#' @param nms Character vector. Variables from data to plot. No default.
#' @param dir Character vector of length 1. Directory in which to store the regressor against regressors plots. Defaults to "./"
#' @param save.plot Logical vector of length 1. If TRUE then the plot is saved to disk. Defaults to TRUE 
#' @export
CreateRegressorRegressorsPlots <- function(data, nms, dir = "./", save.plot = TRUE) {
    ## Error handling
    plt <- data %>% dplyr::select(nms) %>%
        GGally:::ggscatmat()
    if (save.plot)
        suppressMessages({
            ggplot2::ggsave(paste0(dir, paste0(nms, collapse = "_"), "_regreg.png"), plt)
        })
    return (plt)
}
