#' AnalyzeInfluence
#'
#' Wrapper to run Cook's distance and DFFITS, and tabulate the influential points for further investigation.
#' @param data data.frame. Data as prepared in RunProject.R. No default.
#' @param nm.chunks List. Chunks of predictor labels as prepared in RunProject.R. No default.
#' @param dir Character vector of length 1. Directory in which to save the plots and the influence table. Defaults to "./" 
#' @param save.plots Logical vector of length 1. If TRUE plots from the analysis are saved to disk. Defaults to TRUE. 
#' @export
AnalyzeInfluence <- function(data, fit, nm.chunks,
                             dir = "./", save.plots = TRUE) {
    
    cd <- CreateCooksDistancePlot(fit = fit, save.plot = save.plots, dir = dir)
    pdi <- CreateDffitsPlot(fit = fit, critical.value = 2 * sqrt(ncol(data)/nrow(data)),
                            save.plot = save.plots, dir = dir)
    influential.obs <- lapply(list(cd, pdi), function(l) l$influential.obs$Observation)
    points <- unique(unlist(influential.obs))
    df <- data[points, ] %>%
        dplyr::select(-c(density, predicted, residuals, r.student))
    knitr::opts_current$set(label = "influence")
    data.frame(Observation = points, df) %>%
        kableExtra::kable(format = "latex", row.names = FALSE,
                          caption = "Observations considered as outliers from the Cook's distance and \\textit{DFFITS} analysis.", booktabs = TRUE) %>%
        kableExtra::kable_styling(latex_options = "scale_down") %>%
        write(paste0(dir, "influence_table.tex"))

    return (as.numeric(points))
}
