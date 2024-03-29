#' AnalyzeInfluence
#'
#' Wrapper to run Cook's distance, DFFITS, and DFBETA's and tabulate the influential points for further investigation.
#' @param data data.frame. Data as prepared in RunProject.R. No default.
#' @param dv Character vector of length 1. Dependent variable. No default. 
#' @param nm.chunks List. Chunks of predictor labels as prepared in RunProject.R. No default.
#' @param dir Character vector of length 1. Directory in which to save the plots and the influence table. Defaults to "./" 
#' @param save.plots Logical vector of length 1. If TRUE plots from the analysis are saved to disk. Defaults to TRUE. 
#' @export
AnalyzeInfluence <- function(data, dv, fit, nm.chunks,
                             dir = "./", save.plots = TRUE) {

    ## Run DFBETA analysis
    db <- lapply(
        X=nm.chunks,
        FUN=CreateDfbetaPlot,
        fit = fit,
        dir = dir,
        save.plot = save.plots
    )
    ## Run other outlier detection and tabulate
    cd <- CreateCooksDistancePlot(fit = fit, save.plot = save.plots, dir = dir)
    pdi <- CreateDffitsPlot(fit = fit, critical.value = 2 * sqrt(ncol(data)/nrow(data)),
                            save.plot = save.plots, dir = dir)
    influential.obs <- lapply(list(cd, pdi), function(l) l$influential.obs$Observation)
    points <- unique(unlist(influential.obs))
    d <- data[points, ]
    cols <- c(dv, "predicted", "residuals", "r.student")
    mc <- match(cols, names(d))
    df <- d %>% dplyr::select(-mc)
    knitr::opts_current$set(label = "influence")
    data.frame(Observation = points, df) %>%
        kableExtra::kable(format = "latex", row.names = FALSE,
                          caption = "Observations considered as outliers from the Cook's distance and \\textit{DFFITS} analysis.", booktabs = TRUE) %>%
        kableExtra::kable_styling(latex_options = "scale_down") %>%
        write(paste0(dir, "influence_table.tex"))

    return (as.numeric(points))
}
