#' AnalyzeInfluence
#'
#' Wrapper to run Cook's distance, DFFITS, and DFBETA computations, and tabulate the influential points for further investigation.
#' @param data data.frame. Data as prepared in RunProject.R. No default.
#' @param nm.chunks List. Chunks of predictor labels as prepared in RunProject.R. No default. 
#' @param save.plots Logical vector of length 1. If TRUE plots from the analysis are saved to disk. Defaults to TRUE. 
#' @export
AnalyzeInfluence <- function(data, fit, nm.chunks,
                             dir = "./", save.plots = TRUE) {
    cd <- CreateCooksDistancePlot(fit = fit, save.plot = save.plots)
    pdi <- CreateDffitsPlot(fit = fit, critical.value = 2 * sqrt(ncol(data)/nrow(data)),
                            save.plot = save.plots)
    db <- lapply(nm.chunks, CreateDfbetaPlot, fit = fit,
                 save.plot = save.plots)
    cd.pdi.obs <- lapply(list(cd, pdi), function(l) l$influential.obs$Observation)
    db.obs <- lapply(db, function(l) l$influential.obs$Observation)
    points <- table(unlist(c(cd.pdi.obs, db.obs)))
    top.points <- as.numeric(names(sort(points, decreasing = TRUE)[1:5]))
    df <- data[top.points, ] %>%
        dplyr::select(-c(density, predicted, residuals, r.student))
    data.frame(Observation = top.points, df) %>%
        kableExtra::kable(format = "latex", row.names = FALSE,
                          caption = "Top five most frequenly occuring points from the Cook's distance, \\textit{DFBETA}, and \\textit{DFFITS} analysis.", booktabs = TRUE) %>%
        kableExtra::kable_styling("scale_down") %>%
        write(paste0(dir, "influence_table.tex"))
}
