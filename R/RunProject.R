#' RunProject
#'
#' Run full code for project.
#' @param verbose Logical vector of length 1. If TRUE messages are printed for each stage of the project, e.g. when running residual analysis. Defaults to TRUE 
#' @export
RunProject <- function(verbose = TRUE) {
    data <- read.csv("../data/bodyfatmen.csv")
    fit <- lm(data)
    data$predicted <- predict(fit)
    data$residuals <- residuals(fit)
    data$r.student <- rstudent(fit)
    if (verbose)
        message("Running residual analysis...")
    x.vars <- all.vars(formula(fit))[-1]
    nm.chunks <- Chunks(x.vars, 4)
    qq <- CreateQQPlot(data)
    ra <- lapply(nm.chunks, function(nms) {
        CreateFittedAgainstActualPlot(data = data, nms = nms)
        CreateRegressorAgainstResidualsPlot(data = data, nms = nms)
        CreateAddedVariablePlots(fit)
    })
    if (verbose)
        message("Running outlier detection analysis...")
    cd <- CreateCooksDistancePlot(fit = fit)
    di <- CreateDffitsPlot(fit = fit)
    db <- lapply(nm.chunks, CreateDfbetaPlot, fit = fit)
    if (verbose)
        message("Running multicolinearity analysis...")
    mc.list <- GenerateMulticolinearityMeasures(data, fit)
    if (verbose)
        message("Running variable selection analysis...")
    apr <- RunAllPossibleRegression(fit = fit)
    if (verbose)
        message("Project finished.")
}
