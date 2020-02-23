#' RunProject
#'
#' Run full code for project.
#' @param data data.frame. The data to print use for analysis. Defaults to read.csv("../data/bodyfatmen.csv")
#' @param dir Character vector of length 1. Directory in which to save analysis results. Defaults to "."
#' @param verbose Logical vector of length 1. If TRUE messages are printed for each stage of the project, e.g. when running residual analysis. Defaults to TRUE 
#' @export
RunProject <- function(data = read.csv("../data/bodyfatmen.csv"),
                       dir = ".",
                       verbose = TRUE) {
    fit <- lm(formula = density ~ ., data = data)
    b <- BootstrapEstimates(data = data, R = 10)
    data$predicted <- predict(fit)
    data$residuals <- residuals(fit)
    data$r.student <- rstudent(fit)
    if (verbose)
        message("Generate significance tests...")    
    st <- GenerateAnovaTable(fit = fit)
    if (verbose)
        message("Running residual analysis...")
    x.vars <- all.vars(formula(fit))[-1]
    nm.chunks <- Chunks(x.vars, 4)
    qq <- CreateQQPlot(data)
    ra <- lapply(nm.chunks, function(nms) {
        CreateFittedAgainstActualPlot(data = data, nms = nms)
        CreateRegressorAgainstResidualsPlot(data = data, nms = nms)
        CreateAddedVariablePlots(fit = fit, nms = nms)
    })
    far <- CreateFittedAgainstResidualsPlot(data)
    if (verbose)
        message("Detecting possible variable transformation...")
    bx <- CreateBoxCoxPlot(fit = fit)
    trans <- CreateTransformedQQPlot(fit = fit, data = data)
    if (verbose)
        message("Running outlier detection analysis...")
    cd <- CreateCooksDistancePlot(fit = fit)
    di <- CreateDffitsPlot(fit = fit, critical.value = 2 * sqrt(ncol(data)/nrow(data)))
    db <- lapply(nm.chunks, CreateDfbetaPlot, fit = fit)
    if (verbose)
        message("Computing multicolinearity measures...")
    mc.list <- GenerateMulticolinearityMeasures(data, fit)
    if (verbose)
        message("Running variable selection analysis...")
    apr <- RunAllPossibleRegression(fit = fit)
    if (verbose)
        message("Project finished.")
}
