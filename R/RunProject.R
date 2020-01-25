#' RunProject
#'
#' Run full code for project.
#' @param verbose Logical vector of length 1. If TRUE messages are printed for each stage of the project, e.g. when running residual analysis. Defaults to TRUE 
#' @export
RunProject <- function(verbose = TRUE) {
    library(devtools)
    devtools::load_all()
    `%>%` <- magrittr::`%>%`
    data <- read.csv("../data/bodyfatmen.csv")
    fit <- lm(data)
    data$predicted <- predict(fit)
    data$residuals <- residuals(fit)
    data$r.student <- rstudent(fit)
    ## ------------------------ Residual Analysis -----------------------
    if (verbose)
        message("Running residual analysis...")
    x.vars <- all.vars(formula(fit))[-1]
    nm.chunks <- Chunks(x.vars, 4)
    qq <- CreateQQPlot(data)
    pp <- lapply(nm.chunks, CreateFittedAgainstActualPlot, data = data)
    rar <- lapply(nm.chunks, CreateRegressorAgainstResidualsPlot, data = data)
    avp <- lapply(nm.chunks, CreateAddedVariablePlots, data = data, regressors = x.vars, fit = fit)
    ## ------------------------ Outlier detection -----------------------
    if (verbose)
        message("Running outlier detection analysis...")
    cd <- CreateCooksDistancePlot(data = data)
    if (verbose)
        message("Project finished.")
}
