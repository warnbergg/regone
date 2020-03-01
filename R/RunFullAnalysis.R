#' RunFullAnalysis
#'
#' Run a full cycle of residual analysis, multicolinearity analysis etc.
#' @param data data.frame. The data to print use for analysis. Defaults to read.csv("../data/bodyfatmen.csv")
#' @param dir Character vector of length 1. Directory in which to save analysis results. Defaults to "."
#' @param verbose Logical vector of length 1. If TRUE messages are printed for each stage of the project, e.g. when running residual analysis. Defaults to TRUE 
#' @export
RunFullAnalysis <- function(data = read.csv("../data/bodyfatmen.csv"),
                            dir = "./",
                            verbose = TRUE) {
    if (!dir.exists(dir))
        dir.create(dir)
    verbose.dir <- "current working directory\n"
    if (dir != "./")
        verbose.dir <- paste0(dir, "\n")
    CreateCharacteristicsTable(data = data, dir = dir)
    if (verbose)
        message(paste("Sample characteristics table saved to", verbose.dir))
    fit <- lm(formula = density ~ ., data = data)
    data$predicted <- predict(fit)
    data$residuals <- MASS::studres(fit)
    data$r.student <- rstudent(fit)
    if (verbose)
        message("Generating significance tests...")    
    st <- GenerateAnovaTable(fit = fit, dir = dir)
    if (verbose)
        message("Running residual analysis...")
    x.vars <- all.vars(formula(fit))[-1]
    nm.chunks <- Chunks(vec = x.vars, n.chunks = 4)
    qq <- CreateQQPlot(data = data, dir = dir)
    ra <- lapply(nm.chunks, function(nms) {
        CreateFittedAgainstActualPlot(data = data, nms = nms, dir = dir)
        CreateRegressorAgainstResidualsPlot(data = data, nms = nms, dir = dir)
        CreateAddedVariablePlots(fit = fit, nms = nms, dir = dir)
        CreateRegressorRegressorsPlots(data = data, nms = nms, dir = dir)
    })
    d <- data %>% dplyr::mutate(height = 1/height^2)
    f <- lm(formula = density ~ ., data = d)
    av_height <- CreateAddedVariablePlots(fit = f, nms = "height", dir = dir)
    far <- CreateFittedAgainstResidualsPlot(data = data, dir = dir)
    xp <- PRESS(fit)
    if (verbose)
        message("Detecting possible variable transformation...")
    trans <- CreateTransformedQQPlot(fit = fit, data = data, dir = dir)
    if (verbose)
        message("Running outlier detection analysis...")
    influence.points <- AnalyzeInfluence(
        data = data,
        fit = fit,
        nm.chunks = nm.chunks,
        dir = dir
    )
    if (verbose)
        message("Computing multicolinearity measures...")
    mc.list <- GenerateMulticolinearityMeasures(data = data, fit = fit, dir = dir)
    if (verbose)
        message("Running variable selection analysis and bootstrapping...")
    RunAllPossibleRegression(fit = fit, dir = dir)
    vars <- RunCrossValidation(data = data, dir = dir)
    b <- BootstrapEstimates(data = data, vars = vars, dir = dir, R = 1000)
    results <- list(
        fit = fit,
        st = st,
        qq = qq,
        ra = ra,
        far = far,
        press = p,
        trans = trans,
        influence.points = influence.points,
        mc.list = mc.list,
        vars = vars,
        b = b
    )
    saveRDS(results, paste0(dir, "results.Rds"))
    if (verbose) 
        message(paste("Plots and results were saved to", verbose.dir))
}
