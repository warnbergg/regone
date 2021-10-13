#' RunFullAnalysis
#'
#' Run a full cycle of residual analysis, multicolinearity analysis etc.
#' @param data data.frame. The data to print use for analysis. Defaults to read.csv("../data/bodyfatmen.csv")
#' @param dv Character vector of length 1. Column name of dependent variable. No default. 
#' @param dir Character vector of length 1. Directory in which to save analysis results. Defaults to "."
#' @param n.chunks Integer vector of length 1. The number of chunks of variables to run with in e.g. regressor against regressor analysis. Defaults to 4. 
#' @param transform Logical vector of length 1. If TRUE, transformation is found with Box-Cox test. Defaults to TRUE. 
#' @param verbose Logical vector of length 1. If TRUE messages are printed for each stage of the project, e.g. when running residual analysis. Defaults to TRUE
#' @export
RunFullAnalysis <- function(data, dv, dir = "./",
                            n.chunks = 4,
                            transform = FALSE, verbose = TRUE) {
    if (!dir.exists(dir))
        dir.create(dir)
    verbose.dir <- "current working directory\n"
    if (dir != "./")
        verbose.dir <- paste0(dir, "\n")
    CreateCharacteristicsTable(data = data, dir = dir)
    if (verbose)
        message(paste("Sample characteristics table saved to", verbose.dir))
    iv <- names(data)[!names(data) %in% dv]
    f <- paste(dv, "~", paste(iv, collapse=" + "))
    fit <- lm(formula = as.formula(f), data = data)
    data$predicted <- predict(fit)
    data$residuals <- MASS::studres(fit)
    data$r.student <- rstudent(fit)
    if (verbose)
        message("Generating significance tests...")    
    st <- GenerateAnovaTable(fit = fit, dir = dir)
    if (verbose)
        message("Running residual analysis...")
    x.vars <- all.vars(formula(fit))[-1]
    nm.chunks <- Chunks(vec = x.vars, n.chunks = n.chunks)
    qq <- CreateQQPlot(data = data, dir = dir)
    ra <- lapply(nm.chunks, function(nms) {
        nms <- nm.chunks[[2]]
        CreateFittedAgainstActualPlot(data = data, dv = dv, nms = nms, dir = dir)
        CreateRegressorAgainstResidualsPlot(data = data, nms = nms, dir = dir)
        CreateAddedVariablePlots(fit = fit, nms = nms, dir = dir)
        CreateRegressorRegressorsPlots(data = data, nms = nms, dir = dir)
    })
    far <- CreateFittedAgainstResidualsPlot(data = data, dir = dir)
    p <- PRESS(fit)
    if (transform) {
        if (verbose)
            message("Detecting possible variable transformation...")
        trans <- CreateTransformedQQPlot(fit = fit, iv = iv, dv = dv,
                                         data = data, dir = dir)   
    }
    if (verbose)
        message("Running outlier detection analysis...")
    influence.points <- AnalyzeInfluence(
        data = data,
        dv = dv,
        fit = fit,
        nm.chunks = nm.chunks,
        dir = dir
    )
    if (verbose)
        message("Computing multicolinearity measures...")
    mc.list <- GenerateMulticolinearityMeasures(
        data = data,
        dv = dv,
        fit = fit,
        dir = dir
    )
    if (verbose)
        message("Running variable selection analysis and bootstrapping...")
    cv.list <- RunCrossValidation(dv = dv, data = data, k = 2, dir = dir)
    b <- BootstrapEstimates(
        dv = dv,
        data = data,
        vars = cv.list$vars,
        dir = dir,
        R = 1000
    )
    results <- list(
        fit = fit,
        st = st,
        qq = qq,
        ra = ra,
        far = far,
        press = p,
        #trans = trans,
        influence.points = influence.points,
        mc.list = mc.list,
        vars = cv.list$vars,
        b = b,
        test.mse = cv.list$test.mse
    )
    saveRDS(results, paste0(dir, "results.rds"))
    if (verbose) 
        message(paste("Plots and results were saved to", verbose.dir))
}

