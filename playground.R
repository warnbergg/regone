library(devtools)
devtools::load_all()
`%>%` <- magrittr::`%>%`
data <- read.csv(file = "../data/bodyfatmen.csv")
fit <- lm(formula = density ~ ., data = data)
data$predicted <- predict(object = fit)
data$residuals <- MASS::studres(fit)
data$r.student <- rstudent(model = fit)
## Significance tests
st <- GenerateAnovaTable(fit = fit)
## Residual Analysis
x.vars <- all.vars(formula(fit))[-1]
nm.chunks <- Chunks(vec = x.vars, n.chunks = 4)
qq <- CreateQQPlot(data)
ra <- lapply(nm.chunks, function(nms) {
    CreateFittedAgainstActualPlot(data = data, nms = nms)
    CreateRegressorAgainstResidualsPlot(data = data, nms = nms)
    CreateAddedVariablePlots(fit = fit, nms = nms)
    CreateRegressorRegressorsPlots(data = data, nms = nms)
})
far <- CreateFittedAgainstResidualsPlot(data = data)
p <- PRESS(fit)
## Variable transformation
bx <- CreateBoxCoxPlot(fit = fit)
trans <- CreateTransformedQQPlot(fit = fit, data = data)
## Outlier detection
inf.points <- AnalyzeInfluence(data = data, fit = fit, nm.chunks = nm.chunks)
## Multicolinearity 
mc.list <- GenerateMulticolinearityMeasures(data = data, fit = fit)
## Variable selection
vars <- RunCrossValidation(data = data)
## Bootstrapping coefficients
b <- BootstrapEstimates(data = data, vars = vars, R = 1000)

