

library(devtools)
devtools::load_all()
`%>%` <- magrittr::`%>%`
data <- read.csv(file = "../data/bodyfatmen.csv")
fit <- lm(formula = density ~ ., data = data)
b <- BootstrapEstimates(data = data, R = 2000)
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
})
far <- CreateFittedAgainstResidualsPlot(data = data)
## Variable transformation
bx <- CreateBoxCoxPlot(fit = fit)
trans <- CreateTransformedQQPlot(fit = fit, data = data)
## Outlier detection
cd <- CreateCooksDistancePlot(fit = fit)
di <- CreateDffitsPlot(fit = fit, critical.value = 2 * sqrt(ncol(data)/nrow(data)))
db <- lapply(nm.chunks, CreateDfbetaPlot, fit = fit)
## Multicolinearity 
mc.list <- GenerateMulticolinearityMeasures(data = data, fit = fit)
## Variable selection
apr <- RunAllPossibleRegression(fit = fit)

