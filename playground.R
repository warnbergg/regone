## File for testing
library(devtools)
devtools::load_all()
`%>%` <- magrittr::`%>%`
data <- read.csv("../data/bodyfatmen.csv")
fit <- lm(formula = density ~ ., data = data)
b <- BootstrapEstimates(data, R = 10)
data$predicted <- predict(fit)
data$residuals <- residuals(fit)
data$r.student <- rstudent(fit)
## ------------------------ Residual Analysis -------------------------
x.vars <- all.vars(formula(fit))[-1]
nm.chunks <- Chunks(x.vars, 4)
qq <- CreateQQPlot(data)
ra <- lapply(nm.chunks, function(nms) {
    CreateFittedAgainstActualPlot(data = data, nms = nms)
    CreateRegressorAgainstResidualsPlot(data = data, nms = nms)
    CreateAddedVariablePlots(fit)
})
## ------------------------ Outlier detection -------------------------
cd <- CreateCooksDistancePlot(fit = fit)
di <- CreateDffitsPlot(fit = fit)
db <- lapply(nm.chunks, CreateDfbetaPlot, fit = fit)
## ------------------------ Multicolinearity --------------------------
mc.list <- GenerateMulticolinearityMeasures(data, fit)
## ------------------------ Variable selection ------------------------
apr <- RunAllPossibleRegression(fit = fit)

