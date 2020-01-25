## File for testing
library(devtools)
devtools::load_all()
`%>%` <- magrittr::`%>%`
data <- read.csv("../data/bodyfatmen.csv")
fit <- lm(data)
data$predicted <- predict(fit)
data$residuals <- residuals(fit)
## ------------------------ Residual Analysis -----------------------
x.vars <- all.vars(formula(fit))[-1]
nm.chunks <- Chunks(x.vars, 4)
plots <- lapply(nm.chunks, PartialResiduals, data = data)
added.variable.plots <- lapply(nm.chunks, AddedVariablePlots,
                               data = data, x.vars = x.vars, fit = fit)
partial.plots <- lapply(nm.chunks, PartialResiduals, data = data)
qq <- CreateQQPlot(data)
