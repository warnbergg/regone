## File for testing
library(devtools)
devtools::load_all()
`%>%` <- magrittr::`%>%`
data <- read.csv("../data/bodyfatmen.csv")
fit <- lm(data)
data$predicted <- predict(fit)
data$residuals <- residuals(fit)
data$r.student <- rstudent(fit)
## ------------------------ Residual Analysis -----------------------
x.vars <- all.vars(formula(fit))[-1]
nm.chunks <- Chunks(x.vars, 4)
partial.plots <- lapply(nm.chunks, PartialResiduals, data = data)
qq <- CreateQQPlot(data)
regressor.againt.resid <- lapply(nm.chunks, CreateRegressorAgainstResidualsPlot,
                                 data = data)
