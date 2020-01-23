## File for testing
library(devtools)
devtools::load_all()
data <- read.csv("../data/bodyfatmen.csv")
fit <- lm(data)
data$predicted <- predict(fit)
data$residuals <- residuals(fit)
## ------------------------ Residual Analysis -----------------------
x.vars <- all.vars(formula(fit))[-1]
nm.chunks <- Chunks(x.vars, 4)
plots <- lapply(nm.chunks, PartialResiduals, data = data)

