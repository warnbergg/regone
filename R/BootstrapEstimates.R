#' BoostrapEstimates
#'
#' Uses the boot package to bootstrap the regression coefficients for our fitted model.
#' @param data data.frame. Data to fit the lm model to. No default.
#' @param ... Additional arguments for the boot function Description. Default/No default. 
#' @export
BootstrapEstimates <- function(data,  ...) {
    b <- boot::boot(data = data, statistic = BootCoefs, ...)
    return (list(summary = summary(b),
                 boot.object = b))
}
#' BootCoefs
#'
#' Statistic function for boot::boot.
#' @param data data.frame. Data to fit the lm model to. No default.
#' @param indices Helper argument for the boot::boot function.
BootCoefs <- function(data, indices) {
    data <- data[indices, ]
    fit <- lm(formula = density ~ ., data = data)
    return (coef(fit))
}
