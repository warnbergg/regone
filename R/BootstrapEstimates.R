#' BoostrapEstimates
#'
#' Uses the boot package to bootstrap the regression coefficients for our fitted model.
#' @param data data.frame. Data to fit the lm model to. No default.
#' @param vars Named numeric vector. Coefficient values and corresponding variable names. No default.
#' @param dir Character vector of lenght 1. Directory in which to store the plot. Ignored if save.plot is FALSE. Defaults to "./"
#' @param digits Numeric vector of length 1. Digits to round to for values. Defaults to 5.
#' @param ... Additional arguments for the boot function Description. Default/No default. 
#' @export
BootstrapEstimates <- function(data, vars, dir = "./", digits = 5, ...) {
    data <- data %>% dplyr::select(-c(predicted, residuals, r.student))
    b <- boot::boot(data = data, statistic = BootCoefs, vars = names(vars)[-1], ...)
    ci.mat <- sapply(seq(vars), function(i) boot::boot.ci(b, index = i, type = "basic")$basic[, c(4, 5)])
    confs <- paste0("(", apply(round(ci.mat, digits), 2, paste, collapse = " to "), ")")
    knitr::opts_current$set(label = "coeffs")
    data.frame(p = names(vars),
               c = paste(round(vars, digits), confs)) %>%
        `colnames<-`(c("Predictor", "Coefficient (95 %)")) %>%
        kableExtra::kable(format = "latex", booktabs = TRUE,
                          caption = "Coefficients (95\\% CI) of final model.",
                          row.names = FALSE) %>%
        kableExtra::kable_styling(latex_options = "HOLD_position") %>%
        write(paste0(dir, "coeffs.tex"))
    return (list(summary = summary(b),
                 boot.object = b))
}
#' BootCoefs
#'
#' Statistic function for boot::boot.
#' @param data data.frame. Data to fit the lm model to. No default.
#' @param indices Helper argument for the boot::boot function.
#' @param vars Character vector. Predictors derived to be best from some variable selection / model building process. No default. 
BootCoefs <- function(data, indices, vars) {
    data <- data[indices, ][, c("density", vars)]
    fit <- lm(formula = density ~ ., data = data)
    return (coef(fit))
}
