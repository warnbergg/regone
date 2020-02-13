#' CreateTransformedQQPlot
#'
#' Uses the Box-Cox generated lambda to fit and display the QQ-plot for the newly fitted model. Note that
#' a manual lambda is input.
#' @param fit lm object. Model fit to the data. No default.
#' @param data data.frame. Data that the model was fit to. No default.
#' @param lambda Numeric vector of length 1. Lambda to transform the dependent variable in fit. Default to 0.9.
#' @param save.plot Logical vector of length 1. If TRUE then the plot is saved to disk. Defaults to TRUE 
#' @export
CreateTransformedQQPlot<- function(fit, data, lambda = 0.9,
                                   save.plot = TRUE) {
    ## Error handling
    bx <- CreateBoxCoxPlot(fit, save.plot = FALSE)
    new.fit <- lm(formula = ((density^lambda - 1)/lambda) ~ ., data = data)
    res <- data.frame(residuals = MASS::studres(new.fit))
    qq <- CreateQQPlot(res, save.plot = FALSE)
    qq <- qq + ggplot2::ggtitle("Quantile-Quantile plot")
    plt <- gridExtra::grid.arrange(bx, qq, ncol = 2)
    if (save.plot)
        suppressMessages({
            ggplot2::ggsave("boxcox_fit.png", plt,
                            width = 12, height = 5)
        })
    return (plt)
}
