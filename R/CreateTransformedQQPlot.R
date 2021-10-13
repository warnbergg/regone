#' CreateTransformedQQPlot
#'
#' Uses the Box-Cox generated lambda to fit and display the QQ-plot for the newly fitted model. Note that
#' a manual lambda is input.
#' @param fit lm object. Model fit to the data. No default.
#' @param iv Character vector. Independent variables. No default. 
#' @param dv Character vector of length 1. Dependent variable. Defaults to "density"
#' @param data data.frame. Data that the model was fit to. No default.
#' @param lambda Numeric vector of length 1. Lambda to transform the dependent variable in fit. Default to 0.9.
#' @param dir Character vector of lenght 1. Directory in which to store the plot. Ignored if save.plot is FALSE. Defaults to "."
#' @param save.plot Logical vector of length 1. If TRUE then the plot is saved to disk. Defaults to TRUE 
#' @export
CreateTransformedQQPlot <- function(fit, iv, dv = "density",
                                    data, lambda = 0.9,
                                    dir = "./", save.plot = TRUE) {
    ## Box-cox analysis
    bx <- CreateBoxCoxPlot(fit = fit, save.plot = FALSE)
    ## Test transformed fit
    iv.f <- paste(iv, collapse=" + ")
    tf <- paste0("(", dv, "^lambda - 1)/lambda", "~", iv.f)
    new.fit <- lm(formula = as.formula(tf), data = data)
    res <- data.frame(residuals = MASS::studres(new.fit))
    qq <- CreateQQPlot(res, save.plot = FALSE)
    qq <- qq + ggplot2::ggtitle("Quantile-Quantile plot")
    plt <- InvisiblePlot(gridExtra::grid.arrange(bx, qq, ncol = 2))
    if (save.plot)
        suppressMessages({
            ggplot2::ggsave(paste0(dir, "boxcox_fit.png"), plt,
                            width = 12, height = 5)
        })
    return (plt)
}
