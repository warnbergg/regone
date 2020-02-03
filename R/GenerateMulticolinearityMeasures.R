#' GenerateMulticolinearityMeasures
#'
#' Produces variance inflation factors, eigenvalue system analysis for the fitted model.
#' @param data data.frame Data used to fit the model. Used for pair-wise correlation analysis and eigenvalue system analysis. No default.
#' @param fit lm object. Fitted model. No default
#' @export
GenerateMulticolinearityMeasures<- function(data, fit) {
    d <- data[, !names(data) %in% c("predicted", "residuals", "r.student")]
    c <- cor(d)
    e <- eigen(c)$values
    k <- max(e)/min(e)
    v <- car::vif(fit)
    return.object <- setNames(list(c = c, e = e, k = k, v = v),
                              c("Correlation matrix", "Eigen values",
                                "Condition number", "Variance Inflation Factors"))
    return (return.object)
}
