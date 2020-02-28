#' RunCrossValidation
#'
#' Run variable selection nested into cross-validation. Source: https://uc-r.github.io/model_selection. See section 6.1 of Introduction to Statistical Learning.
#' @param data data.frame object. Data as prepared in "RunProject.R". Regressor values, predicted response, and residuals. No default.
#' @param k Numeric vector of length 1. Number of folds to use in k-fold cross-validation. Defaults to 10.
#' @param dir Character vector of length 1. Directory in which to store the cv-plot. Defaults to "./" 
#' @param save.plot Logical vector of length 1. If TRUE then the mean cv error plot -is saved to disk. Defaults to TRUE 
#' @export
RunCrossValidation <- function(data, k = 10, dir = "./", save.plot = TRUE) {
    set.seed(123)
    data <- data[, !names(data) %in% c("predicted", "r.student", "residuals")]
    n.regressors <- ncol(data) - 1
    folds <- sample(1:k, nrow(data), replace = TRUE)
    cv.errors <- matrix(NA, k, n.regressors, dimnames = list(NULL, paste(1:n.regressors)))
    for (j in 1:k) {
        ## perform best subset on rows not equal to j
        best_subset <- leaps::regsubsets(density ~ ., data[folds != j, ], nvmax = n.regressors)
        ## perform cross-validation
        for(i in 1:n.regressors) {
            pred.x <- PredictRegsubsets(best_subset, data[folds == j, ], id = i)
            cv.errors[j, i] <- mean((data$density[folds == j] - pred.x)^2)
        }
    }
    plot.data <- colMeans(cv.errors) %>% reshape2::melt()
    plot.data$id <- factor(as.numeric(rownames(plot.data)))
    plt <- plot.data %>%
        ggplot2::ggplot(ggplot2::aes(x = id, y = value, group = 1)) +
        ggplot2::geom_line() + ggplot2::geom_point()
    if (save.plot)
        suppressMessages({
            ggplot2::ggsave(paste0(dir, "cv_apr.png"), plt)
        })
    b <- which.min(colMeans(cv.errors))
    all.reg <- leaps::regsubsets(density ~ ., data = data, nvmax = n.regressors)
    final <- coef(all.reg, names(which.min(b))[1])
    
    return (final)
}
#' PredictRegsubsets
#'
#' Helper function for running variable selection and cross-validation. Source: https://uc-r.github.io/model_selection. See section 6.1 of Introduction to Statistical Learning.
#' @param object regsubsets object. Object returned from the leaps::regsubsets function. No default
#' @param newdata matrix. Data on which to make predictions. No default.
#' @param id Numeric vector of length 1. Model id. No default.
PredictRegsubsets <- function(object, newdata, id, ...) {
    form <- as.formula("density~.")
    mat <- model.matrix(form, newdata)
    coefi <- coef(object, id = id)
    xvars <- names(coefi)
    mat[, xvars] %*% coefi
}
