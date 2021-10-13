#' RunCrossValidation
#'
#' Run variable selection nested into cross-validation. Source: https://uc-r.github.io/model_selection. See section 6.1 of Introduction to Statistical Learning.
#' @param dv Character vector of length 1. Dependent variable. No default. 
#' @param data data.frame object. Data as prepared in "RunProject.R". Regressor values, predicted response, and residuals. No default.
#' @param k Numeric vector of length 1. Number of folds to use in k-fold cross-validation. Defaults to 10.
#' @param dir Character vector of length 1. Directory in which to store the cv-plot. Defaults to "./" 
#' @param save.plot Logical vector of length 1. If TRUE then the mean cv error and all possible regression plots are saved to disk. Defaults to TRUE 
#' @export
RunCrossValidation <- function(dv, data, k = 10, dir = "./", save.plot = TRUE) {
    set.seed(123)
    data <- data[, !names(data) %in% c("predicted", "r.student", "residuals")]
    n.regressors <- ncol(data) - 1
    ## Partition the data set
    train.i <- caret::createDataPartition(y = data %>% dplyr::pull(dv), p = 0.75)
    train.validation <- data[train.i[[1]], ]
    test <- data[-train.i[[1]], ]
    folds <- sample(1:k, nrow(train.validation), replace = TRUE)
    ## Get the cv'd mean squared errors for each model
    cv.errors <- GetErrors(
        dv = dv,
        d = train.validation,
        k = k,
        p = n.regressors,
        folds = folds,
        dir = dir,
        save.plot = save.plot
    )
    ## Find the combination of predictors that minimize the cv'd MSE
    b <- which.min(colMeans(cv.errors))
    f <- paste0(dv, "~.")
    form <- as.formula(f)
    all.reg <- leaps::regsubsets(form, data = train.validation, nvmax = n.regressors)
    s <- summary(all.reg)
    ## Plot summary statistics
    plt <- data.frame(predictors = factor(1:n.regressors),
                      adj_R2 = s$adjr2,
                      Cp = s$cp,
                      BIC = s$bic) %>%
        tidyr::gather(statistic, value, -predictors) %>%
        dplyr::mutate(statistic = factor(statistic, labels = c("Adjusted R^2", "C(p)", "BIC"))) %>%
        ggplot2::ggplot(ggplot2::aes(x = predictors, y = value, group = 1)) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::xlab("Number of regresors") +
        ggplot2::ylab("Metric value") + 
        ggplot2::facet_wrap(~statistic, scale = "free")
    if (save.plot)
        suppressMessages({
            ggplot2::ggsave(paste0(dir, "apr.png"), plt, width = 10, height = 5)
        })
    ## Predict on the test set, and compute MSE
    p <- PredictRegsubsets(
        dv = dv,
        object = all.reg,
        newdata = test,
        i = names(which.min(b))[[1]]
    )
    test.mse <- mean((dplyr::pull(test, dv) - p)^2)
    final <- coef(all.reg, names(which.min(b))[1])
    
    return (list(vars = final,
                 test.mse = test.mse))
}
#' PredictRegsubsets
#'
#' Helper function for running variable selection and cross-validation. Source: https://uc-r.github.io/model_selection. See section 6.1 of Introduction to Statistical Learning.
#' @param object regsubsets object. Object returned from the leaps::regsubsets function. No default
#' @param newdata matrix. Data on which to make predictions. No default.
#' @param id Numeric vector of length 1. Model id. No default.
PredictRegsubsets <- function(dv, object, newdata, id, ...) {
    f <- paste0(dv, "~.")
    form <- as.formula(f)
    mat <- model.matrix(form, newdata)
    coefi <- coef(object, id = id)
    xvars <- names(coefi)
    mat[, xvars] %*% coefi
}
#' GetErrors
#'
#' Sets up the cross-validation, saves the cv-plot and runs best-subset regression on the full train-validation set.
#' @param d data.frame. Training and validation set. No default
#' @param k Numeric vector of length 1. Number of folds to use in k-fold cross-validation. No default.
#' @param p Numeric vector of length 1. Number of regressors in the data. No default.
#' @param folds Folds to use in CV. No default.
GetErrors <- function(dv, d, k, p, folds,
                      dir = "./", save.plot = TRUE) {
    cv.errors <- matrix(NA, k, p, dimnames = list(NULL, paste(1:p)))
    f <- paste0(dv, "~.")
    form <- as.formula(f)
    for (j in 1:k) {
        ## perform best subset on rows not equal to j
        best_subset <- leaps::regsubsets(form, d[folds != j, ], nvmax = p)
        ## perform cross-validation
        for(i in 1:p) {
            pred.x <- PredictRegsubsets(dv, best_subset, d[folds == j, ], id = i)
            cv.errors[j, i] <- mean((dplyr::pull(d, dv)[folds == j] - pred.x)^2)
        }
    }
    plot.d <- colMeans(cv.errors) %>% reshape2::melt()
    plot.d$id <- factor(as.numeric(rownames(plot.d)))
    plt <- plot.d %>%
        ggplot2::ggplot(ggplot2::aes(x = id, y = value, group = 1)) +
        ggplot2::geom_line() + ggplot2::geom_point() +
        ggplot2::xlab("Number of regressors") +
        ggplot2::ylab("MSE")
    if (save.plot)        
        suppressMessages({
            ggplot2::ggsave(paste0(dir, "cv_apr.png"), plt)
        })
    
    return (cv.errors)
}
