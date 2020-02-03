#' RunAllPossibleRegression
#'
#' Runs "all possible regression" analysis using the olsrr::ols_step_best_subset() function, plots the
#' "relevant" measures and saves to disk.
#' @param fit lm object. Full model fit to the data. No default.
#' @param save.plot Logical vector of length 1. If TRUE then the plot is saved to disk. Defaults to TRUE 
#' @export
RunAllPossibleRegression <- function(fit, save.plot = TRUE) {
    subsets <- olsrr::ols_step_best_subset(fit)
    plt <- with(subsets, reshape2::melt(data.frame(n, aic, adjr, cp, sbic), id = "n")) %>%
        dplyr::mutate(variable = factor(variable, labels = c("AIC",
                                                             "Adjusted R-squared",
                                                             "Cp criterion",
                                                             "BIC"))) %>%
        ggplot2::ggplot(ggplot2::aes(x = n, y = value)) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::facet_wrap(~variable, scale = "free")

    if (save.plot)
        suppressMessages({
            ggplot2::ggsave("variable_selection.png", plt)
        })
    return(plt)
}
