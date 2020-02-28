#' RunAllPossibleRegression
#'
#' Runs "all possible regression" analysis using the olsrr::ols_step_best_subset() function, plots the
#' "relevant" measures and saves to disk.
#' @param fit lm object. Full model fit to the data. No default.
#' @param performance.measures Character vector. Performance measures of candidate model to include in the performance table. Defaults to c("adjr", "aic", "sbic", "cp").
#' @param pretty.labels Character vector. Column names for performance measures in performance table. Defaults to c("Adjusted R-squared", "AIC", "BIC", "C(p)")
#' @param dir Character vector of lenght 1. Directory in which to store the plot. Ignored if save.plot is FALSE. Defaults to "./"
#' @param save.plot Logical vector of length 1. If TRUE then the plot is saved to disk. Defaults to TRUE 
#' @export
RunAllPossibleRegression <- function(fit, performance.measures = c("adjr", "aic", "sbic", "cp"),
                                     pretty.labels = c("Adjusted R-squared", "AIC", "BIC", "C(p)"),
                                     dir = "./", save.plot = TRUE) {
    subsets <- olsrr::ols_step_best_subset(fit)
    tbl <- as.data.frame(subsets)
    ## Performance table
    MakeTable(tbl = tbl,
              labels.list = setNames(c("mindex", "n", performance.measures),
                                     nm = c("Model index", "p", pretty.labels)),
              file.name = "variable_selection.tex",
              caption = "Performance measures for candidate models, where p refers to the number of regressors",
              dir = dir, 
              row.names = FALSE)
    ## Model indexes and regressors
    MakeTable(tbl = tbl,
              labels.list = setNames(c("mindex", "predictors"), nm = c("Model index", "Regressors")),
              file.name = "model_table.tex",
              caption = "Subset model corresponding to each model index in Table 3.",
              dir = dir,
              row.names = FALSE)
    ## Performance plot
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
            ggplot2::ggsave(paste0(dir, "variable_selection.png"), plt)
        })

    return(list(plt = plt,
                subsets = subsets))
}
#' MakeTable
#'
#' Runs kable over a table and writes to disk.
#' @param tbl data.frame Data to be kable'd. No default.
#' @param labels.list List. Containing the columns to subset, and labelled with the pretty name of that column. No default
#' @param file.name Character vector of length 1. File name of the table. No default
#' @param ... Additional arguments for kableExtra::kable. 
MakeTable <- function(tbl, labels.list, file.name, dir = "./", ...) {
    tbl[, unlist(labels.list)] %>%
        `colnames<-`(names(labels.list)) %>%
        kableExtra::kable(format = "latex", booktabs = TRUE, ...) %>%
        kableExtra::kable_styling(latex_options = "scale_down") %>%
        write(paste0(dir, file.name))
}
