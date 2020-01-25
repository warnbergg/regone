#' AddedVariablePlots
#'
#' Plot added variable plots. Much credit to: https://aosmith.rbind.io/2018/01/31/added-variable-plots/
#' @param nms Character vector. Regressors to be plotted against residuals. Each regressor is individually plotted against the externally studentized residuals. No default
#' @param data data.frame object. Data as prepared in "make.project.R". Regressor values, predicted response, and residuals. No default.
#' @param regressors Character vector. The regressor labels in data. No default.
#' @param fit lm object. Linear model fit to data. No default
#' @param save.plot Logical vector of length 1. If TRUE the added variable plots are saved to disk. Defaults to TRUE.
#' @export
CreateAddedVariablePlots <- function(nms, data, regressors, fit,
                                     save.plot = TRUE) {
    added.variable.dfs <- regressors %>%
        purrr::set_names() %>%
        purrr::map(~Summarize(data, regressors, .x))
    dfs.w.predictions <- added.variable.dfs %>%
        purrr::map(~broom::augment(fit, newdata = .x)) %>%
        purrr::map(~dplyr::mutate(.x, pred = .fitted))
    relevant.dfs <- dfs.w.predictions[nms]
    plot.data <- dplyr::bind_rows(lapply(names(relevant.dfs), function(nm) {
        df <- relevant.dfs[[nm]][, c(nm, "pred")]
        colnames(df) <- c("orig", "pred")
        df$id <- nm
        return (df)
    }))
    plot.data$density <- rep(data$density, length(nms))
    plt <- ggplot2::ggplot(plot.data, ggplot2::aes(x = orig, y = pred)) +
        ggplot2::geom_line() +
        ggplot2::geom_point(ggplot2::aes(x = orig, y = density)) +
        ggplot2::facet_wrap(~id, scales = "free") +
        ggplot2::theme_minimal()
    if (save.plot)
        ggplot2::ggsave(paste0(paste0(nms, collapse="_"), "_avp.png"), plt)
    return(plt)
}
#' Summarize
#'
#' Creates set where all other variables but var are repeated medians. Var is maintained in original state. Much credit to: https://aosmith.rbind.io/2018/01/31/added-variable-plots/.
#' @param data data.frame. The data to summarize from. No default.
#' @param allvars Character vector. Names of regressors in data. No default.
#' @param var Character vector of length 1. Variable in data to maintain/regressor to plot in the added variable plot. No default. 
#' @export
Summarize <- function(data, allvars, var) {
    s <- dplyr::summarise_at(data, dplyr::vars(dplyr::one_of(allvars), -dplyr::one_of(var)), median)
    var.w.medians <- cbind(dplyr::select_at(data, var), s)
    return (var.w.medians)
}
