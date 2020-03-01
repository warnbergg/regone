#' AddedVariablePlots
#'
#' Plot added variable plots. Much credit to: https://stackoverflow.com/questions/59150905/is-there-a-ggplot2-analogue-to-the-avplots-function-in-r
#' @param nms Character vector. Regressors to be plotted against residuals. Each regressor is individually plotted against the externally studentized residuals. No default.
#' @param fit lm object. LM model that has been fit to the data. No default.
#' @param dir Character vector of lenght 1. Directory in which to store the plot. Ignored if save.plot is FALSE. Defaults to "./"
#' @param save.plot Logical vector of length 1. If TRUE the added variable plots are saved to disk. Defaults to TRUE.
#' @export
CreateAddedVariablePlots <- function(nms, fit, dir = "./", save.plot = TRUE) {
    plot.data.lst <- InvisiblePlot(car::avPlots(fit, ask = FALSE))[nms]
    plot.data <- do.call(rbind, lapply(names(plot.data.lst), function(nm) {
        id <- rep(nm, nrow(plot.data.lst[[nm]]))
        df <- data.frame(id = id, plot.data.lst[[nm]]) %>%
            dplyr::mutate(observation = row.names(.)) %>%
            `colnames<-`(c("id", "x", "y", "observation"))
        return(df)
    }))
    label.data <- plot.data %>%
        dplyr::group_by(id) %>%
        dplyr::filter(x > quantile(x, 0.999) | x < quantile(x, 0.001))
    plt <- ggplot2::ggplot(plot.data, ggplot2::aes(x = x, y = y, label = observation)) +
        ggplot2::geom_point() +
        ggplot2::geom_point(data = label.data, colour = "blue") + 
        ggplot2::geom_smooth(method = 'lm', se = FALSE, 
                             color = 'black', formula = y ~ x,
                             linetype = 'dashed') +
        ggplot2::geom_text(data = label.data, nudge_y = 0.003) + 
        ggplot2::facet_wrap(~id, scale = "free_x")
    if (save.plot)
        suppressMessages({
            ggplot2::ggsave(paste0(dir, paste0(nms, collapse = "_"), "_av.png"), plt,
                            width = 10, height = 5)
        })
    return(plt)
}

