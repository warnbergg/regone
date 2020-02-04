#' AddedVariablePlots
#'
#' Plot added variable plots. Much credit to: https://stackoverflow.com/questions/59150905/is-there-a-ggplot2-analogue-to-the-avplots-function-in-r
#' @param fit lm object. LM model that has been fit to the data. No default.
#' @param save.plot Logical vector of length 1. If TRUE the added variable plots are saved to disk. Defaults to TRUE.
#' @export
CreateAddedVariablePlots <- function(fit, save.plot = TRUE) {
    plot.data.lst <- InvisibleAvPlots(fit)
    plot.data <- do.call(rbind, lapply(names(plot.data.lst), function(nm) {
        id <- rep(nm, nrow(plot.data.lst[[nm]]))
        df <- data.frame(id = id, plot.data.lst[[nm]])
        colnames(df) <- c("id", "x", "y")
        return(df)
    }))
    plt <- ggplot2::ggplot(plot.data, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point() +
        ggplot2::geom_smooth(method = 'lm', se = FALSE, 
                             color = 'red', formula = y ~ x, linetype = 'dashed') +
        ggplot2::facet_wrap(~id, scale = "free")
    if (save.plot)
        suppressMessages({
            ggplot2::ggsave("av.png", plt)
        })
    return(plt)
}
#' InvisibleAvPlots
#'
#' Gets the data from the avPlots function to use with ggplot.
#' @param fit lm object. LM model that has been fit to the data. No default.
#' @param ... Additional arguments.
#' @export
InvisibleAvPlots <- function(fit, ...) {
    ff <- tempfile()
    png(filename = ff)
    out <- car::avPlots(fit, ask = FALSE, ...)
    dev.off()
    unlink(ff)
    return (out)
}

