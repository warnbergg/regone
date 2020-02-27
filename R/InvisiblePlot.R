#' InvisiblePlot
#'
#' Gets the data from the plot call, but does not show the plot.
#' @param plot.call Function call. The call from which the plot is generated. No default 
#' @export
InvisiblePlot <- function(plot.call) {
    ff <- tempfile()
    png(filename = ff)
    out <- plot.call
    dev.off()
    unlink(ff)
    return (out)
}
