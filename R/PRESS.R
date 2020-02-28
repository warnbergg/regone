#' PRESS
#'
#' Function taken from @Tim's answer here: https://stats.stackexchange.com/questions/248603/how-can-one-compute-the-press-diagnostic
#' @param fit lm object. LM model that has been fit to the data. No default.
#' @export
PRESS <- function(fit) {
    pr <- residuals(fit)/(1 - lm.influence(fit)$hat)
    return (sum(pr^2))
}
