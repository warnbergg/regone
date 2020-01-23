#' 
#'
#' Plot added variable plots. Much credit to: https://aosmith.rbind.io/2018/01/31/added-variable-plots/
#' @export
AddedVariablePlots <- function(nms, data, x.vars, fit) {
    data <- data[, c(nms, "density", "predicted", "residuals")]
    preddat_fun <- function(data, allvars, var) {
        s <- summarise_at(data, vars(one_of(allvars), -one_of(var)), median) 
        cbind(select_at(data, var), s)
    }
    ds <- x.vars[] %>%
        set_names() %>%
        map( ~preddat_fun(data, mod_vars, .x))
    preds <- ds %>%
        map(~augment(fit, newdata = .x) ) %>%
        map(~mutate(.x, lower = exp(.fitted - 2*.se.fit),
                    upper = exp(.fitted + 2*.se.fit),
                    pred = exp(.fitted)))
    plt <- ggplot(data, aes(x = .data[[variable]], y = pred)) +
        geom_line(size = 1) +
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .25) +
        geom_rug(sides = "b") +
        xlab(label = xlab)
    ggsave(paste0("partial", paste0(nms, collapse = "_"), ".png"), plt)     
}
