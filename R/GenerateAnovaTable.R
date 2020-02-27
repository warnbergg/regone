#' GenerateAnovaTable
#'
#' Simple wrapper that generates an Analysis of Variance (ANOVA) table using the built-in anova function, and saves that table to disk.
#' @param fit lm object. Linear model that has been fit to the data. No default.
#' @param pretty.names Character vector. Column names for the analysis of variance table. Defaults to c("Sum sq", "Mean sq", "F value", "Pr(>F)")
#' @param ... Additional argument for anova().
#' @export
GenerateAnovaTable<- function(fit,
                              dir = ".",
                              pretty.names = c("Sum sq", "Mean sq",
                                               "F value", "Pr(>F)"),
                              ...) {
    ## Save table for multicolinearity measures
    knitr::opts_current$set(label = "anova")
    tbl <- data.frame(round(anova(fit), 3)) %>%
        dplyr::select(-Df)
    tbl[is.na(tbl)] <- "Not applicable"
    tbl %>%
        `colnames<-`(pretty.names) %>%
        kableExtra::kable(format = "latex", booktabs = TRUE,
                          caption = "ANOVA table for full model.") %>%
        kableExtra::kable_styling(latex_options = "scale_down") %>%
        write(paste0(dir, "anova.tex"))
}
