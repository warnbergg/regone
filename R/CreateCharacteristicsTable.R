#' CreateCharacteristicsTable
#'
#' Wrapper for tableone::CreateTableOne() to produce table showing sample characteristics
#' @param data data.frame. Sample before predicted and residuals are added. No default
#' @param dir Character vector of length 1. Directory in which to save the table. Defaults to "./" 
#' @param ... Additional arguments for tableone::CreateTableOne.
#' @export
CreateCharacteristicsTable<- function(data, dir = "./", ...) {
    knitr::opts_current$set(label = "tblone")
    tableone::CreateTableOne(data = data, ...) %>%
        print(nonnormal = names(data), printToggle = FALSE) %>%
        kableExtra::kable(format = "latex", booktabs = TRUE,
                          caption = "Sample characteristics.") %>%
            write(paste0(dir, "tblone.tex"))
}
