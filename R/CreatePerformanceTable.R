#' CreatePerformanceTable
#'
#' Summarize the test-set mean squared error for the different models.
#' @export
CreatePerformanceTable <- function() {
    dirs <- c("./main/", "./combo/", "./woinfluence/")
    r <- sapply(dirs, function(d) readRDS(paste0(d, "results.rds"))$test.mse)
    knitr::opts_current$set(label = "performance")
    tbl <- data.frame(Type = c("Full model", "With summary variable", "Summary variable without inf. obs."),
                      MSE = r) %>%
        kableExtra::kable(format = "latex", booktabs = TRUE,
                          caption = "Cross-validated MSE for models.",
                          row.names = FALSE) %>%
        write("performance.tex")
}
