#' RunProject
#'
#' Run full code for project.
#' @param verbose Logical vector of length 1. If TRUE messages are printed for each stage of the project, e.g. when running residual analysis. Defaults to TRUE.
#' @export
RunProject <- function(verbose = TRUE) {
    data <- read.csv("../data/bodyfatmen.csv")
    ## Run with all variables
    if (verbose)
        message("
Running full analysis cycle with all variables and observations...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
")
    RunFullAnalysis(
        data = data,
        dir = "./main/",
        verbose = verbose 
    )
    if (verbose)
        message("
Running full analysis cycle with summary variable of weight, abdomen, thigh, and hip ...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
")
    RunFullAnalysis(
        data = data %>%
            dplyr::mutate(combo = (hip*thigh*abdomen)/weight) %>%
            dplyr::select(-c(thigh, abdomen, weight, hip)),
        dir = "./combo/",
        verbose = verbose
    )
    sum.results <- readRDS("./combo/results.rds")
    inf.points <- sum.results$influence.points
    ## Run without influence points
    if (verbose)
        message("
Running full analysis cycle with summary variable and without influential points ...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
")
    d <- data %>%
            dplyr::mutate(combo = (hip*thigh*abdomen)/weight) %>%
            dplyr::select(-c(thigh, abdomen, weight, hip))
    d <- d[-unique(inf.points), ]
    RunFullAnalysis(
        data = d,
        dir = "./woinfluence/",
        verbose = verbose
    )
    CreatePerformanceTable()
    message("\nProject finished.")
}
