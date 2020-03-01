#' RunProject
#'
#' Run full code for project.
#' @param verbose Logical vector of length 1. If TRUE messages are printed for each stage of the project, e.g. when running residual analysis. Defaults to TRUE.
#' @export
RunProject <- function(verbose = TRUE) {
    data <- read.csv("../data/bodyfatmen.csv")
    ## Run with all variables
    RunFullAnalysis(
        data = data,
        dir = "./main/",
        verbose = verbose
    )
    main.results <- readRDS("./main/results.RDS")
    inf.points <- main.results$influence.points
    ## Run without influence points
    RunFullAnalysis(
        data = data[-inf.points, ],
        dir = "./woinfluence/",
        verbose = verbose
    )
    message("\nProject finished.")
}
