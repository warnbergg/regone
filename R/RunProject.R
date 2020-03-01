#' RunProject
#'
#' Run full code for project.
#' @param run.apr Logical vector of length 1. If TRUE all possible regression is run. Note that this is quite time and resource consuming. Defaults to FALSE. 
#' @param verbose Logical vector of length 1. If TRUE messages are printed for each stage of the project, e.g. when running residual analysis. Defaults to TRUE.
#' @export
RunProject <- function(run.apr = FALSE, verbose = TRUE) {
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
        run.apr = run.apr,
        verbose = verbose
    )
    main.results <- readRDS("./main/results.rds")
    inf.points <- main.results$influence.points
    ## Run without influence points
    if (verbose)
        message("
Running full analysis cycle with without influential observations...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
")
    RunFullAnalysis(
        data = data[-inf.points, ],
        dir = "./woinfluence/",
        run.apr = run.apr,
        verbose = verbose
    )
    message("\nProject finished.")
}
