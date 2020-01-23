#' Chunks
#'
#' Splitting vector into specified n.chunks.
#' @param vec Any vector. Vector to split into chunks. No default.
#' @param n.chunks Integer vector of length 1. Number of chunks to split the vector into. No default.
#' @param exclude Character vector. Names of elements to remove from vector before splitting. Particularly relevant for use in package regone. Defaults to c("residuals", "predicted").
#' @export
Chunks <- function(vec, n.chunks, exclude = c("residuals", "predicted")) {
    vec <- vec[!vec %in% exclude]
    chunks <- split(vec, cut(seq_along(vec), n.chunks, labels = FALSE))
    return (chunks)
} 
