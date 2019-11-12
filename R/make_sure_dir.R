#' Create directory if it doesn't already exist
#' 
#' Remove repetitive "boilerplate" text from documents to minimize noise in the STM analysis.
#' @param dir Directory to create if it doesn't exist.
#' @export

make_sure_dir <- function(dir) {
    if(!dir.exists(dir)) dir.create(dir)
}