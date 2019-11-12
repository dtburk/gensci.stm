#' Remove named entities using Python module ntlk
#' 
#' Remove named entities using Python module ntlk.
#' @param input_dir Directory containing text files to remove named entities from.
#' @param output_dir Directory in which to save texts with named entities removed.
#' @param nes_dir Directory in which to save named entities for review.
#' @export

remove_named_entities <- function(input_dir, output_dir, nes_dir) {
    py_script <- paste(system.file(package="gensci.stm"), "remove_named_entities.py", sep="/")
    cwd <- getwd()
    input_dir <- file.path(cwd, input_dir)
    output_dir <-file.path(cwd, output_dir)
    nes_dir <- file.path(cwd, nes_dir)
    command <- sprintf('python %s "%s" "%s" "%s"', py_script, input_dir, output_dir, nes_dir)
    system(command=command)
}