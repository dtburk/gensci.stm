#' Remove state and country names
#'
#' Remove a set list of countries and US states from documents.
#' @param input_dir Directory containing text files from which to remove state and country names.
#' @param output_dir Directory to which to save documents with state and country names removed.
#' @export

remove_state_and_country_names <- function(input_dir, output_dir) {

    all_files <- list.files(input_dir)
    all <- lapply(all_files, function(x) {
        paste(scan(file=file.path(input_dir, x), what="character"), collapse=" ")
    })
    all <- stringr::str_replace_all(all, paste0(c(countries, us_states), collapse="|"), "")
    for(i in seq_along(all)) {
        cat(all[i], file=file.path(output_dir, all_files[i]))
    }
}
