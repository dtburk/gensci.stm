#' Substitute alternate versions of some documents
#'
#' This function copies alternate versions of documents specified in the
#'   \code{replacement_scheme} from \code{replacements_dir} to
#'   \code{corpus_dir}, overwriting the corresponding document files in
#'   \code{corpus_dir}
#' @param corpus_dir Directory containing documents in individual text files.
#' @param replacements_dir Directory containing replacement documents.
#' @param replacement_scheme A named character vector in which the names are the filenames (without extensions) of documents to be replaced, and the values are the filenames (without extensions) of documents to serve as replacements.
#' @export

substitute_documents <- function(corpus_dir, replacements_dir, replacement_scheme) {
    file.copy(
      from = file.path(replacements_dir, paste0(replacement_scheme, ".txt")),
      to = file.path(corpus_dir, paste0(names(replacement_scheme), ".txt")),
      overwrite = TRUE
    )
}
