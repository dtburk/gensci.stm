#' Extract and save ngrams from text
#'
#' Extract and save ngrams from text to aid in the removal of boilerplate text from documents.
#' @param input_dir Directory containing text files to extract ngrams from.
#' @param ngram_dir Directory to save ngrams to.
#' @param language Language in which documents are written.
#' @export

get_ngrams <- function(input_dir, ngram_dir, language="en") {

    # If ngram_dir doesn't exist yet, create it
    if(!dir.exists(ngram_dir)) dir.create(ngram_dir)


    # Load texts
    docs <- tm::VCorpus(tm::DirSource(input_dir, pattern="\\.txt$"),
                    readerControl = list(language=language))

    for(i in seq_along(docs)) {
        txt <- docs[[i]]$content
        if(length(txt) > 1) {
            txt <- stringr::str_c(txt, collapse=" ")
        }
        docs[[i]]$content <- stringr::str_replace_all(txt, "[ \n\t\r]+", " ")
    }
    rm(txt)

    doc_ngrams <- list(length(docs))

    for(i_doc in 1:length(docs)) {
        cat(sprintf("Document %d of %d: ", i_doc, length(docs)))
        doc <- docs[[i_doc]]
        txt <- doc$content
        ngram_file <- file.path(ngram_dir, stringr::str_replace(doc$meta$id, "\\.txt$", ".Rdata"))
        ngram_vec <- character(0)
        ngram_tabs <- list()
        word_count <- stringr::str_count(txt, "\\S+")
        for(i in 8:min(100, word_count)) {
            cat(sprintf("%d ", i))
            ngram_tab <- ngram::get.phrasetable(ngram::ngram(txt, n=i))
            ngram_tab <- ngram_tab[ngram_tab$freq > 1, ]
            ngram_tabs[[paste0(i, "-grams")]] <- ngram_tab
        }
        cat("\n\n")
        save(ngram_tabs, file=ngram_file)
    }
}
