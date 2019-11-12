#' Create objects for input to STM functions
#' 
#' Create and save objects to be used as input to STM functions.
#' @param input_dir Directory containing text files from which to create STM input.
#' @param output_file Full filepath, including filename, at which to save .Rdata file containing STM input objects.
#' @param bounds An integer vector of length 2 indicating the lower and upper bounds (inclusive) for the number of documents in which a token must appear to be included in the analysis.
#' @param word_lengths An integer vector of length 2 indicating lower and upper bounds (inclusive) for the number of characters a token must have to be included in the analysis.
#' @param split_docs Logical indicating whether documents should be split into 3,000 word sections prior to STM analysis.
#' @export

create_stm_input <- function(input_dir, output_file, bounds=c(2, Inf), 
                             word_lengths=c(3, 24), split_docs=FALSE) {

    require(stm)
    require(tm)
    require(data.table)
    require(stringr)
    require(texanaaid)
    
    all_reports <- VCorpus(DirSource(input_dir))
    
    all_reports <- as.VCorpus(lapply(all_reports, function(x) {
        x$meta$id <- str_replace(x$meta$id, "_no_ref.txt$", ".txt")
        return(x)
    }))
    
    titles <- sapply(all_reports, meta, "id", USE.NAMES=FALSE)
    
    # Rename gensci_meta as d
    d <- gensci_meta
    
    # setdiff(str_replace(titles, "\\.txt$", ""), d$New.Names.for.Pdfs)
    
    # Remove EC_1999b, EC_2016, and all but the first of the series
    # for She Figures reports and NSF "Women, Minorities, and Persons with Disabilities..." reports
    # and EC_2016 (study period ends at 2015)
    she_figures <- paste0(c("EC_2003", "EC_2006", "EC_2009d", "EC_2013c", "EC_2015f"), ".txt")
    nsf <- paste0(c("NSF_1994", "NSF_2000", "NSF_2002", "NSF_2013", "NSF_2015"), ".txt")
    to_drop <- c("EC_1999b.txt", "EC_2016.txt", tail(she_figures, -1), tail(nsf, -1))
    all_reports <- all_reports[!(titles %in% to_drop)]
    titles <- titles[!(titles %in% to_drop)]
    title_order <- order(titles)
    titles <- titles[title_order]
    all_reports <- all_reports[title_order]
    setorder(d, New.Names.for.Pdfs)
    d <- d[New.Names.for.Pdfs %in% str_replace(titles, "\\.txt$", ""), ]
    
    all <- all_reports
    rm(all_reports)
    
    # Remove the space between dash and next word in hyphenated word
    all <- tm_map(all, content_transformer(function(x) str_replace_all(x, "(?<=\\w)- (?=\\w)", "-")))
    
    ## Remove web-addresses
    url_pattern <- "(http)?www[^ ]+"
    
    all <- tm_map(all, content_transformer(function(x) str_replace_all(x, url_pattern, "")))
    
    ## Change "per cent" to "percent"
    all <- tm_map(all, content_transformer(function(x) str_replace_all(x, "per cent", "percent")))
    
    
    # Make raw_pages for most representative document output
    # all_raw <- all
    # which_non_empty <- lapply(all, function(x) {
    #     pages <- str_split(x$content, "\\f")[[1]]
    #     return(str_detect(pages, "\\w"))
    # })
    
    
    
    if(split_docs) {
        all_pages <- list()
        all_pages_years <- integer()
        all_pages_geog <- character()
        all_pages_titles <- character()
        all_pages_report <- character()
        
        for(i in seq_along(all)) {
            doc <- all[[i]]
            pages <- str_split(doc$content, "\\f")[[1]]
            doc_title <- str_replace(doc$meta$id, "\\.txt$", "")
            metadata <- d[New.Names.for.Pdfs==doc_title, ]
            doc_year <- metadata$Year
            doc_geog <- metadata$US.EU.UN
            token_pattern <- "[\\S]+"
            word_count <- 0L
            page_grp <- integer()
            # Aggregate until you have 3000 words
            for(j in seq_along(pages)) {
                word_count <- word_count + str_count(pages[j], token_pattern)
                page_grp <- c(page_grp, j)
                if(word_count >= 3000 | (word_count > 0 & j==length(pages))) {
                    page_grp_title <- sprintf("%s_%d_to_%d", doc_title, head(page_grp, 1), tail(page_grp, 1))
                    to_add <- PlainTextDocument(x=str_c(pages[page_grp], collapse=" "), id=page_grp_title, language="en")
                    all_pages <- c(all_pages, list(to_add))
                    all_pages_years <- c(all_pages_years, doc_year)
                    all_pages_geog <- c(all_pages_geog, doc_geog)
                    all_pages_titles <- c(all_pages_titles, page_grp_title)
                    all_pages_report <- c(all_pages_report, doc_title)
                    word_count <- 0L
                    page_grp <- integer()
                } else {
                    next
                }
            }
        }
        
        all <- as.VCorpus(all_pages)
        raw_reports <- sapply(all, content)
        d <- data.table(title=all_pages_titles, year=all_pages_years, geog=factor(all_pages_geog), 
                                         report=factor(all_pages_report))
        rm(all_pages, all_pages_titles, all_pages_years, all_pages_geog, all_pages_report)
    } else {
        raw_reports <- sapply(all, content)
        d <- d[ , .(title=Title, year=Year, geog=US.EU.UN)]
    }
    
    custom_stopwords <- c("appendix", "annex", "andor", "dqg", "embo", "dfg", "jhqghu", "wklv", "copyright", "pdf", 
                          "typesetting", "typographical", "europe", "european", "america", "american", "january", 
                          "february", "march", "april", "may", "june", "july", "august", "september", "septembre", 
                          "october", "octobre", "november", "novembre", "december", "decembre")
    
    # Convert to lowercase
    all <- tm_map(all, content_transformer(function(x) tolower(x)))
    
    ## Remove Numbers
    all <- tm_map(all, removeNumbers)
    
    # Remove punctuation
    all <- tm_map(all, content_transformer(function(x) remove_punctuation(x)))
    
    # Change "title ix" to "titleix"
    all <- tm_map(all, content_transformer(function(x) str_replace_all(x, "\\btitle ix\\b", "titleix")))
    
    # Transliterate non-ASCII characters
    all <- tm_map(all, content_transformer(function(x) iconv(x, to="ASCII//TRANSLIT")))
    
    # Convert British to American spellings
    british_to_american <- british_to_american_spellings()
    plurals <- paste0(british_to_american, "s")
    names(plurals) <- paste0(names(british_to_american), "s")
    british_to_american <- c(british_to_american, plurals)
    
    for(i in seq_along(all)) {
        for(w in names(british_to_american)) {
            all[[i]]$content <- 
                str_replace_all(all[[i]]$content, 
                                sprintf("\\b%s\\b", w), 
                                british_to_american[w])
        }
    }
    
    ## Remove Stopwords
    all <- tm_map(all, removeWords, stopwords("english"))
    all <- tm_map(all, removeWords, custom_stopwords)
    
    # Pre-stem "hand-stemming"; modifications to ensure words are combined or 
    # distinguished the way we want
    
    ## Make a post-stem conversion dictionary
    post_stem_conversion <- character(0)
    
    ## Distinguish "equality" and "equalities" from "equal"
    ### By default these are all stemmed to "equal"
    ### Replace "equalities" and "equality" with "equalit"
    all <- tm_map(all, content_transformer(
        function(x) str_replace_all(x, "\\bequalities\\b|\\bequality\\b", "equalit")
    ))
    ### After stemming, replace "equalit" with "equaliti"
    post_stem_conversion["\\bequalit\\b"] <- "equaliti"
    ##
    
    ## Combine all versions of root "scien"
    ### This includes "science", "sciences", "scientific", "scientist", and 
    ### "scientists"
    ### Replace all these with "scien"
    all <- tm_map(all, content_transformer(
        function(x) str_replace_all(x, paste0("\\b", 
                                              c("sciences*", "scientific", 
                                                "scientists*"), 
                                              "\\b", collapse="|"), 
                                    "scien")))
    ##
    
    ## Stem all forms of technologi* at techn*
    all <- tm_map(all, content_transformer(
        function(x) str_replace_all(x, "\\btechnolog[a-z]+\\b", 
                                    "techn")))
    ##
    
    ## Stem all forms of entrepreneur at entrepr*
    all <- tm_map(all, content_transformer(
        function(x) str_replace_all(x, "\\bentrepreneur[a-z]*\\b", 
                                    "entrepr")))
    ##
    
    ## Distinguish all forms of "engineer" from "engine"
    all <- tm_map(all, content_transformer(
        function(x) str_replace_all(x, "engineer", 
                                    "engineert")))
    post_stem_conversion["engineert"] <- "engineer"
    
    ## Stem
    all <- tm_map(all, stemDocument)
    
    ## Perform post-stem conversion
    post_stem_conversion["\\bgrante\\b"] <- "grant"
    for(w in names(post_stem_conversion)) {
        all <- tm_map(all, 
                            content_transformer(function(x) str_replace_all(
                                x, w, post_stem_conversion[w])))
    }
    
    ## Convert stemmed British spellings to catch variations on words
    ## not on the original list
    for(i in seq_along(all)) {
        for(w in names(british_to_american)) {
            all[[i]]$content <- 
                str_replace_all(all[[i]]$content, 
                                sprintf("\\b%s\\b", wordStem(w)), 
                                wordStem(british_to_american[w]))
        }
    }
    
    
    ## Remove post-stem stopwords
    all <- tm_map(all, removeWords, post_stem_custom_stopwords)
    
    
    custom_tokenizer <- Token_Tokenizer(tokenize)
    
    dtm <- DocumentTermMatrix(all, control=
                                  list(
                                      tokenize=custom_tokenizer, 
                                      #stemming=TRUE, 
                                      bounds=list(global=bounds), 
                                      wordLengths=word_lengths#,
                                      #stopwords=custom_stopwords
                                      #weighting=weightTfIdf
                                  )
    )
    
    # dim(dtm)
    
    ### With stemming and wordLengths=c(2, 24): 21,583 terms
    ### With stemming and wordLengths=c(2, 24) and bounds c(3, Inf): 14,651 terms
    ### Without stemming and wordLengths=c(2, 24): 30,162
    ### Without stemming and with doc bounds=c(2, 112): 30,131
    ### Without stemming, without bounds, without wordLengths restrictions: 87,797
    
    stm_input <- readCorpus(corpus=dtm, type="slam")
    
    # empty_docs <- apply(as.matrix(dtm)==0, 1, all)
    
    stm_input$meta <- d
    
    stm_input <- prepDocuments(stm_input$documents, stm_input$vocab, stm_input$meta)
    
    # rm(all_pages_metadata, d, head_foot, metadata, tr, adobe_texts, all, 
    #    all_pages, all_reports, custom_stopwords, custom_tokenizer, 
    #    doc, doc_geog, doc_year, i, j, pages, titles, to_add, 
    #    to_remove, to_replace, txt, url_pattern, prep_texts, 
    #    replacement)
    
    save(stm_input, raw_reports, file=output_file) # used to save dtm as well, but not sure I need it anymore
}