#' Run STM analysis
#' 
#' Run STM analysis and save output to aid interpretation.
#' @param input_file Path to file containing objects created by function \code{create_stm_input}.
#' @param output_dir Path to directory in which to save model output.
#' @param k Number of topics for STM analysis.
#' @param split_docs Logical indicating whether documents have been split into 3,000 word chunks, in which case a flag will be added as a prevalence covariate to indicate which chunks come from the same full document.
#' @param representative_docs If TRUE (the default), save full texts of most representative documents. If FALSE, save only a list of titles of the most representative documents.
#' @export

run_stm <- function(input_file, output_dir, k, split_docs=FALSE, representative_docs=TRUE) {
    require(stm)
    require(tm)
    require(data.table)
    require(stringr)
    require(texanaaid)
    
    load(file=input_file)
    
    prev <- "~year+geog"
    
    if(split_docs) {
        prev <- paste0(prev, "+report")
    }
    
    prev <- as.formula(prev)
    
    model_name <- output_dir
    
    assign(model_name, stm(stm_input$documents, stm_input$vocab, 
                       K=k, prevalence=prev, 
                       data=stm_input$meta))
    save(list=output_dir, file=file.path(output_dir, paste0(model_name, ".Rdata")))
    create_topic_model_report(get(model_name), 
                              file.path(output_dir), 
                              stm_input$meta, 
                              raw_reports , 
                              diff_cov="geog", 
                              diff_cov.value1="US", 
                              diff_cov.value2="EU", 
                              representative_docs=representative_docs)
}