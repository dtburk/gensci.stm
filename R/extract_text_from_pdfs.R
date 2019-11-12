#' Extract text from PDF files
#' 
#' Extract text from PDF files and save as VCorpus and/or in individual text files.
#' @param pdf_dir Directory containing PDF files to extract text from.
#' @param output_dir Directory to save output to.
#' @param save_Rdata Logical indicating whether to save VCorpus object named "all_reports" as an .Rdata file.
#' @param save_txt_files Logical indicating whether to save documents as individual text files.
#' @param language Language in which documents are written.
#' @export

extract_text_from_pdfs <- function(pdf_dir, output_dir, save_Rdata=TRUE, save_txt_files=FALSE, language="en") {
    ###############################################################################
    # The script below performs the following operations:
    
    # 1. Generates a list of file paths to each PDF document to be converted.
    # 2. Creates a custom PDF reader function to convert the PDFs to text.
    # 3. Applies the PDF reader function to each document.
    # 4. Saves the converted documents to individual text files.
    ###############################################################################
    
    require(tm)
    
    if(!dir.exists(output_dir)) dir.create(output_dir)
    
    # 1. Generate a list of file paths to each PDF document to be converted.
    file_paths <- list.files(pdf_dir, 
                             pattern="\\.pdf$|\\.PDF$", 
                             full.names=TRUE)
    # Note: `list.files` takes a folder/directory as its first argument
    # and returns a character vector of the filenames in that folder;
    # the "pattern" argument is optional, and filters the filenames
    # to only include those that match the specified regular expression;
    # the "full.names" argument is also optional, and if set to TRUE, the function
    # returns the full path to each file (relative to the working directory), 
    # rather than just the file name.
    
    
    # 2. Create a custom PDF reader function to convert the PDFs to text.
    my_readPDF <- readPDF(control=list(
        info=NULL, 
        text=c("-raw")
    )
    )
    
    # 3. Apply the PDF reader function to each document.
    all_reports <- lapply(file_paths, function(fp) {
        my_readPDF(elem=list(uri=fp), language=language)
    })
    # Note: `lapply` (for "list-apply") applies the function specified in its second 
    # argument to each element of the sequence (aka "iterable") specified in its 
    # first argument, and returns the result as a new sequence (of class "list", to 
    # be exact).
    
    
    # 4. Save the converted documents to individual text files.
    if(save_txt_files) {
        for(report in all_reports) {
            cat(paste0(report$content, collapse=" "),
                file=file.path(output_dir, 
                               gsub("\\.pdf$", ".txt", report$meta$id)
                )
            )
        }
    }
    
    if(save_Rdata) {
        save(all_reports, file=file.path(output_dir, "all_reports.Rdata"))
    }
    
    # Notes: 
    # `cat` can be used to print raw output to the console, or to a file
    # if the file argument is specified (as here). 
    
    # `paste0` pastes together characters/strings. Here, we only specify one 
    # object (a sequence of lines read from a PDF file and converted to text) 
    # with the collapse argument set to equal a single space, which combines 
    # the elements of the sequence into one character string, with the elements 
    # separated by single spaces. If we had not specified a value for collapse, 
    # `paste0` would have returned our sequence unmodified.
    
    # `gsub` looks in its third argument for the regular expression pattern 
    # in its first argument, and replaces it with the string specified in the 
    # second argument. Here, we replace the file suffix ".pdf" with ".txt"
}





