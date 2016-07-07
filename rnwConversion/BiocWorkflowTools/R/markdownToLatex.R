
## Turn Rmarkdown version of workflow document into latex file that can be uploaded to F1000 Overleaf
#' @importFrom rmarkdown render pandoc_convert md_document
#' @importFrom tools file_path_sans_ext
#' @export
markdownToLatex <- function(input) {
    
    if(!file.exists(input)) {
        stop("Cannot find file provided by the argument: 'input'")
    }
    
    md_tmp <- tempfile()
    output <- paste0(file_path_sans_ext(input), '.tex')
    
    render(input, 
           output_format = md_document(preserve_yaml = TRUE, variant = 'markdown_github'),
           output_file = md_tmp)
    
    pandoc_convert(input = md_tmp,
                   to = 'latex',
                   options = c(paste0('--template=', system.file('resources/template_F1000SoftwareArticle.tex', 
                                                                 package = "BiocWorkflowTools"))),
                   output = output)
    
    message("Output file has been written to:\n\t", output)
    
}
