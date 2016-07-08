## Turn Rmarkdown version of workflow document into latex file that can be uploaded to F1000 Overleaf
#' @importFrom rmarkdown pandoc_convert
#' @importFrom knitr knit render_latex
#' @importFrom tools file_path_sans_ext
#' @export
markdownToLatex <- function(input) {
    
    if(!file.exists(input)) {
        stop("Cannot find file provided by the argument: 'input'")
    }
    input <- file_path_as_absolute(input)
    
    ## create file paths
    tmp_dir <- tempdir() 
    tmp_file <- tempfile(tmpdir = tmp_dir)
    output <- paste0(file_path_sans_ext(input), '.tex')
    
    ## generate a half-way-house, with latex code chunks, but everything else still markdown
    setwd(tmp_dir)
    render_latex()
    knit(input, 
         output = tmp_file)
    ## complete conversion, using our template to stick F1000 styles etc in place
    pandoc_convert(input = tmp_file,
                   to = 'latex',
                   options = c(paste0('--template=', system.file('resources/template_F1000SoftwareArticle.tex', 
                                                                 package = "BiocWorkflowTools"))),
                   output = output)
    ## if we created some plots in the temp dir, move them to the same directory as our output file
    if(file.exists(file.path(tmp_dir, 'figure'))) {
        file.copy(from = file.path(tmp_dir, 'figure'), 
                  to = dirname(input),
                  recursive = TRUE)
    }
    
    message("Output file has been written to:\n\t", output)
    
}