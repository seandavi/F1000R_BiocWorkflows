#' Convert R markdown to F1000 latex
#'
#' Turn Rmarkdown version of workflow document into latex file that can be uploaded to F1000 Overleaf
#' 
#' @param input path to Rmd file to be converted
#' 
#' @return No value is returned, but a tex file is written to disk.  Currently this is 
#' output to the same location as the input file, and is given an identical name except 
#' for the file extension.
#' 
#' @importFrom rmarkdown pandoc_convert
#' @importFrom knitr knit render_latex
#' @importFrom tools file_path_sans_ext file_path_as_absolute
#' @export
markdownToLatex <- function(input) {
    
    if(!file.exists(input)) {
        stop("Cannot find file provided by the argument: 'input'")
    }
    input <- file_path_as_absolute(input)
    
    ## create file paths
    tmp_dir <- tempdir() 
    tmp_file <- tempfile(tmpdir = tmp_dir)
    tmp_file2 <- paste0(file_path_sans_ext(input), '.tmp.tex')
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
                   output = tmp_file2)
    ## pandoc uses a different table format to the F1000 template, so we'll try to modify it
    .processPandocTables(input = tmp_file2, output = output)
    ## if we created some plots in the temp dir, move them to the same directory as our output file
    if(file.exists(file.path(tmp_dir, 'figure'))) {
        file.copy(from = file.path(tmp_dir, 'figure'), 
                  to = dirname(input),
                  recursive = TRUE)
    }
    
    message("Output file has been written to:\n\t", output)
    
}

#' pandoc uses \longtable when converting markdown tables.  From what I understand this is hardcoded behaviour.
#' Since this isn't the table format specified by the f1000 template, we'll attempt to convert the pandoc
#' output into one that matches.  There are probably lots of caveats that aren't caught yet.
#' The general principle is to find each \longtable chunk and process it separately. The modified text
#' is then re inserted into the document, and the original lines removed.
#' 
#' @importFrom stringr str_detect
.processPandocTables <- function(input, output) {
    
    lines <- readLines(input, warn = FALSE)
    
    ## find lines marking start and end of \longtable chunks and create a list of duples
    longtableLines <- which(str_detect(string = lines, pattern= "(\\\\begin|\\\\end)\\{longtable\\}.*$"))
    longtableLines <- split(longtableLines, rep(1:(length(longtableLines)/2), each = 2))
    ## create a list where each entry is a table chunk
    tableList <- lapply(longtableLines, function(x, lines) { lines[(x[1]:x[2])] }, lines)

    ## fix each chunck and return a vector of lines, which we insert in the correct place
    fixed_table_lines <- sapply(tableList, .individualTable)
    lines[ do.call("rbind", longtableLines)[,1] ] <- fixed_table_lines
    
    ## flag all the original \longtable lines and remove
    rm_lines <- unlist(lapply(longtableLines, function(x) { (x[1]+1):(x[2]) }))
    lines <- lines[ -rm_lines ]
    
    writeLines(lines, con = output)
}

#' @importFrom stringr str_detect str_replace
.individualTable <- function(lines) {
    
    ## find the column justifications
    ## we use these later once the tabledata environment is defined
    justLines <- which(str_detect(pattern = "^.*\\\\begin\\{longtable\\}.*$", string = lines))
    if(length(justLines)) {
        justEntries <- gsub(pattern = "^.*\\](\\{.*\\})", replacement = "\\1", x = lines[justLines])
    }
    
    ## header line comes after the first \toprule
    first_toprule <- min(which(str_detect(pattern = "^.*\\\\toprule$", string = lines)))
    last_endhead <- max(which(str_detect(pattern = "^.*\\\\endhead$", string = lines)))
    
    ## insert header line with latex tag, and remove all other parts of table header
    lines[first_toprule] <- paste0("\\begin{tabledata}", justEntries)
    lines[first_toprule+1] <- paste("\\header", lines[first_toprule+1])
    lines <- lines[-((first_toprule+2):(last_endhead))]
    
    ## lines between our new header and \bottomrule are data rows
    last_header <- max(which(str_detect(pattern = "^\\\\header", string = lines)))
    last_bottomrule <- max(which(str_detect(pattern = "^\\\\bottomrule$", string = lines)))
    lines[(last_header+1):(last_bottomrule-1)] <- paste("\\row", lines[(last_header+1):(last_bottomrule-1)])
    
    ## mark the end of the table data
    lines[last_bottomrule] <- "\\end{tabledata}"
    
    ## substitute longtable for regular table
    lines <- gsub(pattern = "^.*(\\\\begin\\{|\\\\end\\{)long(table\\}).*$", replacement = "\\1\\2", x = lines)
    
    ## fix the new lines
    lines <- str_replace(string = lines, pattern = "(^\\\\row.*|^\\\\header.*)\\\\tabularnewline$", replacement = "\\1\\\\\\\\")
    lines <- str_replace(string = lines, pattern = "(^.*)\\\\tabularnewline$", replacement = "\\1")
    
    ## collapse into a single string seperated with new lines.
    paste(lines, collapse = "\n")
}