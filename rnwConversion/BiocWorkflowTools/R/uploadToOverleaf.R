
#' Upload a LaTeX project to Overleaf
#' 
#' @param files Character vector of file names to upload.  If the first entry is a zip file this is uploaded directly.
#'      Otherwise the files will be added to a zip archive and then uploaded.
#' @param openInBrowser Boolean determining whether to open a browser at the create Overleaf project or not.
#' 
#' @importFrom stringr str_replace_all
#' @importFrom utils browseURL zip
#' @importFrom tools file_ext
#' @importFrom httr POST
#' @export
uploadToOverleaf <- function(files = NULL, openInBrowser = FALSE) {
 
    if(is.null(files)) {
        stop("No file(s) specified")
    } else if(file_ext(files[1]) != "zip") {
        ## zip the files up, even if there's only one
        tf <- tempfile(fileext = "zip")
        zip(zipfile = tf, files = files)
        files[1] <- tf
    } 

    ## this is an irritating two step process. First we upload the zip file to a free file host.
    ## then we pass the URL for this to the overleaf API.  Maybe we can improve this in the future?
    uploaded <- POST(url = 'https://transfer.sh/',
                     body = list(zip_file = upload_file(files[1])))
    ## strip new line
    zip_url <- str_replace_all(content(uploaded, encoding = "UTF-8", as = "parsed"), 
                               "[\r\n]" , "")
    ## post to overleaf
    tmp <- POST(url = 'https://www.overleaf.com/docs',
                body = list(zip_uri = zip_url))
    overleaf_url <- head(tmp)$url
    
    message("Overleaf project created at:\n\t", overleaf_url)
    
    if(openInBrowser){
        browseURL(url = overleaf_url)
    }
}