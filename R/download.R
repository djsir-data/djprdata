



#' @title Download Files
#'
#' @description Given a URL download to a temporary file
#' and test that the response is free of errors and that the correct file
#' type was found. A .zip file will be automatically extracted after download
#'
#' @param url character URL
#' @param filepath character path file and extension for the downloaded file.
#' Leave as \code{NULL} to save as temporary file
#' @param ext character extension, must be one of:
#'   \itemize{
#'     \item{.xlsx}
#'     \item{.xls}
#'     \item{.zip}
#'   }
#' @param verbose logical print messages to console
#' @param files character vector of specific files to extract from a .zip
#'
#' @importFrom crayon blue green
#' @importFrom assertthat assert_that
#' @importFrom utils unzip
#' @import httr
#'
#' @return character name of temporary file/s
#' @export
#'
#' @examples
#' \dontrun{
#'   download_file(url)
#' }
download_file <- function(url, filepath = NULL, ext = c('.xlsx','.xls','.zip'), files = NULL, verbose = FALSE){

  ext <- match.arg(ext, several.ok = FALSE)

  assertthat::assert_that(startsWith(ext, '.'),
                          msg = 'the extension variable is passed to tempfile and needs a preceeding .')

  if (ext == '.zip') {
    expected_type <- "application/zip"
  } else {
    expected_type <- c("application/vnd.ms-excel",
                       "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
  }

  if (is.null(filepath)) {
    filepath <- tempfile(fileext = ext)
  }

  resp <- httr::GET(url, httr::write_disk(filepath, overwrite=TRUE))
  status <- httr::http_status(resp)

  assertthat::assert_that(status$category == "Success",
                          msg = glue('Download Failed with message: {status$message}'))



  assertthat::assert_that(httr::http_type(resp) %in% expected_type,
                          msg = glue::glue('Failed to retrieve a {ext} file as Expected'))

  if (ext == '.zip') {

    filepath <- unzip(filepath,
                      files = files,
                      exdir = dirname(filepath))

  }

  if (verbose){
    message(crayon::blue('File available at: \n'),
            crayon::green(glue::glue('   {paste(filepath, collapse = "\n   ")}')))
  }

  return(filepath)


}



#' @rdname download_file
#' @export
#'
#' @examples
#' \dontrun{
#'   download_excel(url)
#' }
download_excel <- download_file

