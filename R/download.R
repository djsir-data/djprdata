

#' @title Download Excel File
#'
#' @description Given a URL download an excel spreadsheet to a temporary file
#' and test that the response is free of errors and that the correct file
#' type was found
#'
#' @param url character URL
#' @param filepath character path file and extension for the downloaded file.
#' Leave as \code{NULL} to save as temporary file
#'
#' @param ext character extension (ignored if \code{filepath} defined), must
#' be one of:
#'   \itemize{
#'     \item{.xlsx}
#'     \item{.xls}
#'   }
#' @param verbose logical print messages to console
#'
#' @importFrom crayon blue green
#' @importFrom assertthat assert_that
#' @import httr
#'
#' @return character name of temporary file
#' @export
#'
#' @examples
#' \dontrun{
#'   download_excel(url)
#' }
download_excel <- function(url, filepath = NULL, ext = '.xlsx', verbose = FALSE){

  assertthat::assert_that(ext %in% c('.xlsx', '.xls'),
                          msg = 'the extension provided is not recognised')
  assertthat::assert_that(startsWith(ext, '.'),
                          msg = 'the extension variable is passed to tempfile and needs a preceeding .')

  if (is.null(filepath)) {
    filepath <- tempfile(fileext = ext)
  }
  resp <- GET(url, write_disk(filepath, overwrite=TRUE))
  status <- http_status(resp)

  assertthat::assert_that(status$category == "Success",
                          msg = glue('Download Failed with message: {status$message}'))
  assertthat::assert_that(http_type(resp) %in% c("application/vnd.ms-excel",
                                                 "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
                          msg = 'Failed to retrieve a Spreadsheet as Expected')

  if (verbose){
    message(crayon::blue('File available at: \n'),
            crayon::green(glue::glue('   {filepath}')))
  }

  return(filepath)

}
