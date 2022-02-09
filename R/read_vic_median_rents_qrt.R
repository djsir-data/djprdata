

library(httr)
library(rvest)
library(lubridate)
library(readxl)

#' @title Get median rental prices for Victorian housing
#'
#' @importFrom httr GET
#' @importFrom glue glue
#'
#' @return
#' @export
#' @source https://www.dffh.vic.gov.au/publications/rental-report
#' @examples
read_vic_median_rents_qrt <- function(){

  url <- 'https://www.dffh.vic.gov.au/publications/rental-report'
  url_config <- httr::parse_url(url)

  links <- rvest::read_html(url) |>
    html_elements('article') |>
    html_elements('a') |>
    html_attr('href')

  # update url with latest file location
  vic_median_rents_qrt <- grep('quarterly-median-rents-local-government-area',
                               links,
                               ignore.case = TRUE,
                               value = TRUE)

  url_config$path <- vic_median_rents_qrt
  latest_url <- build_url(url_config)


  date_latest <- lubridate::parse_date_time(vic_median_rents_qrt, '%B%Y')

  # TODO: check here if data already downloaded?

  filename <- tempfile(fileext = '.xlsx')

  resp <- GET(test_url, write_disk(filename, overwrite=TRUE))
  status <- http_status(resp)

  assertthat::assert_that(status$category == "Success",
                          msg = glue('Download Failed with message: {status$message}'))
  assertthat::assert_that(http_type(resp) == "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                          msg = 'Failed to retrieve a Spreadsheet as Expected')

  sheets <- readxl::excel_sheets(filename)


  # process and row_bind each sheet
  vic_median_rents <- map_dfr(1:length(sheets), function(i){

    sht <- readxl::read_excel(filename, sheet = sheets[i], skip = 1, col_names = F) |>
      tidyr::fill(1, .direction = 'down')

    rows <- nrow(sht)

    # iterate across paired columns to reshape
    ## 3 methods were tested using microbenchmark
    ## the tidy options were 3-6 times slower than base R
    map_dfr(seq(3, ncol(sht), by = 2), function(n){

      tibble(region = sht[3:rows, 1][[1]],
             lga = sht[3:rows, 2][[1]],
             property = sheets[i],
             date = lubridate::my(as.character(sht[1, n])),
             count = sht[3:rows, n][[1]],
             median = sht[3:rows, n + 1][[1]]
      )

    })

  })




}
