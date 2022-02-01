



#' @title Get median rental prices for Victorian housing
#'
#' @return
#' @export
#' @source https://www.dffh.vic.gov.au/publications/rental-report
#' @examples
read_vic_median_rents_qrt <- function(){

  url <- 'https://www.dffh.vic.gov.au/publications/rental-report'

  links <- rvest::read_html(url) |>
    html_elements('article') |>
    html_elements('a') |>
    html_attr('href')

  vic_median_rents_qrt <- grep('quarterly-median-rents-local-government-area',
                               links,
                               ignore.case = TRUE,
                               value = TRUE)

  date_latest <- lubridate::parse_date_time(vic_median_rents_qrt, '%B%Y')

  # TODO: check here if data already downloaded?

  #filename <- tempfile(fileext = '.xlsx')
  filename <- 'data-raw/vic_median_rents_qrt.xlsx' # TODO: not sure why but could not read file from temp folder

  download.file(paste0('https://www.dffh.vic.gov.au/', vic_median_rents_qrt), method = 'curl',
                filename)

  ex <- httr::GET(paste0('https://www.dffh.vic.gov.au/', vic_median_rents_qrt))


  df <- readxl::read_excel(filename)

}
