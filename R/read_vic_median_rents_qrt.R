

#' @title Victorian Rental Properties, Count and Median prices.
#'
#' @description Each quarter the Victorian
#' \link[https://www.dffh.vic.gov.au/]{Department of Families Fairness and Housing (DFFH)}
#' publish data on the number of rental properties and the median price. Data is included for
#' each Local Government Area
#'
#' @note Replaces the data import component of \url{https://github.com/djpr-data/osd-nous-dashboard/blob/main/processing/rents-by-lga.R}
#'
#' @param include character the data set includes various spatial groupings
#' that can be automatically filtered. Options are:
#' \itemize{
#'   \item{all}
#'   \item{lga}
#'   \item{metro}
#' }
#' @param test logical for testing to avoid repeatedly downloading data
#'
#' @import rvest
#' @import httr
#' @import readxl
#' @import dplyr
#' @importFrom purrr map_dfr
#' @importFrom tidyr pivot_longer fill
#' @importFrom lubridate month year
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#'
#' @return tibble
#'
#' @format A data frame with 11 variables. This is an active dataset rows increase weekly:
#' \describe{
#'   \item{district \code{character}}{Broad area districts:
#'     \itemize{
#'           \item{Barwon South West}
#'           \item{Grampians}
#'           \item{Loddon Mallee}
#'           \item{Hume}
#'           \item{Gippsland}
#'           \item{North and West Metro}
#'           \item{Eastern Metro}
#'           \item{Southern Metro}
#'           \item{METRO NON-METRO}
#'     }
#'   }
#'   \item{area \code{character}}{Local Government Area (LGA)}
#'   \item{property \code{character}}{Property type based on bedroom number}
#'   \item{date \code{date}}{Record date fields converted from "Year Month" format}
#'   \item{variable \code{character}}{Type of data}
#'   \item{value \code{numeric}}{Value}
#'   \item{mon \code{character}}{Month}
#'   \item{mon_num \code{character}}{Month number}
#'   \item{yr \code{numeric}}{Year}
#' }
#'
#' @export
#' @source \url{https://www.dffh.vic.gov.au/publications/rental-report}
#' @examples
#' \dontrun{
#'   rents_all <- read_vic_median_rents_qrt()
#'
#'   rents_lga <- read_vic_median_rents_qrt(include = "lga")
#' }
read_vic_median_rents <- function(include = c('all','lga','metro'), test = FALSE){

  include <- match.arg(include, several.ok = FALSE)

  url <- 'https://www.dffh.vic.gov.au/publications/rental-report'
  search_term <- 'quarterly-median-rents-local-government-area'

  url <- get_latest_download_url(url, search_term)

  message(glue::glue('Latest Data From: {format(url$date, "%B %Y")}'))

  # TODO: check here if data already downloaded?

  if (test) {
    filename <- 'test_data/vic_median_rents_qrt_testing.xlsx'
  } else {

    filename <- download_excel(url)

  }

  sheets <- readxl::excel_sheets(filename)

  # process and row_bind each sheet
  vic_median_rents <- map_dfr(1:length(sheets), function(i){

    suppressMessages({
      sht <- readxl::read_excel(filename,
                                sheet = sheets[i],
                                skip = 1,
                                col_names = FALSE,
                                na = '-') |>
        tidyr::fill(1, .direction = 'down')
    })

    rows <- nrow(sht)

    # iterate across paired columns to reshape
    ## 3 methods were tested using microbenchmark
    ## the tidy options were 3-6 times slower than base R
    map_dfr(seq(3, ncol(sht), by = 2), function(n){

      tibble(district = sht[3:rows, 1][[1]],
             area = sht[3:rows, 2][[1]],
             property = sheets[i],
             date = lubridate::my(as.character(sht[1, n])),
             count = sht[3:rows, n][[1]],
             median = sht[3:rows, n + 1][[1]]
      )
    })
  })

  suppressWarnings({

  out <- vic_median_rents |>
      filter(across(c(district, area), ~ !grepl('Total', .x))) |>
      pivot_longer(c('count','median'), names_to = 'variable', values_to = 'value') |>
      mutate(value = as.numeric(value),
             mon = month(date, label = TRUE, abbr = TRUE),
             mon_num = stringr::str_pad(month(date), pad = '0', width = 2, side = 'left'),
             yr = year(date))

  })

  if (include == 'lga') {
    out <- out |>
      filter(district != 'METRO NON-METRO')
  } else if (include == 'metro') {
    out <- out |>
      filter(district == 'METRO NON-METRO')
  }

  return(out)

}




