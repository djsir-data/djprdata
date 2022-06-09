




#' @title Victorian Property prices, yearly and recent median prices.
#'
#' @description The Victorian Property Sales Report is released quarterly in
#' March, June, September and December. It covers median sale prices by suburb/locality
#' for houses, units and vacant land over a period of 15 months.
#'
#' @note Replaces the data import component of \url{https://github.com/djpr-data/osd-nous-dashboard/blob/main/processing/house-and-land-prices.R}
#'
#' @param include character the data set includes multiple temporal groupings
#' that can be automatically filtered. Options are:
#' \itemize{
#'   \item{all}
#'   \item{year}
#'   \item{quarterly}
#' }
#' @param test logical for testing to avoid repeatedly downloading data
#'
#' @import rvest
#' @import httr
#' @import readxl
#' @import dplyr
#' @importFrom purrr map_dfr
#' @importFrom tidyr pivot_longer
#' @importFrom lubridate ceiling_date
#' @importFrom stringr str_to_sentence
#' @importFrom lubridate my
#'
#' @return tibble
#'
#' @format A data frame with 11 variables. This is an active dataset rows increase weekly:
#' \describe{
#'   \item{locality \code{factor}}{locality or spatial grouping of data}
#'   \item{cdate \code{character}}{date as shown in spreadsheets}
#'   \item{date \code{date}}{last day in the period indicated by \code{time_scale}}. Therefore,
#'   last day of the year or last day of the quarter.
#'   \item{median \code{numeric}}{median dollar value of property sales within the \code{time_scale}}
#'   \item{time_scale \code{factor}}{indicating time period of the record:
#'     \itemize{
#'       \item{quarter}
#'       \item{year}
#'     }
#'   }
#'   \item{type \code{factor}}{properties are one of:
#'     \itemize{
#'       \item{house}
#'       \item{unit}
#'       \item{vacant}
#'     }}
#' }
#'
#' @export
#' @source \url{https://www.land.vic.gov.au/valuations/resources-and-reports/property-sales-statistics}
#' @examples
#' \dontrun{
#'   property_prices_all <- read_vic_property_prices()
#'
#'   rents_yearly <- read_vic_property_prices(include = "year")
#' }
read_vic_property_prices <- function(url = urls$read_vic_property_prices,
                                     include = c('all','year','quarterly'),
                                     test = FALSE){

  include <- match.arg(include, several.ok = FALSE)

  search_term <- 'house|unit|vacant'

  links <- get_latest_download_url(url, search_term)
  links$url <- links$url[endsWith(links$url, '.xls')]

  prop <- map_dfr(links$url, function(link){

    property_type <- dplyr::case_when(grepl('house', link, ignore.case = T) ~ 'house',
                                      grepl('unit', link, ignore.case = T) ~ 'unit',
                                      grepl('vacant', link, ignore.case = T) ~ 'vacant')

    filename <- download_excel(link, '.xls')

    suppressMessages({
      readxl::read_excel(filename, skip = 1, na = c('NA', '-')) |>
        dplyr::rename(locality = 1) |>
        tidyr::pivot_longer(-1, names_to = 'cdate', values_to = 'median') |>
        dplyr::mutate(time_scale = ifelse(nchar(.data$cdate) == 4, 'year', 'quarterly'),
                      type = .env$property_type)

    })

  })

  suppressWarnings({
    property <- prop |>
      dplyr::distinct() |>
      dplyr::filter(!is.na(.data$locality),
                    !(.data$locality %in% c('2021','locality')),
                    !is.na(.data$median),
                    !grepl('prelim|growth|sales|\\.\\.\\.', .data$cdate, ignore.case = TRUE)) |>
      tidyr::separate(col = .data$cdate, c('qrts','qrte'), sep = '-', remove = FALSE) |>
      dplyr::mutate(median = as.numeric(.data$median),
                    date = case_when(time_scale == 'year' ~ ceiling_date(as.Date(.data$cdate, format = '%Y'), 'year') - 1,
                                     time_scale == 'quarterly' ~ ceiling_date(lubridate::my(.data$qrte), 'month') - 1),
                    type = stringr::str_to_sentence(.data$type)) |>
      dplyr::mutate(across(c(.data$locality, .data$time_scale, .data$type), as.factor)) |>
      dplyr::select(!dplyr::starts_with('q'))
  })


  if (include == 'year') {
    property <- property |>
      dplyr::filter(time_scale == 'year')
  } else if (include == 'quarterly') {
    property <- property |>
      dplyr::filter(time_scale == 'quarterly')
  }

  return(property)

}
