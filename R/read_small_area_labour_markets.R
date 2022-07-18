#' @title National Skills Commission's Small Area Labour Markets Estimates
#'
#' @description Extracting and cleaning quarterly unemployment rates
#' by SA2 from the NSC's SALM Dataset.
#'
#' @param url Base url which contains links to all data, default url is: \link[https://labourmarketinsights.gov.au/regions/small-area-labour-markets/]
#' @param filename Default filename is tempfile()
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#'   df <- read_salm()
#' }

read_salm <- function(url = urls$read_salm, filename = tempfile()) {

  search_term <- 'salm-smoothed-sa2'

  download_link <- get_latest_download_url(url, search_term) |>
    as_tibble() |>
    dplyr::select(url) |>
    dplyr::filter(stringr::str_detect(url, "\\.xlsx$")) |>
    mutate(filename = tolower(gsub(".*salm-smoothed-sa2-datafiles-asgs-2016-(.+).*", "\\1", url)))

  download_excel(download_link$url, filepath = filename)

  unemprate_orig <- readxl::read_excel(filename,
                                   sheet = 'Smoothed SA2 unemployment rate',
                                   skip = 3, na = "-") |>
    rename_with(.cols = dplyr::starts_with('4'), ~ as.character(as.Date(as.integer(.x), origin = '1899-12-30'))) |>
    rename(region = `SA2 Code (2016 ASGS)`, sa2_name = `Statistical Area Level 2 (SA2) (2016 ASGS)`) |>
    pivot_longer(cols = contains('-01'), names_to = 'date', values_to = 'value') |>
    mutate(value = as.numeric(value) / 100,
           date = as.Date(date))

return(unemprate_orig)

}
