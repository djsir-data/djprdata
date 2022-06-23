

#' @title School Enrollments for Victoria
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'   read_vic_schools_fte()
#' }
read_vic_schools_fte <- function(){

  ckanr::ckanr_setup(url = "https://discover.data.vic.gov.au/")
  school_data = ckanr::package_search('schools fte', as = 'table')
  all_resources <- school_data$results |>
    dplyr::mutate(url = purrr::map_chr(resources, ~.x$url),
           year = stringr::str_extract(title, '\\d{4}')) |>
    dplyr::select(title, year, url)

  split(all_resources, all_resources$year) |>
    purrr::map_dfr(function(x){
      filename <- tempfile(fileext = '.csv')
      download.file(x$url, filename, mode = 'wb')
      df <- readr::read_csv(filename) |>
        dplyr::rename_all(.funs = ~tolower(gsub('\\"', '', .x))) |>
        dplyr::mutate(year = x$year)
      print(colnames(df))
      df
    })

}
