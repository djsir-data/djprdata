

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

  ckanr_setup(url = "https://discover.data.vic.gov.au/")
  school_data = package_search('schools fte', as = 'table')
  all_resources <- school_data$results |>
    mutate(url = map_chr(resources, ~.x$url),
           year = stringr::str_extract(title, '\\d{4}')) |>
    select(title, year, url)

  split(all_resources, all_resources$year) |>
    map_dfr(function(x){
      filename <- tempfile(fileext = '.csv')
      download.file(x$url, filename)
      df <- read_csv(filename) |>
        rename_all(.funs = ~tolower(gsub('\\"', '', .x))) |>
        mutate(year = x$year)
      print(colnames(df))
      df
    })

}
