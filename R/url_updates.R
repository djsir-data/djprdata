



#' @title Get URL for latest Dataset
#'
#' @param url character URL or web link
#' @param search_term character to match new data set link
#' @import rvest
#' @import httr
#'
#' @return list
#'
#' @examples
#' \dontrun{
#'   dlpr::get_url_vic_median_rents_qrt()
#' }
#'
get_latest_download_url <- function(url, search_term){

  url_config <- httr::parse_url(url)

  links <- rvest::read_html(url) |>
    #html_elements('article') |>
    html_elements('a') |>
    html_attr('href')

  # update url with latest file location
  new_link <- grep(search_term,
                   links,
                   ignore.case = FALSE,
                   value = TRUE) |> unique()

  url_config$path <- new_link

  return(list(url = build_url(url_config),
              base_url = url,
              date = lubridate::parse_date_time(new_link, '%B%Y')))

}


