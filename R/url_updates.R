



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
#'   dlpr::get_url_vic_median_rents_qrt(url, search_term)
#' }
#'
get_latest_download_url <- function(url, search_term){

  links <- rvest::read_html(url) |>
    #html_elements('article') |>
    html_elements('a') |>
    html_attr('href')

  # update url with latest file location
  new_link <- grep(search_term,
                   links,
                   ignore.case = TRUE,
                   value = TRUE) |> unique()

  out_links <- sapply(new_link, USE.NAMES = FALSE, function(x){
    if (is.null(httr::parse_url(x)$scheme)) {
      url_config <- httr::parse_url(url)
      url_config$path <- x
      build_url(url_config)
    } else {x}
  })


  return(list(url = out_links,
              base_url = url))

}


