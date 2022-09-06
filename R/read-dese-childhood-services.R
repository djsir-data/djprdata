#' @title DESE Childhood Services data
#'
#' @description Reading childcare utilisation data from the DESE child care report/
#'
#' @param url Base url which contains links to all data, default value is: \link[https://www.dese.gov.au/child-care-package/early-childhood-data-and-reports/quarterly-reports/]
#' @param filename Default file name is tempfile()
#'
#' @importFrom janitor clean_names
#' @importFrom readr parse_number
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#'   df <- read_dese_childhood_services()
#' }
#'

read_dese_childhood_services <- function(url = urls$read_dese_childhood_services, filename = tempfile()){

  search_term <- 'child-care-australia-report'

  links <- get_latest_download_url(url, search_term) %>%
    as_tibble() %>%
    dplyr::mutate(date_ref = stringr::str_sub(url, start = stringr::str_locate(url, 'quarterly-reports/child-care-australia-report-')[,2] + 1),
                  download_link = glue::glue('https://www.dese.gov.au/early-childhood/resources/{date_ref}'))

  current_download_url <- get_latest_download_url(links$download_link[1], 'xlsx')

  download_excel(current_download_url$url, filepath = filename)


  v_childhood_services <- readxl::read_xlsx(filename,
                                    sheet = "Statistical Area",
                                    skip = 2) %>%
    janitor::clean_names() %>%
    select(sa3_code, number_of_services) %>%
    filter(!is.na(sa3_code)) %>% # this is the total row
    slice(1:(n()-3)) %>% # last three rows are footnotes
    # Some numbers are presented like "<5" replace these with a value in the middle and round to integer (since it's count)
    mutate(number_of_services = ifelse(grepl("<", number_of_services), readr::parse_number(number_of_services)/2, number_of_services),
           number_of_services = as.numeric(number_of_services) %>% round())

  return(v_childhood_services)

}
