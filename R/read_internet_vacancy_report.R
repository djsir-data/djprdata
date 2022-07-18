#' @title National Skills Commission's Internet Vacancy Report
#'
#' @description Extracting and cleaning monthly online job advertisements
#' for Melbourne from NSC's Internet Vacancy Report. Data is filtered by 2-digit ANZSCO
#'
#' @param url Base url which contains links to all data, default url is: \link[https://labourmarketinsights.gov.au/our-research/internet-vacancy-index/]
#' @param filename Default filename is tempfile()
#'
#' @importFrom janitor clean_names
#' @importFrom openxlsx convertToDate
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#'   df <- read_read_internet_vacancy()
#' }

read_internet_vacancy <- function(url = urls$read_internet_vacancy_report, filename = tempfile()) {

  search_term <- 'ivi_data_regional-may-2010-onwards.xlsx'

  links <- get_latest_download_url(url, search_term)

  download_excel(links$url, filepath = filename)

  v_internet_vacancies <- readxl::read_xlsx(filename, sheet = "Averaged") %>%
    filter(State == "VIC",
           !grepl("TOTAL", ANZSCO_TITLE)) %>%
    pivot_longer(-c(Level, State, region, ANZSCO_CODE, ANZSCO_TITLE), names_to = "date", values_to = "value") %>%
    mutate(observation_date = openxlsx::convertToDate(date)) %>%
    janitor::clean_names() %>%
    # take only 2-digit anzsco
    filter(nchar(anzsco_code) == 2) %>%
    filter(region == "Melbourne")

return(v_internet_vacancies)

}
