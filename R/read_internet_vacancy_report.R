#' @title National Skills Commission's Internet Vacancy Report
#'
#' @description Extracting and cleaning monthly online job advertisements
#'  from JSA's Internet Vacancy Index Report.
#'
#' @param url Base url which contains links to all data, default url is: \link[https://www.jobsandskills.gov.au/work/internet-vacancy-index]
#' @param dataset character id of dataset to be downloaded. Must be one of:
#' \itemize{
#'   \item{skills}{anzsco_skill_level_states_and_territories}
#'   \item{occupations}{anzsco4_occupations_states_and_territories}
#'   \item{occupations_skills}{anzsco2_occupations_skill_level_states_and_territories}
#'   \item{regions}{anzsco2_occupations_ivi_regions}
#' }
#' @param filename Default filename is tempfile()
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#'   df <- read_read_internet_vacancy(dataset = 'skills')
#' }
#'
read_internet_vacancy <- function(url = urls$read_internet_vacancy_report,
                                  dataset = c('skills', 'occupations', 'occupation_anzsco2', 'regions'),
                                  filename = tempfile()) {

  dataset <- match.arg(dataset)

  datasets <- list(
    skills = "skill",
    occupations = "anzsco4",
    occupation_anzsco2 = 'ANZSCO2%20Occupations%2C%20States',
    regions = "region"
  )

  dataset <- datasets[[dataset]]

  links <- djprdata:::get_latest_download_url(url, '\\.xlsx')

  link_n <- grep(dataset, basename(links$url), value = FALSE, ignore.case = TRUE)

  djprdata:::download_excel(links[link_n], filepath = filename)

  sheets <- readxl::excel_sheets(filename)
  sheets <- sheets[sheets != 'Notes']

  data <- sheets |>
    purrr::map_dfr(function(sht){
      readxl::read_excel(filename, sheet = sht) |>
        mutate(dataset = dataset,
               measure = sht) |>
        tidyr::pivot_longer(cols = matches('\\d{5}'),
                            names_to = "date",
                            values_to = "value") |>
        mutate(
          date = as.Date(as.numeric(date), origin = "1899-12-30"),
          value = as.numeric(value)
        )
    })

  return(data)

}
