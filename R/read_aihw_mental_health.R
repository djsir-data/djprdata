#' @title Australian Institute of Health and Welfare - Mental Health Data
#'
#' @description Extracting and cleaning yearly mental health data metrics
#' published by AIHW. The functions collect data for public healthcare episodes,
#' presentations and emergency departments, community care contacts and
#' prescriptions.The data is transformed into 'per 10k population' measures.
#'
#' @param url Base url which contains links to all data, default url is: \link[https://labourmarketinsights.gov.au/our-research/internet-vacancy-index/]
#' @param filename Default filename is tempfile()
#'
#' @importFrom janitor clean_names
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#'   df <- read_aihw_care_episodes()
#'   df <- read_aihw_emergency_presentations()
#'   df <- read_aihw_community_care()
#'   df <- read_aihw_prescriptions()
#'   }

#' @note 1: Number of episodes of care per 10000 population
read_aihw_care_episodes <- function(url = urls$read_aihw_care_episodes, filename = tempfile()) {

  download.file(url, filename, mode = "wb")

  mh_res_orig <- readxl::read_excel(filename, sheet = "Table RMHC.18", skip = 3)

  mh_res_non_geo <- mh_res_orig %>%
    janitor::clean_names() %>%
    filter(counting_unit == "Episodes of\r\ncare" &
             stringr::str_detect(measure, "Rate") &
             state == "VIC") %>%
    select(-c(state, sa3_name, counting_unit, measure)) %>%
    pivot_longer(!sa3_code, names_to = "observation_date", values_to = "value") %>%
    # get rid of NAs
    filter(value != "n.p." & value != "0") %>%
    mutate(value = as.numeric(value),
           observation_date = as.character(glue::glue("{as.numeric(stringr::str_sub(observation_date, 2, 5)) + 1}-06-30"))) %>%
    mutate(observation_date = as.Date(observation_date))

  return(mh_res_non_geo)

}

#' @note  2: Number of presentations to emergency department per 10000 population
read_aihw_emergency_presentations <- function(url = urls$read_aihw_emergency_presentations, filename = tempfile()) {

  download.file(url, filename, mode = "wb")

  mh_ed_orig <- readxl::read_excel(filename, sheet = "Table ED.21", skip = 3)

  mh_ed_non_geo <- mh_ed_orig %>%
    janitor::clean_names() %>%
    filter(stringr::str_detect(type_of_presentation, "Mental") &
             stringr::str_detect(statistic, "Rate") &
             state_territory == "Victoria") %>%
    select(-c(type_of_presentation, state_territory, sa3_name, statistic)) %>%
    mutate(across(.cols = contains('x20'),.fns = as.numeric)) %>%
    pivot_longer(!sa3_code, names_to = "observation_date", values_to = "value") %>%
    mutate(value = as.numeric(value),
           observation_date = as.character(glue::glue("{as.numeric(stringr::str_sub(observation_date, 2, 5)) + 1}-06-30"))) %>%
    filter(!is.na(value) & value != 0) %>%
    mutate(observation_date = as.Date(observation_date))

  return(mh_ed_non_geo)

}



#' @note  3: Number of contacts per 10000 population

# Note that timeseries data exists for community MH care by SA3 for FY14-FY18 in Table CMHC.29 of
# https://www.aihw.gov.au/getmedia/4e93a556-b357-4741-8ab0-e31d93bed395/Community-mental-health-care-services-tables-2017-18.xlsx.aspx
# but data for 2019-20 is only at state/territory level (in Table CMHC.1) or remoteness area in Table CMHC.11 of
# https://www.aihw.gov.au/getmedia/5642baad-f0ee-4b9e-9dea-ac3a43069775/Community-mental-health-care-tables-2018-19.xlsx.aspx

read_aihw_community_care <- function(url = urls$read_aihw_community_care, filename = tempfile()) {

  download.file(url, filename, mode = "wb")

  mh_comm_orig <- readxl::read_excel(filename, sheet = "Table CMHC.29", skip = 3)

  mh_comm_non_geo <- mh_comm_orig %>%
    janitor::clean_names() %>%
    filter(stringr::str_detect(statistic, "Rate of contacts") & !is.na(sa3_code)) %>%
    select(-c(phn_code, phn_name, sa3_name, statistic)) %>%
    pivot_longer(!sa3_code, names_to = "observation_date", values_to = "value") %>%
    mutate(value = as.numeric(value),
           observation_date = as.character(glue::glue("{as.numeric(stringr::str_sub(observation_date, 2, 5)) + 1}-06-30"))) %>%
    filter(!is.na(value) & value != 0) %>%
    mutate(observation_date = as.Date(observation_date))

return(mh_comm_non_geo)

}


#' @note  4: Number of prescriptions per 10000 population

read_aihw_prescriptions <- function(url = urls$read_aihw_prescriptions, filename = tempfile()) {

  download.file(url, filename, mode = "wb")

  mhp_orig <- readxl::read_excel(filename, sheet = "Table PBS.20", skip = 3)

  non_geo_data <- mhp_orig %>%
    filter(stringr::str_detect(Count, "Prescriptions")) %>%
    select(sa3_code = `SA3 code`, value = last_col(offset = 1)) %>%
    filter(value != 'n.p.') %>%
    mutate(observation_date = as.Date("2019-20"),   # this is buggy
           value = as.numeric(value) * 10,
           sa3_code = as.numeric(sa3_code))

  return(non_geo_data)

}




