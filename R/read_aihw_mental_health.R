

# 1: Number of episodes of care per 10000 population
read_aihw_care_episodes <- function(url = urls$read_aiwh_care_episodes) {

  download.file(url, "data-raw/aihw/mental-health-res.xlsx", mode = "wb")

  mh_res_orig <- readxl::read_excel("data-raw/aihw/mental-health-res.xlsx", sheet = "Table RMHC.18", skip = 3)

  mh_res_non_geo <- mh_res_orig %>%
    janitor::clean_names() %>%
    filter(counting_unit == "Episodes of\r\ncare" &
             str_detect(measure, "Rate") &
             state == "VIC") %>%
    select(-c(state, sa3_name, counting_unit, measure)) %>%
    pivot_longer(!sa3_code, names_to = "observation_date", values_to = "value") %>%
    # get rid of NAs
    filter(value != "n.p." & value != "0") %>%
    mutate(value = as.numeric(value),
           observation_date = as.character(glue::glue("{as.numeric(str_sub(observation_date, 2, 5)) + 1}-06-30")), # financial year
           indicator_id = 'Residential mental health care episodes (per 10k population)')

  return(mh_res_non_geo)

}

# 2: Number of presentations to emergency department per 10000 population
read_aihw_emergency_presentations <- function(url = urls$read_aihw_emergency_presentations) {

  download.file(url, "data-raw/aihw/mental-health-ed.xlsx", mode = "wb")

  mh_ed_orig <- readxl::read_excel("data-raw/aihw/mental-health-ed.xlsx", sheet = "Table ED.21", skip = 3)

  mh_ed_non_geo <- mh_ed_orig %>%
    janitor::clean_names() %>%
    filter(str_detect(type_of_presentation, "Mental") &
             str_detect(statistic, "Rate") &
             state_territory == "Victoria") %>%
    select(-c(type_of_presentation, state_territory, sa3_name, statistic)) %>%
    mutate(across(.cols = contains('x20'),.fns = as.numeric)) %>%
    pivot_longer(!sa3_code, names_to = "observation_date", values_to = "value") %>%
    mutate(value = as.numeric(value),
           observation_date = as.character(glue::glue("{as.numeric(str_sub(observation_date, 2, 5)) + 1}-06-30")), # financial year
           indicator_id = 'Presentations to emergency department (per 10k popultaion)') %>%
    filter(!is.na(value) & value != 0)

  return(mh_ed_non_geo)

}



# 3: Number of contacts per 10000 population

# Note that timeseries data exists for community MH care by SA3 for FY14-FY18 in Table CMHC.29 of
# https://www.aihw.gov.au/getmedia/4e93a556-b357-4741-8ab0-e31d93bed395/Community-mental-health-care-services-tables-2017-18.xlsx.aspx
# but data for 2019-20 is only at state/territory level (in Table CMHC.1) or remoteness area in Table CMHC.11 of
# https://www.aihw.gov.au/getmedia/5642baad-f0ee-4b9e-9dea-ac3a43069775/Community-mental-health-care-tables-2018-19.xlsx.aspx

read_aihw_community_care <- function(url = urls$read_aihw_community_care) {

  download.file(url, "data-raw/aihw/mental-health-community.xlsx", mode = "wb")

  mh_comm_orig <- readxl::read_excel("data-raw/aihw/mental-health-community.xlsx", sheet = "Table CMHC.29", skip = 3)

  mh_comm_non_geo <- mh_comm_orig %>%
    janitor::clean_names() %>%
    filter(str_detect(statistic, "Rate of contacts") & !is.na(sa3_code)) %>%
    select(-c(phn_code, phn_name, sa3_name, statistic)) %>%
    pivot_longer(!sa3_code, names_to = "observation_date", values_to = "value") %>%
    mutate(value = as.numeric(value),
           observation_date = as.character(glue::glue("{as.numeric(str_sub(observation_date, 2, 5)) + 1}-06-30")), # financial year
           indicator_id = 'Community Mental Health Care Cases (per 10k population') %>%
    filter(!is.na(value) & value != 0)

return(mh_comm_non_geo)

}


# 4: Number of prescriptions per 1000 population



read_aihw_prescriptions <- function(url = urls$read_aihw_prescriptions) {

  download.file(url, "data-raw/aihw/mental-health-prescriptions.xlsx", mode = "wb")

  mhp_orig <- readxl::read_excel("data-raw/aihw/mental-health-prescriptions.xlsx", sheet = "Table PBS.20", skip = 3)

  non_geo_data <- mhp_orig %>%
    filter(str_detect(Count, "Prescriptions")) %>%
    select(sa3_code = `SA3 code`, value = last_col(offset = 1)) %>%
    filter(value != 'n.p.') %>%
    mutate(observation_date = as.character("2019-20"),
           value = as.numeric(value) * 10,
           indicator_id = new_indicator_4$indicator_id,
           sa3_code = as.numeric(sa3_code)) %>%
    select(observationsa3_code)




}











download.file("https://www.aihw.gov.au/getmedia/4d45a801-25ad-483a-9578-f5226e8d40c6/Mental-health-related-prescriptions-tables-20118-19.xlsx.aspx",
                  "raw-data/mental-health-prescrip.xlsx", mode = "wb")

mhp_orig <- read_excel("data-raw/mental-health-prescrip.xlsx",
                       sheet = "Table PBS.10", skip = 3)

non_geo_data <- mhp_orig %>%
  select(sa3_code = `SA3 code`,
         value = `Rate of prescriptions (per 1000 of the specific population)`) %>%
  filter(value != "n.p.") %>%
  mutate(observation_date = as.character("2019-06-30"),
         # multiply by 10 so they become rates per 10,000 of the population:
         value = as.numeric(value) * 10,
         indicator_id = new_indicator_4$indicator_id,
         sa3_code = as.numeric(sa3_code)) %>%
  filter(!is.na(value) & value != 0)

