# Add mental health-related episodes of care, presentations, contacts and prescriptions per 10,000 population
#
# FY2019 data for community contacts are missing (other published data files checked)
#

#-------------Set up indicator and clear old versions------------

# -- First indicator - No. of episodes of RESIDENTIAL mental health-related care per 10000 pop
new_indicator_1 <- tibble(
  indicator_id = get_next_indicator_id(),
  indicator_code = 'residential_mental_health_care_rate',
  indicator_category = 'Community, liveability and wellbeing',
  indicator_description = "Residential mental health-related care per 10,000 population by SA3",
  indicator_short_name = "Residential mental health-related care rate",
  indicator_source = "Australian Institute of Health and Welfare",
  indicator_url = "https://www.aihw.gov.au/reports-data/health-welfare-services/mental-health-services/data",
  indicator_units = "Rate",
  aggregation_method = "average",
  # these next we will populate all at once in the database at the end of the build:
  max_value = NA,
  min_value = NA,
  avg_value = NA,
  standard_deviation = NA,
  number_data_points = NA,
  number_observation_dates = NA,
  earliest_observation = NA,
  latest_observation = NA,
  number_dimensions = 1,
  dim_var_1_classification_shorthand = "Residential mental health-related care"
)

# Remove any prior versions of this data:
dbExecute(con, glue("DELETE FROM d_indicators WHERE indicator_short_name = '{new_indicator_1$indicator_short_name}'"))

# Write this new indicator to the indicators dimension table:
dbWriteTable(con, "d_indicators", new_indicator_1, append = TRUE)

# -- Second indicator - No. of mental health-related presentations to EMERGENCY DEPTS per 10000 pop
new_indicator_2 <- tibble(
  indicator_id = get_next_indicator_id(),
  indicator_code = 'mental_health_presentations_to_emergency_department_rate',
  indicator_category = 'Community, liveability and wellbeing',
  indicator_description = "Mental health-related presentations to emergency department per 10,000 population by SA3",
  indicator_short_name = "Mental health-related presentations to emergency department rate",
  indicator_source = "Australian Institute of Health and Welfare",
  indicator_url = "https://www.aihw.gov.au/reports-data/health-welfare-services/mental-health-services/data",
  indicator_units = "Rate",
  aggregation_method = "average",
  # these next we will populate all at once in the database at the end of the build:
  max_value = NA,
  min_value = NA,
  avg_value = NA,
  standard_deviation = NA,
  number_data_points = NA,
  number_observation_dates = NA,
  earliest_observation = NA,
  latest_observation = NA,
  number_dimensions = 1,
  dim_var_1_classification_shorthand = "Mental health-related presentations to emergency department"
)

# Remove any prior versions of this data:
dbExecute(con, glue("DELETE FROM d_indicators WHERE indicator_short_name = '{new_indicator_2$indicator_short_name}'"))

# Write this new indicator to the indicators dimension table:
dbWriteTable(con, "d_indicators", new_indicator_2, append = TRUE)

# -- Third indicator - No. of mental health-related COMMUNITY contacts per 10000 pop
new_indicator_3 <- tibble(
  indicator_id = get_next_indicator_id(),
  indicator_code = 'mental_health_community_contacts_rate',
  indicator_category = 'Community, liveability and wellbeing',
  indicator_description = "Mental health-related community contacts per 10,000 population by SA3",
  indicator_short_name = "Mental health-related community contacts rate",
  indicator_source = "Australian Institute of Health and Welfare",
  indicator_url = "https://www.aihw.gov.au/reports-data/health-welfare-services/mental-health-services/data",
  indicator_units = "Rate",
  aggregation_method = "average",
  # these next we will populate all at once in the database at the end of the build:
  max_value = NA,
  min_value = NA,
  avg_value = NA,
  standard_deviation = NA,
  number_data_points = NA,
  number_observation_dates = NA,
  earliest_observation = NA,
  latest_observation = NA,
  number_dimensions = 1,
  dim_var_1_classification_shorthand = "Mental health-related community contacts"
)

# Remove any prior versions of this data:
dbExecute(con, glue("DELETE FROM d_indicators WHERE indicator_short_name = '{new_indicator_3$indicator_short_name}'"))

# Write this new indicator to the indicators dimension table:
dbWriteTable(con, "d_indicators", new_indicator_3, append = TRUE)

# -- Fourth indicator - No. of mental health-related prescriptions per thousand population
new_indicator_4 <- tibble(
  indicator_id = get_next_indicator_id(),
  indicator_code = 'mental_health_prescriptions_rate',
  indicator_category = 'Community, liveability and wellbeing',
  indicator_description = "Mental health-related prescriptions per 10,000 population by SA3",
  indicator_short_name = "Mental health-related prescriptions rate",
  indicator_source = "Australian Institute of Health and Welfare",
  indicator_url = "https://www.aihw.gov.au/reports-data/health-welfare-services/mental-health-services/data",
  indicator_units = "Rate",
  aggregation_method = "average",
  # these next we will populate all at once in the database at the end of the build:
  max_value = NA,
  min_value = NA,
  avg_value = NA,
  standard_deviation = NA,
  number_data_points = NA,
  number_observation_dates = NA,
  earliest_observation = NA,
  latest_observation = NA,
  number_dimensions = 1,
  dim_var_1_classification_shorthand = "Mental health-related prescriptions"
)

# Remove any prior versions of this data:
dbExecute(con, glue("DELETE FROM d_indicators WHERE indicator_short_name = '{new_indicator_4$indicator_short_name}'"))

# Write this new indicator to the indicators dimension table:
dbWriteTable(con, "d_indicators", new_indicator_4, append = TRUE)

#--------------Import data and get the classification read-----------

# 1: Number of episodes of care per 10000 population


url <- 'https://www.aihw.gov.au/reports-data/health-welfare-services/mental-health-services/data'
search_term <- 'Residential-mental-health-care'

links <- djprdata:::get_latest_download_url(url, search_term)

djprdata:::download_excel(links$url, filepath = "data-raw/lim/internet_vacancies.xlsx")


https://www.aihw.gov.au/getmedia/a456331c-d343-409b-b967-b3a457557090/Residential-mental-health-care-tables-1920.xlsx.aspx





download_if_fresh("https://www.aihw.gov.au/getmedia/89187dc3-0949-45d1-a992-80ea28d9e112/Residential-mental-health-care-tables-2018-19.xlsx.aspx",
                  "raw-data/mental-health-res.xlsx")
mh_res_orig <- read_excel("raw-data/mental-health-res.xlsx", sheet = "Table RMHC.18", skip = 3)

mh_res_non_geo <- mh_res_orig %>%
  clean_names() %>%
  filter(counting_unit == "Episodes of\r\ncare" &
           str_detect(measure, "Rate") &
           state == "VIC") %>%
  select(-c(state, sa3_name, counting_unit, measure)) %>%
  pivot_longer(!sa3_code, names_to = "observation_date", values_to = "value") %>%
  # get rid of NAs
  filter(value != "n.p." & value != "0") %>%
  mutate(value = as.numeric(value),
         observation_date = as.character(glue("{as.numeric(str_sub(observation_date, 2, 5)) + 1}-06-30")), # financial year
         indicator_id = new_indicator_1$indicator_id)

# 2: Number of presentations to emergency department per 10000 population
download_if_fresh("https://www.aihw.gov.au/getmedia/34def0aa-b250-404c-9a90-512d6c9655f5/Mental-health-services-provided-in-emergency-departments-tables_1819.xlsx.aspx",
                  "raw-data/mental-health-ed.xlsx")
mh_ed_orig <- read_excel("raw-data/mental-health-ed.xlsx", sheet = "Table ED.17", skip = 3)

mh_ed_non_geo <- mh_ed_orig %>%
  clean_names() %>%
  filter(str_detect(type_of_presentation, "Mental") &
           str_detect(statistic, "Rate") &
           state_territory == "Victoria") %>%
  select(-c(type_of_presentation, state_territory, sa3_name, statistic)) %>%
  pivot_longer(!sa3_code, names_to = "observation_date", values_to = "value") %>%
  mutate(value = as.numeric(value),
         observation_date = as.character(glue("{as.numeric(str_sub(observation_date, 2, 5)) + 1}-06-30")), # financial year
         indicator_id = new_indicator_2$indicator_id) %>%
  filter(!is.na(value) & value != 0)

# 3: Number of contacts per 10000 population
download_if_fresh("https://www.aihw.gov.au/getmedia/4e93a556-b357-4741-8ab0-e31d93bed395/Community-mental-health-care-services-tables-2017-18.xlsx.aspx",
                  "raw-data/mental-health-comm.xlsx")
mh_comm_orig <- read_excel("raw-data/mental-health-comm.xlsx", sheet = "Table CMHC.29", skip = 3)

# Note that timeseries data exists for community MH care by SA3 for FY14-FY18 in Table CMHC.29 of
# https://www.aihw.gov.au/getmedia/4e93a556-b357-4741-8ab0-e31d93bed395/Community-mental-health-care-services-tables-2017-18.xlsx.aspx
# but data for FY19 is only at state/territory level (in Table CMHC.1) or remoteness area in Table CMHC.11 of
# https://www.aihw.gov.au/getmedia/5642baad-f0ee-4b9e-9dea-ac3a43069775/Community-mental-health-care-tables-2018-19.xlsx.aspx

mh_comm_non_geo <- mh_comm_orig %>%
  clean_names() %>%
  filter(str_detect(statistic, "Rate of contacts") & !is.na(sa3_code)) %>%
  select(-c(phn_code, phn_name, sa3_name, statistic)) %>%
  pivot_longer(!sa3_code, names_to = "observation_date", values_to = "value") %>%
  mutate(value = as.numeric(value),
         observation_date = as.character(glue("{as.numeric(str_sub(observation_date, 2, 5)) + 1}-06-30")), # financial year
         indicator_id = new_indicator_3$indicator_id) %>%
  filter(!is.na(value) & value != 0)

# 4: Number of prescriptions per 1000 population
download_if_fresh("https://www.aihw.gov.au/getmedia/4d45a801-25ad-483a-9578-f5226e8d40c6/Mental-health-related-prescriptions-tables-20118-19.xlsx.aspx",
                  "raw-data/mental-health-prescrip.xlsx")

mhp_orig <- read_excel("raw-data/mental-health-prescrip.xlsx",
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


#--------------Update dimension table--------------------------------

# Append new rows in dim_classifications
dim_classification <- data.frame(dim_description = c('Residential mental health-related care per 10,000 population',
                                                     'Mental health-related presentations to emergency department per 10,000 population',
                                                     'Mental health-related community contacts per 10,000 population',
                                                     'Mental health-related prescriptions per 10,000 population'),
                                 dim_id= seq(get_next_dim_id(), length.out = 4),
                                 dim_code = 1,
                                 dim_source = "AIHW",
                                 dim_classification_name = c("Residential mental health-related care by population",
                                                             "Mental health-related presentations to emergency department by population",
                                                             "Mental health-related community contacts",
                                                             "Mental health-related prescriptions"),
                                 dim_classification_shorthand = c(new_indicator_1$dim_var_1_classification_shorthand,
                                                                  new_indicator_2$dim_var_1_classification_shorthand,
                                                                  new_indicator_3$dim_var_1_classification_shorthand,
                                                                  new_indicator_4$dim_var_1_classification_shorthand)
)

# Remove any prior versions of this data:
dbExecute(con, glue("DELETE FROM d_dim_classifications WHERE dim_classification_shorthand = '{new_indicator_1$dim_var_1_classification_shorthand}'"))
dbExecute(con, glue("DELETE FROM d_dim_classifications WHERE dim_classification_shorthand = '{new_indicator_2$dim_var_1_classification_shorthand}'"))
dbExecute(con, glue("DELETE FROM d_dim_classifications WHERE dim_classification_shorthand = '{new_indicator_3$dim_var_1_classification_shorthand}'"))
dbExecute(con, glue("DELETE FROM d_dim_classifications WHERE dim_classification_shorthand = '{new_indicator_4$dim_var_1_classification_shorthand}'"))

# Write this new classification to the classification dimension table:
dbWriteTable(con, "d_dim_classifications", dim_classification, overwrite = FALSE, append = TRUE)

#-------------Prepare fact table and upload------------

d_regions_sa3 <- dbGetQuery(con, "SELECT region_id, region_code
                                  FROM d_regions
                                  WHERE regional_short_classification = 'SA3'")

f_mh <- bind_rows(mh_res_non_geo, mh_ed_non_geo, mh_comm_non_geo, non_geo_data) %>%
  mutate(sa3_code = as.character(sa3_code)) %>%
  left_join(d_regions_sa3, by = c("sa3_code" = "region_code")) %>%
  filter(!is.na(region_id)) %>%
  # get dim ID
  mutate(dim_classification_shorthand = case_when(
    indicator_id == new_indicator_1$indicator_id ~ new_indicator_1$dim_var_1_classification_shorthand,
    indicator_id == new_indicator_2$indicator_id ~ new_indicator_2$dim_var_1_classification_shorthand,
    indicator_id == new_indicator_3$indicator_id ~ new_indicator_3$dim_var_1_classification_shorthand,
    indicator_id == new_indicator_4$indicator_id ~ new_indicator_4$dim_var_1_classification_shorthand)) %>%
  left_join(select(dim_classification, dim_classification_shorthand, dim_id), by = "dim_classification_shorthand") %>%
  select(region_id, observation_date, value, indicator_id, dim_id)

# ---------- Upload to database ----------
dbWriteTable(con, "f_regional_data_one_dim", f_mh, overwrite = FALSE, append = TRUE)
