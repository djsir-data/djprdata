
#--------------Import data and get the classification read-----------

url <- 'https://labourmarketinsights.gov.au/our-research/internet-vacancy-index/'
search_term <- 'ivi_data_regional-may-2010-onwards.xlsx'

links <- djprdata:::get_latest_download_url(url, search_term)

djprdata:::download_excel(links$url, filepath = "data-raw/lim/internet_vacancies.xlsx")



v_internet_vacancies <- readxl::read_xlsx("data-raw/lim/internet_vacancies.xlsx", sheet = "Averaged") %>%
  filter(State == "VIC",
         !grepl("TOTAL", ANZSCO_TITLE)) %>%
  pivot_longer(-c(Level, State, region, ANZSCO_CODE, ANZSCO_TITLE), names_to = "date", values_to = "value") %>%
  mutate(observation_date = openxlsx::convertToDate(date)) %>%
  janitor::clean_names() %>%
  # take only 2-digit anzsco
  filter(nchar(anzsco_code) == 2) %>%
  filter(region == "Melbourne")


#-------------Prepare fact table and upload------------

# Append new rows in fact table
d_dim_classifications <- dbGetQuery(con, "SELECT * FROM d_dim_classifications")

d_regions_ivi <- dbGetQuery(con, "SELECT region_id, region_name
                                       FROM d_regions
                                       WHERE regional_short_classification = 'IVI'")

f_internet_vacancies <- v_internet_vacancies %>%
  left_join(select(filter(d_dim_classifications, dim_classification_shorthand == "ANZSCO2"), dim_id, dim_code), by = c("anzsco_code" = "dim_code")) %>%
  drop_na(dim_id, value) %>% # Drop those without a matching dim_id
  left_join(d_regions_ivi, by = c("region" = "region_name")) %>%
  drop_na(region_id, value) %>% # Drop those without a matching region in d_region or no values
  mutate(indicator_id = new_indicator$indicator_id,
         observation_date = as.character(observation_date)) %>%
  mutate(observation_date = as.character(ceiling_date(as.Date(observation_date), unit = "month") - 1)) %>%
  select(observation_date, region_id, indicator_id, dim_id, value)

dbWriteTable(con, "f_regional_data_one_dim", f_internet_vacancies, overwrite = FALSE, append = TRUE)
