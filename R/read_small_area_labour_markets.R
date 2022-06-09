
#--------------Import data and get the classification read-----------

# Get the latest data. Warning: this location might vary over time.


url <- 'https://labourmarketinsights.gov.au/regions/small-area-labour-markets/'
search_term <- 'salm-smoothed-sa2'

download_link <- djprdata:::get_latest_download_url(url, search_term) |>
  as.tibble() |>
  dplyr::select(url) |>
  dplyr::filter(str_detect(url, "\\.xlsx$")) |>
  mutate(filename = tolower(gsub(".*salm-smoothed-sa2-datafiles-asgs-2016-(.+).*", "\\1", url)))

djprdata:::download_excel(download_link$url, filepath = file.path('data-raw\\salm',download_link$filename))

unemprate_orig <- readxl::read_excel(file.path('data-raw\\salm',download_link$filename),
                                 sheet = 'Smoothed SA2 unemployment rate',
                                 skip = 3, na = "-") |>
  rename_with(.cols = dplyr::starts_with('4'), ~ as.character(as.Date(as.integer(.x), origin = '1899-12-30'))) |>
  rename(region = `SA2 Code (2016 ASGS)`, sa2_name = `Statistical Area Level 2 (SA2) (2016 ASGS)`) |>
  pivot_longer(cols = contains('-01'), names_to = 'date', values_to = 'value') |>
  mutate(value = as.numeric(value) / 100)

#--------------Update dimension table--------------------------------

# Remove any prior versions of this data:
dbExecute(con, glue("DELETE FROM d_dim_classifications
                     WHERE dim_classification_shorthand = '{new_indicator$dim_var_1_classification_shorthand}'"))

dim_classification <- data.frame(dim_description = 'Smoothed unemployment rate',
                                 dim_id = get_next_dim_id(),
                                 dim_code = 1,
                                 dim_source = "LMIP",
                                 dim_classification_name = "Unemployment rate",
                                 dim_classification_shorthand = new_indicator$dim_var_1_classification_shorthand)

# Write this new classification to the classification dimension table:
dbWriteTable(con, "d_dim_classifications", dim_classification, overwrite = FALSE, append = TRUE)

# ---------- Prepare for upload ----------

d_regions <- dbGetQuery(con, "SELECT region_name, region_id
                                       FROM d_regions
                                       WHERE regional_short_classification = 'SA2'")

upload_data <- dim_unemployment_sa2 %>%
  filter(!is.na(value)) %>%
  mutate(indicator_id = {new_indicator$indicator_id}) %>%
  mutate(dim_id = get_next_dim_id() - 1) %>%
  left_join(d_regions, by = c("sa2_name" = "region_name")) %>%
  filter(!is.na(region_id)) %>%
  select(observation_date, region_id, indicator_id, dim_id, value) %>%
  mutate(observation_date = as.character(observation_date))

# ---------- Upload to database ----------

dbWriteTable(con, "f_regional_data_one_dim", upload_data, overwrite = FALSE, append = TRUE)

