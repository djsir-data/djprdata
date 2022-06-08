# Create directory
suppressWarnings(dir.create("raw-data/dss-jobseeker"))

pg <- read_html("https://data.gov.au/data/dataset/jobseeker-payment-and-youth-allowance-recipients-monthly-profile")

# Find all the links to the xlsx files to download ----------------------------

links <- html_attr(html_nodes(pg, "a"), "href") %>%
  as_tibble() %>%
  # Links to xlsx files only as these hold the data we need
  filter(str_detect(value, "\\.xlsx$")) %>%
  rename(url = value) %>%
  unique() %>%
  # Extract filename
  mutate(filename = tolower(gsub(".*download/(.+).xlsx.*", "\\1", url))) %>%
  # Extract month and year
  mutate(month = str_extract(filename, glue::glue_collapse(tolower(month.name), sep = "|")),
         year = str_extract(filename, "2020|2021|2022|2023|2024"),
         clean_filename = paste("dss-jobseeker", year, month, sep = "-"))


# ----- Download the data

for (i in 1:nrow(links)) {

  dest_loc <- glue::glue("data-raw/dss-jobseeker/{links$clean_filename[i]}.xlsx")

  # The URL returns false errors sometimes if downloading is too frequent
  # we will keep trying until no errors
  keep_trying <- TRUE
  while(keep_trying){
    tryCatch({
      download.file(links$url[i], dest_loc, mode = "wb")
      keep_trying <- FALSE
    }, warning = function(e){
      print("second try")
    })
  }

}

#--------------Import data-----------

jobseeker_df <- tibble()
for (this_file in list.files("data-raw/dss-jobseeker")) {

  this_month <- str_extract(this_file, glue::glue_collapse(tolower(month.name), sep = "|"))
  this_year <- str_extract(this_file, "2020|2021")

  this_file_processed <- readxl::read_xlsx(glue::glue("data-raw/dss-jobseeker/{this_file}"),
                                   sheet = "Table 4 - By SA2", skip = 6,
                                   col_types = rep('text', 4)) %>%
    janitor::clean_names() %>%
    drop_na(sa2_name) %>% # remove Unknown, Total, and other random footer rows
    # Some payments are presented like "<5" replace these with a value in the middle (since it's count)
    mutate(value = ifelse(grepl("<", job_seeker_payment), parse_number(job_seeker_payment)/2, job_seeker_payment),
           value = as.numeric(value) %>% round(),
           month = this_month,
           year = this_year) %>%
    select(sa2_name, value, month, year)

  jobseeker_df <- bind_rows(jobseeker_df, this_file_processed)

}

#--------------Update dimension table--------------------------------

dim_classification <- data.frame(dim_description = 'JobSeeker payment recipients',
                                 dim_id= get_next_dim_id(),
                                 dim_code = 1,
                                 dim_source = "DSS",
                                 dim_classification_name = "JobSeeker payment recipient",
                                 dim_classification_shorthand = new_indicator$dim_var_1_classification_shorthand)

# Remove any prior versions of this data:
dbExecute(con, glue("DELETE FROM d_dim_classifications WHERE dim_classification_shorthand = '{new_indicator$dim_var_1_classification_shorthand}'"))

# Write this new classification to the classification dimension table:
dbWriteTable(con, "d_dim_classifications", dim_classification, overwrite = FALSE, append = TRUE)

# ---------- Prepare for upload ----------

d_regions <- dbGetQuery(con, "SELECT region_id, region_name
                                       FROM d_regions
                                       WHERE regional_short_classification = 'SA2'")

f_jobkeeper <- jobseeker_df %>%
  left_join(d_regions, by = c("sa2_name" = "region_name")) %>%
  # Not all SA2s match with what we have in d_regions
  drop_na(region_id) %>%
  mutate(dim_id = dim_classification$dim_id,
         indicator_id = new_indicator$indicator_id,
         month_num = match(month, tolower(month.name)),
         date = as_date(glue("{year}-{month_num}-01")),
         observation_date = as.character(ceiling_date(as.Date(date), unit = "month") - 1)) %>% # end of month
  select(region_id, dim_id, observation_date, indicator_id, value)

# ---------- Upload to database ----------
dbWriteTable(con, "f_regional_data_one_dim", f_jobkeeper, overwrite = FALSE, append = TRUE)
