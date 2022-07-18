read_vic_school_temp <- function(){


  # DET doesn't have this data readily available, so our approach is to:
  ## 1. Treat the list of schools in enrolment data as the definitive list of schools
  ## 2. Match that with myschools data in DAWN - just the postcode
  ## 3. Aggregate to counts of schools in each postcode
  ## 4. Include these new schools and labelled as new schools

  # Download definitive list of schools
  download_if_fresh("https://www.education.vic.gov.au/Documents/about/research/datavic/dv300-allschoolsFTEenrolmentsFeb2020.csv",
                    "raw-data/school-enrolment-2020.csv")

  suppressMessages({ # parsing warnings and messages
    suppressWarnings({
      all_schools <- readr::read_csv("raw-data/school-enrolment-2020.csv") %>%
        janitor::clean_names() %>%
        distinct(education_sector, school_name, school_type, school_no) %>%
        mutate(school_name = str_to_title(school_name))
    })
  })

  # all schools in Vic and their postcode from DAWN
  all_schools_long_lat <- dbGetQuery(dawn, "SELECT name AS school_name, postcode, sector AS education_sector
                                          FROM [schools_ed].[v_schools]
                                          WHERE state_or_territory = 'Victoria' AND status = 'Open'") %>%
    arrange(school_name, postcode, education_sector) %>%
    as_tibble() %>%
    mutate(school_name = str_to_title(school_name))

  # Joining the two lists of school by name (we don't have other columns to join on) comes with the complication that
  # school names are not unique - e.g. we have 36 St Mary's School - this will result in a many-to-many match.
  # A way to overcome this is to group up schools with the same name and sector, and compare the count between DAWN and DET
  # i.e. there are the same number of schools with the same name and sector.
  # This works since we're only after number of schools per area, but wouldn't work if we're after school enrolment per area
  # The latter would require proper data from DET.

  school_count_by_name_dawn <- all_schools_long_lat %>%
    group_by(school_name, education_sector) %>%
    summarise(n_dawn = n())

  school_matched_by_name <- all_schools %>%
    # n schools by name
    group_by(school_name, education_sector) %>%
    summarise(n_det = n()) %>%
    # compare with DAWN
    full_join(school_count_by_name_dawn, by = c("school_name", "education_sector")) %>%
    replace_na(list(n_det = 0, n_dawn = 0)) %>%
    mutate(diff = n_det - n_dawn) %>%
    # the matched ones
    filter(diff == 0) %>%
    ungroup() %>%
    mutate(school_name_sector_col = glue("{school_name} | {education_sector}"))

  # Number of schools per postcode for those that has a match
  n_schools_with_postcode_matched <- all_schools_long_lat %>%
    mutate(school_name_sector_col = glue("{school_name} | {education_sector}")) %>%
    filter(school_name_sector_col %in% school_matched_by_name$school_name_sector_col) %>%
    count(postcode, education_sector)

  stopifnot(sum(n_schools_with_postcode_matched$n) == sum(school_matched_by_name$n_det))

  # For the school names that can't match with DAWN, we export the list to a separate csv
  # and manually populate the postcode column
  if(!file.exists("raw-data/school_not_in_dawn_postcode_completed.csv")){
    school_not_matched_by_name <- all_schools %>%
      filter(!school_name %in% school_matched_by_name$school_name) %>%
      write_csv("output/school_not_in_dawn_postcode.csv")
  } else{
    school_not_matched_by_name <- readr::read_csv("raw-data/school_not_in_dawn_postcode_completed.csv",
                                           col_types = "ccccc")
  }

  n_schools_with_postcode_not_matched <- school_not_matched_by_name %>%
    count(postcode, education_sector)

  # Final list
  n_schools_per_postcode_open <- n_schools_with_postcode_not_matched %>%
    bind_rows(n_schools_with_postcode_matched) %>%
    group_by(postcode, education_sector) %>%
    summarise(value = sum(n)) %>%
    mutate(school_status = "Open in 2020",
           observation_date = "2020-02-01") %>% # per data temporal coverage on the source website
    ungroup()

  stopifnot(sum(n_schools_per_postcode_open$value) == nrow(all_schools))

  # New schools
  ## a df of the 42 new schools due to open post-2021 from the DET website
  ## https://www.schoolbuildings.vic.gov.au/blog/Pages/100-New-Schools.aspx
  n_schools_per_postcode_new <- tribble(
    ~school_name,                              ~postcode,
    #"Truganina North Secondary School" location TBD
    "Camms Road Primary School",              "3977",
    "Hayes Hill Primary School",              "3064",
    "Holyoake Parade Primary School",         "3024",
    "Lollypop Creek Primary School",          "3030",
    #"Merrifield West Secondary School" location TBD
    "Mount Ridley Special School",            "3064",
    "North Melbourne Primary School",         "3051",
    #"Officer Rix Road Primary School" location TBD
    #"Riverdale East Primary School", location TBD
    #"Rockbank Murray Road Primary School"
    "Tarneit Missen House Primary School",    "3029",
    #"Wollert East Secondary School" location TBD
    #"Wollert West Primary School" location TBD
    "Bass Coast College - San Remo Campus",   "3925",
    "Clyde Creek Primary School",             "3978",
    "Clyde Secondary College",                "3978",
    "Deanside Primary School",                "3336",
    "Endeavour Hills Specialist School",      "3802",
    "Fitzroy Gasworks",                       "3068",
    "Gilgai Plains Primary School",           "3064",
    "Greater Shepparton Secondary College",   "3630",
    "Greenvale Secondary College",            "3059",
    "McKinnon Secondary College",             "3204",
    "Port Melbourne Secondary College",       "3207",
    "Strathtulloh Primary School",            "3338",
    "Willowbank Primary School",              "3437",
    "Wollert Primary School",                 "3750",
    "Aintree Primary School",                 "3336",
    "Cranbourne West Secondary College",      "3977",
    "Docklands Primary School",               "3008",
    "Edenbrook Secondary College",            "3810",
    "Edgars Creek Primary School",            "3750",
    "Eynesbury Primary School",               "3338",
    "Footscray High School",                  "3011",
    "Gaayip-Yagila Primary School",           "3064",
    "Garrang Wilam Primary School",           "3029",
    "Keelonith Primary School",               "3059",
    "Oberon High School",                     "3217",
    "Orchard Park Primary School",            "3809",
    "Ramlegh Park Primary School",            "3978",
    "Riverbend Primary School",               "3024") %>%
    count(postcode) %>%
    rename(value = n) %>%
    mutate(school_status = "New or planned in 2021",
           education_sector = "Government",
           observation_date = "2021-08-13") %>% # the day we took this list from the website
    ungroup()

  n_schools_per_postcode <- bind_rows(n_schools_per_postcode_open, n_schools_per_postcode_new)

  #--------------Update dimension table--------------------------------

  # Remove any prior versions of this data:
  dbExecute(con, glue("DELETE FROM d_dim_classifications
                     WHERE dim_classification_shorthand = '{new_indicator$dim_var_1_classification_shorthand}'"))
  dbExecute(con, glue("DELETE FROM d_dim_classifications
                     WHERE dim_classification_shorthand = '{new_indicator$dim_var_2_classification_shorthand}'"))

  d_sector <- n_schools_per_postcode %>%
    distinct(education_sector) %>%
    mutate(dim_id = seq(get_next_dim_id(), length.out = n()),
           dim_code = 1:n(),
           dim_source = "Department of Education and Training",
           dim_classification_name = "Education sector",
           dim_classification_shorthand = new_indicator$dim_var_1_classification_shorthand) %>%
    rename(dim_description = education_sector)

  # Write this new classification to the classification dimension table:
  dbWriteTable(con, "d_dim_classifications", d_sector, overwrite = FALSE, append = TRUE)

  d_school_status <- n_schools_per_postcode %>%
    distinct(school_status) %>%
    mutate(dim_id = seq(get_next_dim_id(), length.out = n()),
           dim_code = 1:n(),
           dim_source = "Department of Education and Training",
           dim_classification_name = "School status",
           dim_classification_shorthand = new_indicator$dim_var_2_classification_shorthand) %>%
    rename(dim_description = school_status)

  # Write this new classification to the classification dimension table:
  dbWriteTable(con, "d_dim_classifications", d_school_status, overwrite = FALSE, append = TRUE)

  #-------------Prepare fact table and upload------------

  d_regions_lga <- dbGetQuery(con, "SELECT region_code, region_id, metro_region
                                  FROM d_regions
                                  WHERE regional_short_classification = 'Postcode'")

  f_n_schools <- n_schools_per_postcode %>%
    left_join(d_regions_lga, by = c("postcode" = "region_code")) %>%
    # postcode in Metro Melb
    filter(!is.na(metro_region)) %>%
    left_join(select(d_sector, dim_description, dim_one_id = dim_id), by = c("education_sector" = "dim_description")) %>%
    left_join(select(d_school_status, dim_description, dim_two_id = dim_id), by = c('school_status' = 'dim_description')) %>%
    mutate(indicator_id = {new_indicator$indicator_id}) %>%
    select(observation_date, region_id, indicator_id, dim_one_id, dim_two_id, value)

  # ---------- Upload to database ----------
  dbWriteTable(con, "f_regional_data_two_dim", f_n_schools, overwrite = FALSE, append = TRUE)


}
