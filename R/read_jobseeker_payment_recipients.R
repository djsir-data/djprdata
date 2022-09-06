#' @title DSS JobSeeker Data
#'
#' @description Extracting and cleaning monthly JobSeeker payment data
#' from the Department of Social Services back to April 2020. Data
#' is transformed into long, and is filtered by SA2.
#'
#' @param url Base url which contains links to all data, default value is:
#' \link[https://data.gov.au/data/dataset/jobseeker-payment-and-youth-allowance-recipients-monthly-profile/]
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#'   df <- read_jobseeker_payments()
#' }


read_jobseeker_payments <- function(url = urls$read_jobseeker_payments) {

  pg <- rvest::read_html(url)

  # Find all the links to the xlsx files to download ----------------------------

  links <- rvest::html_attr(rvest::html_nodes(pg, "a"), "href") %>%
    as_tibble() %>%
    # Links to xlsx files only as these hold the data we need
    filter(stringr::str_detect(value, "\\.xlsx$")) %>%
    rename(url = value) %>%
    unique() %>%
    # Extract filename
    dplyr::mutate(filename = tolower(gsub(".*download/(.+).xlsx.*", "\\1", url))) %>%
    # Extract month and year
    dplyr::mutate(month = stringr::str_extract(filename, glue::glue_collapse(tolower(month.name), sep = "|")),
           year = stringr::str_extract(filename, "2020|2021|2022|2023|2024"),
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
        keep_trying <- FALSE             # fail safe
      })
    }

  }

  #--------------Import data-----------

  jobseeker_df <- tibble()
  for (this_file in list.files("data-raw/dss-jobseeker")) {

    this_month <- stringr::str_extract(this_file, glue::glue_collapse(tolower(month.name), sep = "|"))
    this_year <- stringr::str_extract(this_file, "2020|2021|2022|2023|2024")

    this_file_processed <- readxl::read_xlsx(glue::glue("data-raw/dss-jobseeker/{this_file}"),
                                     sheet = "Table 4 - By SA2", skip = 6,
                                     col_types = rep('text', 4)) %>%
      janitor::clean_names() %>%
      drop_na(sa2_name) %>% # remove Unknown, Total, and other random footer rows
      # Some payments are presented like "<5" replace these with a value in the middle (since it's count)
      dplyr::mutate(value = ifelse(grepl("<", job_seeker_payment), readr::parse_number(job_seeker_payment)/2, job_seeker_payment),
             value = as.numeric(value) %>% round(),
             month = this_month,
             year = this_year) %>%
      select(sa2_name, value, month, year) |>
      mutate(mon_yr = str_c(month, year, sep = "-"))

    jobseeker_df <- bind_rows(jobseeker_df, this_file_processed)

  }

return(jobseeker_df)

}
