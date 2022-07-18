#' @title DSS Payment Demographic Data
#'
#' @description Extracting and cleaning payment data from the Department of
#' Social Services for each quarter back to September 2013. Data is transformed
#' into long, and is filtered by LGA and payment type.
#'
#' @param url Base url which contains links to all data, default value is: \link[https://data.gov.au/data/dataset/dss-payment-demographic-data/]
#' @param filename Default folder name is tempdir()
#'
#' @importFrom janitor clean_names
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#'   df <- read_dss_payments()
#' }
#'
read_dss_payments <- function(url = urls$read_dss_payments, foldername = tempdir()) {

    # Find all the links to the xlsx files to download ----------------------------

    # Only the xlsx files hold the demographic data we need
    # One has a filename "...-june-2014-jan-2020-edit.xlsx" but I've confirmed it's june-2014 only
    # Not all files have data at the SA2 level, but most (if not all) have LGA-level data


  pg <- rvest::read_html(url)

  links <- rvest::html_attr(rvest::html_nodes(pg, "a"), "href") %>%
    as_tibble() %>%
    # Links to xlsx files only as these hold the demographic data we need
    dplyr::filter(stringr::str_detect(value, "\\.xlsx$")) %>%
    dplyr::rename(url = value) %>%
    unique() %>%
    # Extract filename
    dplyr::mutate(filename = tolower(gsub(".*download/(.+).xlsx.*", "\\1", .data$url))) %>%
    # Drop non-demographic-related links (catch typo)
    dplyr::filter(stringr::str_detect(.data$filename, "demographic|demogrphics")) %>%
    # Fix anamalous filename for Sept 2015 (201509)
    dplyr::mutate(filename = stringr::str_replace_all(.data$filename, "201509", "september-2015")) %>%
    # Extract month and year
    dplyr::mutate(month = stringr::str_sub(stringr::str_extract(.data$filename, "march|mar|june|september|sept|december|dec"), 1, 3),
           year = stringr::str_extract(filename, "[:digit:]+"),
           clean_filename = paste("dss-demographics", year, month, sep = "-"))

  # ----- Download the data

  for (i in 1:nrow(links)) {
    dest_loc <- glue::glue("{foldername}/{links$clean_filename[i]}.xlsx")
    download.file(links$url[i], dest_loc, mode = "wb")
  }

  # ----- Find which files have which pages

  file_details <- tibble(filename = list.files(foldername)) %>%
    filter(stringr::str_detect(filename, 'dss-demographics-') == TRUE) %>%
    mutate(filepath = glue::glue("{foldername}/{filename}"),
           year = stringr::str_sub(filename, 18, 21),
           month = stringr::str_sub(filename, 23, 25),
           quarter_end_date = lubridate::parse_date_time(glue::glue("{year}-{month}"), orders = "ym"))

  file_details$has_sa2_sheet <- NA
  file_details$has_sa2ws_sheet <- NA
  file_details$has_lga_sheet <- NA
  file_details$has_lgaws_sheet <- NA
  file_details$has_pc_sheet <- NA
  file_details$has_pcws_sheet <- NA

  # Note that there are xlsx page names that have a whitespace at the end
  for (i in 1:nrow(file_details)) {
    file_details$has_sa2_sheet[i] <- "SA2" %in% readxl::excel_sheets(path = file_details$filepath[i])
    file_details$has_sa2ws_sheet[i] <- "SA2 " %in% readxl::excel_sheets(path = file_details$filepath[i])
    file_details$has_lga_sheet[i] <- "LGA" %in% readxl::excel_sheets(path = file_details$filepath[i])
    file_details$has_lgaws_sheet[i] <- "LGA " %in% readxl::excel_sheets(path = file_details$filepath[i])
    file_details$has_pc_sheet[i] <- "Postcode" %in% readxl::excel_sheets(path = file_details$filepath[i])
    file_details$has_pcws_sheet[i] <- "Postcode " %in% readxl::excel_sheets(path = file_details$filepath[i])
  }

  file_details <- file_details %>%
    mutate(has_sa2_data = has_sa2_sheet | has_sa2ws_sheet,
           has_lga_data = has_lga_sheet | has_lgaws_sheet,
           has_pc_data = has_pc_sheet | has_pcws_sheet)

  # ----- Load the data from each file

  # Upload LGA data
  file_details_lga <- file_details %>%
    filter(has_lga_data)

  all_dss_lga <- tibble()

  for (i in 1:nrow(file_details_lga)){

    filepath <- file_details_lga$filepath[i]
    quarter_end_date <- file_details_lga$quarter_end_date[i]

    suppressWarnings({ # Suppress parsing warnings - expected numeric in SA2 column but was confused by 'Unknown' and 'Total'
      tmp <- tryCatch(
        {
          readxl::read_excel(filepath,
                     sheet = "LGA",
                     col_names = TRUE,
                     skip = 2)
        },
        error = function(e){
          readxl::read_excel(filepath,
                     sheet = "LGA ",
                     col_names = TRUE,
                     skip = 2)
        }
      )
    })

    dss_month_year <- tmp %>%
      mutate_all(as.character) %>%
      mutate(quarter_end_date = quarter_end_date) %>%
      pivot_longer(!c(LGA, `LGA name`, quarter_end_date),
                   names_to = "payment_type",
                   values_to = "payments") %>%
      janitor::clean_names() %>%
      # Get rid of a few records where LGA is unknown
      filter(!is.na(lga_name) & !lga_name %in% c('Total', 'Unknown') & !grepl("Unincorp", lga) &
               !grepl("n.p", payments)) %>%
      # Some payments are presented like "<5" replace these with a value in the middle (since it's count)
      mutate(payments = ifelse(grepl("<", payments), readr::parse_number(payments)/2, payments),
             payments = as.numeric(payments) %>% round())

    all_dss_lga <- all_dss_lga %>% rbind(dss_month_year) %>%
      mutate(quarter_end_date = as.Date(quarter_end_date))
  }

return(all_dss_lga)

}
