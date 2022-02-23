

#' @title Victorian Crime Statostics, Counts and rates.
#'
#' @description Each quarter the Victorian
#' \link[https://www.crimestatistics.vic.gov.au/]{Crime Statistics Agence (CSA)}
#' publish data on the number of criminal offenses and related information. Data is included for
#' each Local Government Area
#'
#' @note Replaces the data import component of
#' \itemize{
#'   \item \url{https://github.com/djpr-data/osd-nous-dashboard/blob/main/processing/offences-by-location-type-lga.R}
#'   \item \url{https://github.com/djpr-data/osd-nous-dashboard/blob/main/processing/offence-by-division-postcode.R}
#'   \item \url{https://github.com/djpr-data/osd-nous-dashboard/blob/main/processing/offences-rate-by-division-lga.R}
#' }
#'
<<<<<<< HEAD
#' @param dataset Required. select the database c("offence_by_location_by_lga", "offence_by_postcode_suburb", "offence_rate_by_lga")
=======
#' @param dataset Required. select the database c("offence_by_location_by_lga", "offence_by_postcode", "offence_rate_by_lga")
>>>>>>> 32a6690b2bdb5bad504e80139fda77ab7aaa4422
#' @param dim_code_input Optional. Filter by dim code (i.e. 11)
#' @param lga_input Optional. Filter by LGA (i.e. "Alpine")
#' @param suburb_input Optional. Filter by Suburb (i.e. "Dederang")
#' @param offence_div_code_input Optional. Filter by Offence Code (i.e. "A")
#'
#' @import rvest
#' @import httr
#' @import readxl
#' @import dplyr
#' @importFrom tidyr pivot_longer fill
#' @importFrom lubridate month year
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#'
#' @return data.frame
#'
#' @export
#' @source \url{https://www.crimestatistics.vic.gov.au/}
#' @examples
#' \dontrun{
#'   lga_offence_rates <- read_vic_crime_stats(dataset = "offence_rate_by_lga")
#'
#'   Alpine_lga_offence_rates <- read_vic_crime_stats(dataset = "offence_rate_by_lga", lga_input = "Alpine")
#' }
<<<<<<< HEAD
read_vic_crime_stats <- function(dataset,
                                 lga_input,
                                 suburb_input,
                                 postcode_input,
                                 dim_code_input,
                                 offence_div_code_input,
                                 test = FALSE){


  #Test scenario:
  if(test == TRUE){
    lga_input = "Alpine"
    suburb = "Dederang"
    dim_code_input = 11
    offence_div_code_input = "A"
  }

  stopifnot(!missing(dataset))
  stopifnot(is.character(dataset))
=======
read_vic_crime_stats <- function(dataset = NULL,
                                 lga_input = NULL,
                                 suburb_input = NULL,
                                 dim_code_input = NULL,
                                 offence_div_code_input = NULL
                                 ){

  if(is.null(dataset)){message("You must select a database, there are three to choose from:

                               offence_by_location_by_lga
                               offence_by_postcode
                               offence_rate_by_lga")}else{
>>>>>>> 32a6690b2bdb5bad504e80139fda77ab7aaa4422

  data_url <- 'https://www.crimestatistics.vic.gov.au/crime-statistics/latest-victorian-crime-data/download-data'
  search_term <- 'Data_Tables_LGA_Recorded_Offences'

  for (i in 1:5){
    if(exists("URL")==FALSE){
      URL <- get_latest_download_url(data_url, search_term)
      }
  }

  extension <- stringr::str_extract(stringr::str_sub(URL$url,-20,-1), '\\b[a-z]+')
  month <- stringr::str_extract(stringr::str_sub(URL$url,-20,-1), '[^_]+')
  year<- stringr::str_extract(stringr::str_sub(URL$url,-20,-1), '[^A-z.]+')
  url_filename <- paste0(stringr::str_extract_all(URL$url,"\\w+")[[1]][9],".",stringr::str_extract_all(URL$url,"\\w+")[[1]][10])

  message(glue::glue('Latest Data From: {paste0(month," ",year)}'))

  filename <- download_excel(URL$url)

  sheets <- readxl::excel_sheets(filename)

  if(dataset=="offence_by_location_by_lga"){
    df<-readxl::read_excel(filename,sheet = "Table 04")%>%
<<<<<<< HEAD
      mutate(observation_date = as.Date(glue("{Year}-03-30")),
=======
      mutate(observation_date = glue("{Year}-03-30"),
>>>>>>> 32a6690b2bdb5bad504e80139fda77ab7aaa4422
             value = `Offence Count`,
             dim_description = stringr::str_sub(`Location Subdivision`, 3, -1),
             dim_description = stringr::str_sub(dim_description,2,-1)) %>%
      rename(local_government_area = `Local Government Area`,
             location_subdivision = `Location Subdivision`)%>%
      mutate(dim_code = as.numeric(stringr::str_extract(location_subdivision, "[0-9]*")))%>%
      select(observation_date, local_government_area, location_subdivision, dim_code, value, dim_description) %>%
      group_by(observation_date, local_government_area, location_subdivision, dim_description, dim_code) %>%
      summarize(value = sum(value)) %>%
<<<<<<< HEAD
      mutate(value = as.numeric(value)) %>%
=======
>>>>>>> 32a6690b2bdb5bad504e80139fda77ab7aaa4422
      ungroup()

    df_filtered <- df

<<<<<<< HEAD
    if(!missing(lga_input)){
=======
    if(exists("lga_input")==TRUE){
>>>>>>> 32a6690b2bdb5bad504e80139fda77ab7aaa4422
      df_filtered <- df_filtered%>%
        filter(.data$local_government_area == lga_input)
    }

<<<<<<< HEAD
    if(!missing(dim_code_input)){
    df_filtered <- df_filtered%>%
      filter(.data$dim_code==dim_code_input)
=======
    if(exists("dim_code")==TRUE){
    df_filtered <- df_filtered%>%
      filter(.data$dim_code==dim_code)
>>>>>>> 32a6690b2bdb5bad504e80139fda77ab7aaa4422
    }

  }

<<<<<<< HEAD
  if(dataset=="offence_by_postcode_suburb"){
    df<-readxl::read_excel(filename,sheet = "Table 03")%>%
      mutate(observation_date = as.Date(glue("{Year}-09-30")),
=======
  if(dataset=="offence_by_postcode"){
    df<-readxl::read_excel(filename,sheet = "Table 03")%>%
      mutate(observation_date = glue("{Year}-09-30"),
>>>>>>> 32a6690b2bdb5bad504e80139fda77ab7aaa4422
             value = `Offence Count`) %>%
      rename(local_government_area = `Local Government Area`,
             postcode = "Postcode",
             suburb = `Suburb/Town Name`,
             offence_div = `Offence Division`,
             offence_subdiv = `Offence Subdivision`)%>%
      mutate(offence_div_code = sub("\\ .*", "", offence_div),
             offence_subdiv_code = sub("\\ .*", "", offence_subdiv),
             offence_div = stringr::str_sub(offence_div,3,-1),
             offence_subdiv = stringr::str_sub(offence_subdiv,4,-1))%>%
      select(observation_date, local_government_area, suburb, postcode, offence_div, offence_div_code, offence_subdiv, offence_subdiv_code, value) %>%
      group_by(observation_date, local_government_area, suburb, postcode, offence_div, offence_div_code) %>%
      summarize(value = sum(value)) %>%
<<<<<<< HEAD
      mutate(value = as.numeric(value)) %>%
=======
>>>>>>> 32a6690b2bdb5bad504e80139fda77ab7aaa4422
      ungroup()

    df_filtered <- df

<<<<<<< HEAD
    if(!missing(lga_input)){
      df_filtered <- df_filtered%>%
        filter(.data$local_government_area == lga_input)
    }
    if(!missing(suburb_input)){
      df_filtered <- df_filtered%>%
        filter(.data$suburb == suburb_input)
    }
    if(!missing(postcode_input)){
      df_filtered <- df_filtered%>%
        filter(.data$postcode == postcode_input)
    }
    if(!missing(offence_div_code_input)){
=======
    if(exists("lga_input")==TRUE){
      df_filtered <- df_filtered%>%
        filter(.data$local_government_area == lga_input)
    }
    if(exists("suburb_input")==TRUE){
      df_filtered <- df_filtered%>%
        filter(.data$suburb == suburb_input)
    }
    if(exists("postcode_input")==TRUE){
      df_filtered <- df_filtered%>%
        filter(.data$postcode == postcode_input)
    }
    if(exists("offence_div_code_input")==TRUE){
>>>>>>> 32a6690b2bdb5bad504e80139fda77ab7aaa4422
      df_filtered <- df_filtered%>%
        filter(.data$offence_div_code == offence_div_code_input)
    }

  }

  if(dataset=="offence_rate_by_lga"){
    df<-readxl::read_excel(filename,sheet = "Table 02")%>%
<<<<<<< HEAD
      mutate(observation_date = as.Date(glue("{Year}-09-30")),
=======
      mutate(observation_date = glue("{Year}-09-30"),
>>>>>>> 32a6690b2bdb5bad504e80139fda77ab7aaa4422
             value = `LGA Rate per 100,000 population`) %>%
      rename(local_government_area = `Local Government Area`,
             offence_div = `Offence Division`)%>%
      mutate(offence_div_code = sub("\\ .*", "", offence_div),
             offence_div = sub("*.\\ ", "", offence_div))%>%
      select(observation_date, local_government_area, offence_div, offence_div_code, value) %>%
      group_by(observation_date, local_government_area, offence_div, offence_div_code) %>%
      summarize(value = sum(value)) %>%
<<<<<<< HEAD
      mutate(value = as.numeric(value)) %>%
=======
>>>>>>> 32a6690b2bdb5bad504e80139fda77ab7aaa4422
      ungroup()

    df_filtered <- df

<<<<<<< HEAD
    if(!missing(lga_input)){
=======
    if(exists("lga_input")==TRUE){
>>>>>>> 32a6690b2bdb5bad504e80139fda77ab7aaa4422
      df_filtered <- df_filtered%>%
        filter(.data$local_government_area == lga_input)
    }

<<<<<<< HEAD
    if(!missing(offence_div_code_input)){
=======
    if(exists("offence_div_code_input")==TRUE){
>>>>>>> 32a6690b2bdb5bad504e80139fda77ab7aaa4422
      df_filtered <- df_filtered%>%
        filter(.data$offence_div_code == offence_div_code_input)
    }

  }

  df_filtered

<<<<<<< HEAD
}


=======
  }

}

>>>>>>> 32a6690b2bdb5bad504e80139fda77ab7aaa4422
