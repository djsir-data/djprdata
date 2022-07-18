


#' @title Google COVID Mobility Data
#' @description download and transpose to long-format
#' "Google COVID-19 Community Mobility Reports".
#' @source \link{https://www.google.com/covid19/mobility/}
#'
#' @param filepath character filename and path
#' @param region character code for Country. Defaults to "AU"
#' @param sub_region_1 character code for State Defaults to "Victoria"
#' @param attribution logical include Google attribution statement
#'
#' @importFrom zip unzip zip_list
#' @importFrom fs path_dir
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#'   read_google_mobility()
#' }
read_google_mobility <- function(url = urls$read_google_mobility,
                                 region = 'AU',
                                 sub_region_1 = 'Victoria',
                                 filepath = NULL,
                                 attribution = FALSE){

  if (is.null(filepath)) {
    filepath <- tempfile(fileext = paste0('.', tools::file_ext(url)))
  }
  resp <- httr::GET(url, httr::write_disk(filepath, overwrite=TRUE))
  status <- httr::http_status(resp)

  assertthat::assert_that(status$category == "Success",
                          msg = glue('Download Failed with message: {status$message}'))
  assertthat::assert_that(httr::http_type(resp) == "application/zip",
                          msg = 'Failed to retrieve a Zip file as Expected')

  aus_csvs <- grep(pattern = region, x = zip::zip_list(filepath)$filename, value = TRUE)

  zip::unzip(filepath, files = aus_csvs, exdir = fs::path_dir(filepath))

  suppressMessages({

    data <- purrr::map_dfr(aus_csvs, function(csv){

      readr::read_csv(file.path(fs::path_dir(filepath), csv)) |>
        dplyr::as_tibble() |>
        janitor::clean_names() |>
        dplyr::filter(sub_region_1 %in% .env$sub_region_1) |>
        tidyr::drop_na(sub_region_2) |> # sub_region_2 is LGA. Those NAs are state-level data
        dplyr::select(sub_region_2,
                      date,
                      contains('percent_change_from_baseline')) |>
        tidyr::pivot_longer(cols = setdiff(dplyr::everything(), dplyr::one_of("date", 'sub_region_2')))

    }) |>
      dplyr::mutate(name = gsub("_percent_change_from_baseline", "", name),
                    name = stringr::str_to_sentence(name),
                    name = gsub("_", " ", name)) |>
      tidyr::drop_na(value)

  })

  if (attribution) {
    data$attribution <- glue::glue('Google LLC "Google COVID-19 Community Mobility Reports". ',
                                   'https://www.google.com/covid19/mobility/ Accessed: {Sys.Date()}')
  }

  return(data)

}
