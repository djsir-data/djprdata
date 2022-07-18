#' @title Get ABS Counts of Businesses including Entries and Exits
#'
#' @return
#' @export
#' @source https://www.abs.gov.au/statistics/economy/business-indicators/counts-australian-businesses-including-entries-and-exits/latest-release
#' @examples

read_abs_cabee <- function(path = tempdir(), delete = TRUE) {
	cat_name <- readabs::search_catalogues("entries and exits")$catalogue

	suppressMessages(
	readabs::download_abs_data_cube("counts-australian-businesses-including-entries-and-exits",
									cube = 10,
									path = path)
	)
	suppressMessages(
	readabs::download_abs_data_cube("counts-australian-businesses-including-entries-and-exits",
									cube = 11,
									path = path)
	)

	file_names <- list.files(path)[grepl("8165", list.files(path))]

	cabee_emp_raw <- suppressMessages(
		readxl::read_excel(file.path(path, file_names[grepl("10.xls", file_names)]),
					   sheet = 3)
		)

	cabee_emp_raw_release_time <- cabee_emp_raw %>%
		dplyr::filter_all(dplyr::any_vars(stringr::str_detect(., pattern = "Released")))

	cabee_emp_raw_release_time <- as.character(cabee_emp_raw_release_time[ , colSums(is.na(cabee_emp_raw_release_time)) == 0])

	cabee_emp_raw_release_date <- sub(".{0,}\\s([0-9]{1,2}\\D+.{3,}\\D+[0-9]{4}).{0,}", "\\1", cabee_emp_raw_release_time) %>%
		as.Date(format = "%d %B %Y")

	cabee_emp_raw_time <- cabee_emp_raw %>%
		dplyr::filter_all(dplyr::any_vars(stringr::str_detect(., pattern = "Local Government Area")))

	cabee_emp_raw_time <- as.character(cabee_emp_raw_time[ , colSums(is.na(cabee_emp_raw_time)) == 0])

	cabee_emp_raw_date <- sub(".{0,}\\s(.{3,}\\D+[0-9]{4}).{0,}", "\\1", cabee_emp_raw_time) %>%
		paste("01") %>%
		as.Date(format = "%B %Y %d")

	names(cabee_emp_raw) <- paste(cabee_emp_raw[5,], cabee_emp_raw[6,])

	cabee_emp_raw <- utils::tail(cabee_emp_raw, -6) %>%
		tidyr::pivot_longer(cols = names(cabee_emp_raw)[grepl("no.", names(cabee_emp_raw))],
					 		names_to = "category") %>%
		dplyr::mutate(category = gsub(" no.", "", category),
					  series = "business_employment_range") %>%
		dplyr::rename_with(~ tolower(gsub(" ", "_", .x, fixed = TRUE)))

	cabee_emp_raw$date <- cabee_emp_raw_date

	cabee_emp_raw$release <- cabee_emp_raw_release_date

	cabee_turn_raw <- suppressMessages(
		readxl::read_excel(file.path(path, file_names[grepl("11.xls", file_names)]),
					   sheet = 2)
		)

	cabee_turn_raw_release_time <- cabee_turn_raw %>%
		dplyr::filter_all(dplyr::any_vars(stringr::str_detect(., pattern = "Released")))

	cabee_turn_raw_release_time <- as.character(cabee_turn_raw_release_time[ , colSums(is.na(cabee_turn_raw_release_time)) == 0])

	cabee_turn_raw_release_date <- sub(".{0,}\\s([0-9]{1,2}\\D+.{3,}\\D+[0-9]{4}).{0,}", "\\1", cabee_turn_raw_release_time) %>%
		as.Date(format = "%d %B %Y")

	cabee_turn_raw_time <- cabee_turn_raw %>%
		dplyr::filter_all(dplyr::any_vars(stringr::str_detect(., pattern = "Local Government Area")))

	cabee_turn_raw_time <- as.character(cabee_turn_raw_time[ , colSums(is.na(cabee_turn_raw_time)) == 0])

	cabee_turn_raw_date <- sub(".{0,}\\s(.{3,}\\D+[0-9]{4}).{0,}", "\\1", cabee_turn_raw_time) %>%
		paste("01") %>%
		as.Date(format = "%B %Y %d")

	names(cabee_turn_raw) <- paste(cabee_turn_raw[5,], cabee_turn_raw[6,])

	cabee_turn_raw <- utils::tail(cabee_turn_raw, -6) %>%
		tidyr::pivot_longer(cols = names(cabee_turn_raw)[grepl("no.", names(cabee_turn_raw))],
					 		names_to = "category") %>%
		dplyr::mutate(category = gsub(" no.", "", category),
					  series = "business_turnover_range" ) %>%
		dplyr::rename_with(~ tolower(gsub(" ", "_", .x, fixed = TRUE)))

	cabee_turn_raw <- utils::tail(cabee_turn_raw, -6)

	cabee_turn_raw$date <- cabee_turn_raw_date

	cabee_turn_raw$release <- cabee_turn_raw_release_date

	df_cabee <- dplyr::bind_rows(cabee_emp_raw, cabee_turn_raw) %>%
	  mutate(value = as.numeric(value))

	if(delete){
		unlink(file.path(path, file_names))
	}

	df_cabee
}
