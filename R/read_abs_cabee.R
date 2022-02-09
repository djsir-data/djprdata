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

	cabee_emp_raw_time <- cabee_emp_raw %>%
		dplyr::filter_all(dplyr::any_vars(stringr::str_detect(., pattern = "Released"))) 

	cabee_emp_raw_time <- as.character(cabee_emp_raw_time[ , colSums(is.na(cabee_emp_raw_time)) == 0])

	cabee_emp_raw_date <- sub(".{0,}\\s([0-9]{1,2}\\D+.{3,}\\D+[0-9]{4}).{0,}", "\\1", cabee_emp_raw_time) %>%
		as.Date(format = "%d %B %Y")

	row_names <- paste(cabee_emp_raw[5,], cabee_emp_raw[6,]) %>%
		tolower() 

	row_names <- gsub("\\.", "", gsub("-", "_", gsub(" ", "_", row_names)))

	names(cabee_emp_raw) <- row_names

	cabee_emp_raw <- tail(cabee_emp_raw, -6)

	cabee_emp_raw$emp_date <- cabee_emp_raw_date

	cabee_turn_raw <- suppressMessages(
		readxl::read_excel(file.path(path, file_names[grepl("11.xls", file_names)]),
					   sheet = 2)
		)

	cabee_turn_raw_time <- cabee_turn_raw %>%
		dplyr::filter_all(dplyr::any_vars(stringr::str_detect(., pattern = "Released"))) 

	cabee_turn_raw_time <- as.character(cabee_turn_raw_time[ , colSums(is.na(cabee_turn_raw_time)) == 0])

	cabee_turn_raw_date <- sub(".{0,}\\s([0-9]{1,2}\\D+.{3,}\\D+[0-9]{4}).{0,}", "\\1", cabee_turn_raw_time) %>%
		as.Date(format = "%d %B %Y")

	row_names <- paste(cabee_turn_raw[5,], cabee_turn_raw[6,]) %>%
		tolower() 

	row_names <- gsub("\\.", "", gsub("\\$", "", gsub(" ", "_", row_names)))

	names(cabee_turn_raw) <- row_names

	cabee_turn_raw <- tail(cabee_turn_raw, -6)

	cabee_turn_raw$turn_date <- cabee_turn_raw_date

	df_cabee <- dplyr::left_join(cabee_emp_raw, cabee_turn_raw, by = c("state_name", "lga_code", "lga_label", "industry_code", "industry_label", "total_no"))

	if(delete){
		unlink(file.path(path, file_names))
	}

	df_cabee
}

