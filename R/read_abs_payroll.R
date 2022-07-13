#' @title Get ABS Weekly Payroll data
#'
#' @return
#' @export
#' @source https://www.abs.gov.au/statistics/labour/earnings-and-work-hours/weekly-payroll-jobs-and-wages-australia/latest-release
#' @examples

read_abs_payroll <- function(path = tempdir(), delete = TRUE) {


	suppressMessages(
	readabs::download_abs_data_cube("weekly-payroll-jobs-and-wages-australia",
									cube = "DO005",
									path = path)
	)

	file_names <- list.files(path)[grepl("6160", list.files(path))]

	df_payroll <- suppressMessages(
		readxl::read_excel(file.path(path, file_names[grepl("DO005", file_names)]),
			sheet = 3)
		)

	names(df_payroll) <- tolower(gsub(" ", "_", as.character(df_payroll[5,])))

	df_payroll <- df_payroll %>%
		utils::tail(-5) %>%
		tidyr::pivot_longer(cols = 4:dplyr::last_col(),
					 names_to = "date") %>%
		dplyr::filter(!grepl("NA",value)) %>%
		dplyr::mutate(date = as.Date(as.numeric(date), origin = "1899-12-30"),
					  value = as.numeric(value)) %>%
		tidyr::separate(state_or_territory, sep = "\\.\\s(?!=[0-9]{1,})", c("state_code", "state_or_territory"), extra = "merge") %>%
		tidyr::separate(statistical_area_level_4, sep = "\\.\\s(?!=[0-9]{1,})", c("sa4_code", "statistical_area_level_4"), extra = "merge") %>%
		tidyr::separate(statistical_area_level_3, sep = "\\.\\s(?!=[0-9]{1,})", c("sa3_code", "statistical_area_level_3"), extra = "merge")

	if(delete){
		unlink(file.path(path, file_names))
	}

	df_payroll

}
