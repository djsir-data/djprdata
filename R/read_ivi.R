#' @title Get LMIP Internet Vacancy Index data
#'
#' @return
#' @export
#' @source https://lmip.gov.au/default.aspx?LMIP/GainInsights/VacancyReport
#' @examples

read_ivi <- function(path = tempdir(), delete = TRUE) {
	url <- "https://lmip.gov.au/default.aspx?LMIP/GainInsights/VacancyReport"

	lmip_page <- rvest::read_html(url)

	link_text <- lmip_page %>%
		rvest::html_nodes(".download-link") %>%
		rvest::html_text()

	links <- lmip_page %>%
		rvest::html_nodes(".download-link") %>%
		rvest::html_attr("href")

	matching_link <- links[grepl("Occupation", link_text)]

	matching_link <- paste0("https://lmip.gov.au/", matching_link)

	excel_location <- file.path(path, "ivi.xlsx")

	download.file(url = matching_link[grepl("xlsx", matching_link)],
	              destfile = excel_location,
	              mode = "wb")

	raw_data <- readxl::read_excel(excel_location, sheet = 2)

	if(delete){
		unlink(excel_location)
	}

	names(raw_data) <- tolower(names(raw_data))

	df_ivi <- raw_data %>%
		tidyr::pivot_longer(cols = 4:dplyr::last_col(),
							names_to = "date") %>%
		dplyr::mutate(date = as.Date(as.numeric(date), origin = "1899-12-30"))

	return(df_ivi)
}