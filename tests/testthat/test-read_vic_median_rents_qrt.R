

test_that("Vic median rents file can be parsed", {

  rent <- read_vic_median_rents(test = TRUE)
  columns <- colnames(rent)

  expect_s3_class(rent, 'data.frame')
  expect_equal(columns, c("district",
                           "area",
                           "property",
                           "date",
                           "variable",
                           "value",
                           "mon",
                           "mon_num",
                           "yr"))
  expect_type(rent$value, 'double')
  expect_equal(class(rent$date), 'Date')

})


test_that('Vic median rents URL has not changed', {

  url <- 'https://www.dffh.vic.gov.au/publications/rental-report'
  search_term <- 'quarterly-median-rents-local-government-area'

  url <- get_latest_download_url(url, search_term)

  expect_false(httr::http_error(url$url))
  expect_false(httr::http_error(url$base_url))

})



