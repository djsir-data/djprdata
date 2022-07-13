

test_that("Vic property price files can be parsed", {

  prices <- read_vic_property_prices()
  columns <- colnames(prices)

  expect_s3_class(prices, 'data.frame')
  expect_equal(columns, c("locality",
                           "cdate",
                           "median",
                           "time_scale",
                           "type",
                           "date"))
  expect_type(prices$median, 'double')
  expect_type(prices$locality, 'integer')
  expect_type(prices$time_scale, 'integer')
  expect_type(prices$type, 'integer')
  expect_equal(class(prices$date), 'Date')

  expect_equal(levels(prices$type), c("House",
                                      "Unit",
                                      "Vacant")
)

})


test_that("Vic property prices URL's have not changed", {

  url <- 'https://www.land.vic.gov.au/valuations/resources-and-reports/property-sales-statistics'
  search_term <- 'house|unit|vacant'

  url <- get_latest_download_url(url, search_term)
  url$url <- url$url[endsWith(url$url, '.xls')]

  expect_false(httr::http_error(url$base_url))

  for (i in 1:length(url$url)) {
    expect_false(httr::http_error(url$url[i]))
  }

})


