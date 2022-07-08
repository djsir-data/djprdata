# testing the get_latest_download_url() function

test_that("Get URL for latest Dataset returns two URLs that exist", {

  # Is it okay to hardcode an example, like this:
  url <- get_latest_download_url(
    url = "https://labourmarketinsights.gov.au/our-research/internet-vacancy-index/",
    search_term <- 'ivi_data_regional-may-2010-onwards.xlsx')

  expect_true(
    RCurl::url.exists(url$url))

  expect_true(
    RCurl::url.exists(url$base_url))

  })
