# testing read_salm function

test_that("Read of DSS payment Data works", {

  salm <- read_salm()

  expect_s3_class(salm, 'data.frame')
  expect_equal(class(salm$value), 'numeric')
  expect_equal(class(salm$date), 'Date')

  expect_equal(colnames(salm), c("sa2_name",
                                 "region",
                                 "date",
                                 "value"))

})

test_that('read_small_area_labour_markets URL exists', {

  expect_true(
    RCurl::url.exists(urls$read_salm, useragent="curl/7.39.0 Rcurl/1.95.4.5")
  )

})
