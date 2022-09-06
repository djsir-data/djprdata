# testing read_abs_payroll function

test_that("ABS Weekly Payroll data works", {

  skip_on_ci()      # this function takes a long time to run, so better to skip it on ci

  payroll <- read_abs_payroll()

  expect_s3_class(payroll, 'data.frame')
  expect_equal(class(payroll$value), 'numeric')
  expect_equal(class(payroll$date), 'Date')

  expect_equal(colnames(payroll), c("state_code",
                                  "state_or_territory",
                                  "sa4_code",
                                  "statistical_area_level_4",
                                  "sa3_code",
                                  "statistical_area_level_3",
                                  "date",
                                  "value"))

})

test_that('read_abs_payroll URL exists', {

  expect_true(
    RCurl::url.exists(urls$read_abs_payroll, useragent="curl/7.39.0 Rcurl/1.95.4.5")
  )

})
