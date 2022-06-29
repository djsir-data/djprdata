# testing read_abs_payroll function

test_that("ABS Weekly Payroll data works", {

  payroll <- read_abs_payroll()

  expect_s3_class(payroll, 'data.frame')
  expect_equal(class(payroll$value), 'numeric')
  expect_equal(class(payroll$date), 'Date')

  expect_equal(colnames(payroll), c("state_name",
                                  "lga_code",
                                  "lga_label",
                                  "industry_code",
                                  "industry_label",
                                  "category",
                                  "value",
                                  "series",
                                  "date",
                                  "release"))

})

test_that('read_abs_payroll URL exists', {

  expect_true(
    RCurl::url.exists(urls$read_abs_payroll)
  )

})
