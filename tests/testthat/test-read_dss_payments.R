# testing read_dss_payments function

test_that("Read of DSS payment Data works", {

  skip_on_ci()      # this function takes a while to run, so better to skip it on ci

  dss <- read_dss_payments()

  expect_s3_class(dss, 'data.frame')
  expect_equal(class(dss$payments), 'numeric')
  expect_equal(class(dss$quarter_end_date), 'Date')

  expect_equal(colnames(dss), c("lga",
                                "lga_name",
                                "quarter_end_date",
                                "payment_type",
                                "payments"))

})

test_that('read_dss_payments URLs exist', {

  expect_true(
    RCurl::url.exists(urls$read_dss_payments)
  )

})
