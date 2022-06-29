# testing read_abs_cabee function

test_that("ABS Counts of Businesses including Entries and Exits works", {

#  skip_on_ci()   # skip this test of it takes a lot of time, depending on the platform

  cabee <- read_abs_cabee()

  expect_s3_class(cabee, 'data.frame')
  expect_equal(class(cabee$value), 'numeric')
  expect_equal(class(cabee$date), 'Date')
  expect_equal(class(cabee$release), 'Date')

  expect_equal(colnames(cabee), c("state_name",
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

test_that('read_abs_cabee URL exists', {

  expect_true(
    RCurl::url.exists(urls$read_abs_cabee)
  )

})
