# testing read_abs_cabee function

# no need to test for URL etc., because we are using readabs package to download data,which has its own testing

test_that("ABS Counts of Businesses including Entries and Exits works", {

  skip_on_ci()

  cabee <- read_abs_cabee()

  expect_s3_class(cabee, 'data.frame')
  expect_equal(class(cabee$value), 'numeric')   # this is chr, but shouldn't it be numeric?
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
