# testing read_vic_schools_fte function
# there is no URL, urls$read_vic_schools_fte is NULL
# function not finished yet, will wait for that and then write more tests

test_that("read_vic_schools_fte works", {

  skip_on_ci()

  fte <- read_vic_schools_fte()

  expect_s3_class(fte, 'data.frame')
# more tests to check class of variables, will have to wait for final product
#  expect_equal(class(fte$value), 'numeric')
#  expect_equal(class(fte$date), 'Date')















})
