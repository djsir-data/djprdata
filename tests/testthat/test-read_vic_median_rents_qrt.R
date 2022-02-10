

test_that("Vic median rents file can be parsed", {

  rent <- read_vic_median_rents_qrt(test = TRUE)
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

  url <- djprdata:::get_url_vic_median_rents_qrt()

  expect_false(httr::http_error(url$url))
  expect_false(httr::http_error(url$base_url))

})



