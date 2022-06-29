# testing read_internet_vacancy function

test_that("Read internet vacancy works", {

  ivi <- read_internet_vacancy()

  expect_s3_class(ivi, 'data.frame')
  expect_equal(class(ivi$value), 'numeric')
  expect_equal(class(ivi$observation_date), 'Date')

  expect_equal(colnames(ivi), c("level",
                                "state",
                                "region",
                                "anzsco_code",
                                "anzsco_title",
                                "date",
                                "value",
                                "observation_date"))

})

test_that('read_internet_vacancy URL exists', {

  expect_true(
    RCurl::url.exists(urls$read_internet_vacancy_report)
  )

})
