# testing read_internet_vacancy_report function

test_that("Read internet vacancy report works", {

  ivi <- read_internet_vacancy_report()

  expect_s3_class(iva, 'data.frame')
  expect_equal(class(ivi$value), 'numeric')
  expect_equal(class(ivi$observation_date), 'Date')

  expect_equal(colnames(ivi), c("level",
                                "state",
                                "region",
                                "anzsco_code",
                                "anzsco_title",
                                "date",
                                "value",
                                "obeservation_date"))

})

test_that('read_internet_vacancy_report URL exists', {

  expect_true(
    RCurl::url.exists(urls$read_internet_vacancy_report)
  )

})
