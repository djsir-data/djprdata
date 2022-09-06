# testing read_dese_childhood_services function

test_that("Read of DESE childhood services Data works", {

  keep_trying <- TRUE
  while(keep_trying){
    tryCatch({
      dese <- read_dese_childhood_services()
      keep_trying <- FALSE
    }, warning = function(e){
      print("second try")
      keep_trying <- FALSE             # fail safe
    })
  }

  expect_s3_class(dese, 'data.frame')
  expect_equal(class(dese$number_of_services), 'numeric')
  expect_equal(class(dese$sa3_code), 'character')

  expect_equal(colnames(dese), c("sa3_code",
                                 "number_of_services"))

})

test_that('read_dese_childhood_services URL exists', {

  expect_true(
    RCurl::url.exists(urls$read_dese_childhood_services, useragent="curl/7.39.0 Rcurl/1.95.4.5")
  )

})
