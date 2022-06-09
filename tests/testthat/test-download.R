test_that("test download excel fails with error", {

  expect_error(download_excel('https://url.com/'),
               "Unknown http status code: 525")

})
