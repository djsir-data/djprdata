test_that("test download fails with error", {

  expect_error(download_excel('https://url.com'), "Failed to retrieve a Spreadsheet")

})
