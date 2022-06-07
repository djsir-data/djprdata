

test_that("read_google_mobility works", {

  gm <- read_google_mobility()

  expect_s3_class(gm, 'data.frame')

  expect_equal(colnames(gm), c("sub_region_2",
                               "date",
                               "name",
                               "value"))
  expect_equal(unique(gm$name), c("Workplaces",
                                  "Retail and recreation",
                                  "Grocery and pharmacy",
                                  "Parks",
                                  "Residential","Transit stations"))

})
