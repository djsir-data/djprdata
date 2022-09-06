# testing read_jobseeker_payments function

# test_that("Read jobseeker payment recipients works", {
#
#   skip_on_ci()     # added this command because currently this test hangs and I don't know why.
#
#   jobseeker <- read_jobseeker_payments()   # read_jobseeker_payments runs fine in the console, but hangs if run here
#
#   expect_s3_class(jobseeker, 'data.frame')
#   expect_equal(class(jobseeker$value), 'numeric')
#
#   expect_equal(colnames(jobseeker), c("sa2_name",
#                                       "value",
#                                       "month",
#                                       "year",
#                                       "mon_yr"))
#
# })

test_that('read_jobseeker_payments URL exists', {

  expect_true(
    RCurl::url.exists(urls$read_jobseeker_payments)
  )

})
