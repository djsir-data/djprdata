# testing read_aihw_mental_health function (contains four functions)

test_that("Read of Australian Institute of Health and Welfare - care episodes data works", {

  care_episodes <- read_aihw_care_episodes()

  expect_s3_class(care_episodes, 'data.frame')
  expect_equal(class(care_episodes$value), 'numeric')
  expect_equal(class(care_episodes$observation_date), 'Date')

  expect_equal(colnames(care_episodes), c("sa3_code",
                                          "observation_date",
                                          "value"))
})

test_that("Read of Australian Institute of Health and Welfare - emergency presentation data works", {

  emergency_presentations <- read_aihw_emergency_presentations()

  expect_s3_class(emergency_presentations, 'data.frame')
  expect_equal(class(emergency_presentations$value), 'numeric')
  expect_equal(class(emergency_presentations$observation_date), 'Date')

  expect_equal(colnames(emergency_presentations), c("sa3_code",
                                                    "observation_date",
                                                    "value"))
})

test_that("Read of Australian Institute of Health and Welfare - community care data works", {

  community_care <- read_aihw_community_care()

  expect_s3_class(community_care, 'data.frame')
  expect_equal(class(community_care$value), 'numeric')
  expect_equal(class(community_care$observation_date), 'Date')

  expect_equal(colnames(community_care), c("sa3_code",
                                                    "observation_date",
                                                    "value"))
})

test_that("Read of Australian Institute of Health and Welfare - prescriptions data works", {

  prescriptions <- read_aihw_prescriptions()

  expect_s3_class(prescriptions, 'data.frame')
  expect_equal(class(prescriptions$value), 'numeric')
  expect_equal(class(prescriptions$observation_date), 'Date')   # there is a bug that needs fixing in the function

  expect_equal(colnames(prescriptions), c("sa3_code",
                                           "value",
                                           "observation_date"))
})

test_that('read_aihw_mental_health care episodes URL exists', {

  expect_true(
    RCurl::url.exists(urls$read_aihw_care_episodes, useragent="curl/7.39.0 Rcurl/1.95.4.5")
  )
})

test_that('read_aihw_mental_health community care URL exists', {

    expect_true(
    RCurl::url.exists(urls$read_aihw_community_care, useragent="curl/7.39.0 Rcurl/1.95.4.5")
  )
})

test_that('read_aihw_mental_health emergency presentations URL exists', {

  expect_true(
    RCurl::url.exists(urls$read_aihw_emergency_presentations, useragent="curl/7.39.0 Rcurl/1.95.4.5")
  )
})

test_that('read_aihw_mental_health prescriptions URL exists', {

  expect_true(
    RCurl::url.exists(urls$read_aihw_prescriptions, useragent="curl/7.39.0 Rcurl/1.95.4.5")
  )
})
