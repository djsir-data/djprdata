# testing read_aihw_mental_health function (contains four functions)

test_that("Read of Australian Institute of Health and Welfare - Mental Health Data works", {

  care_episodes <- read_aihw_care_episodes()

  expect_s3_class(care_episodes, 'data.frame')
  expect_equal(class(care_episodes$value), 'numeric')
  expect_equal(class(care_episodes$observation_date), 'character')       # probably should be 'Date'?

  expect_equal(colnames(care_episodes), c("sa3_code",
                                          "observation_date",
                                          "value"))

  emergency_presentations <- read_aihw_emergency_presentations()

  expect_s3_class(emergency_presentations, 'data.frame')
  expect_equal(class(emergency_presentations$value), 'numeric')
  expect_equal(class(emergency_presentations$observation_date), 'character')       # probably should be 'Date'?

  expect_equal(colnames(emergency_presentations), c("sa3_code",
                                                    "observation_date",
                                                    "value"))

  community_care <- read_aihw_community_care()

  expect_s3_class(community_care, 'data.frame')
  expect_equal(class(community_care$value), 'numeric')
  expect_equal(class(community_care$observation_date), 'character')       # probably should be 'Date'?

  expect_equal(colnames(community_care), c("sa3_code",
                                                    "observation_date",
                                                    "value"))

  prescriptions <- read_aihw_prescriptions()

  expect_s3_class(prescriptions, 'data.frame')
  expect_equal(class(prescriptions$value), 'numeric')
  expect_equal(class(prescriptions$observation_date), 'character')       # probably should be 'Date'?

  expect_equal(colnames(prescriptions), c("sa3_code",
                                           "value",
                                           "observation_date"))

})

test_that('read_aihw_mental_health URLs exists', {

  expect_true(
    RCurl::url.exists(urls$read_aihw_care_episodes)
  )

  expect_true(
    RCurl::url.exists(urls$read_aihw_community_care)
  )

  expect_true(
    RCurl::url.exists(urls$read_aihw_emergency_presentations)
  )

  expect_true(
    RCurl::url.exists(urls$read_aihw_prescriptions)
  )



})
