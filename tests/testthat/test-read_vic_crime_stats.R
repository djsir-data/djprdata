

#tests
test_that("Vic crime statistics file can be parsed", {

  dblist <- c("offence_by_location_by_lga", "offence_by_postcode_suburb", "offence_rate_by_lga")

  for (i in 1:length(dblist)){

    crime <- read_vic_crime_stats(dataset = dblist[i], test = TRUE)
    columns <- colnames(crime)


    expect_s3_class(crime, 'data.frame')
    expect_equal(class(crime$value), 'numeric')
    expect_equal(class(crime$observation_date), 'Date')

    if (dblist[i] == "offence_by_location_by_lga"){

    expect_equal(columns, c("observation_date",
                            "local_government_area",
                            "location_subdivision",
                            "dim_description",
                            "dim_code",
                            "value"))

    } else if (dblist[i] == "offence_by_postcode_suburb") {

      expect_equal(columns, c("observation_date",
                              "local_government_area",
                              "suburb",
                              "postcode",
                              "offence_div",
                              "offence_div_code",
                              "value"))

    } else if (dblist[i] == "offence_rate_by_lga"){

      expect_equal(columns, c("observation_date",
                              "local_government_area",
                              "offence_div",
                              "offence_div_code",
                              "value"))
    }
  }
})

test_that('Vic crime stats URL has not changed', {

  url <- 'https://www.crimestatistics.vic.gov.au/crime-statistics/latest-victorian-crime-data/download-data'
  search_term <- 'Data_Tables_LGA_Recorded_Offences'

  url <- get_latest_download_url(url, search_term)

  expect_false(httr::http_error(url$url))
  expect_false(httr::http_error(url$base_url))

})
