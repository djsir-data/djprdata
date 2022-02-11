





read_vic_property_prices <- function(){

  url <- 'https://www.land.vic.gov.au/valuations/resources-and-reports/property-sales-statistics'
  url_house <- 'SUBURB_HOUSE'

  get_latest_download_url(url, url_house)

  # website includes both annual data and recent quarterly data
}
