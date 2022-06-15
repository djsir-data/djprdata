% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_internet_vacancy_report.R
\name{read_internet_vacancy}
\alias{read_internet_vacancy}
\title{National Skills Commission's Internet Vacancy Report}
\usage{
read_internet_vacancy(url = urls$read_internet_vacancy_report)
}
\arguments{
\item{url}{Base url which contains links to all data, default value is:
\link[https://labourmarketinsights.gov.au/our-research/internet-vacancy-index/]}
}
\value{
data.frame
}
\description{
Extracting and cleaning monthly online job advertisements
for Melbourne from NSC's Internet Vacancy Report. Data is filtered by 2-digit ANZSCO
}
\examples{
df <- read_read_internet_vacancy()
}