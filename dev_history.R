

library(devtools)
library(usethis)
library(pkgdown)

# create package via new project option in Rstudio then open project
# use project options to choose roxygen2
devtools::document()

# save this script as dev_history.R
usethis::use_build_ignore("dev_history.R") # ensure this file is ignored

use_mit_license()

use_readme_rmd()
use_testthat()
use_pkgdown()
use_data_raw()
#use_pipe()
use_version("patch")
use_news_md()
use_vignette('djprdata') # user guide

# setup git branch rename master as main
shell("git add .")
shell('git commit -m "base package structure completed"')
shell('git branch -m master main')



# Add code coverage
use_coverage(type = c("codecov"))
