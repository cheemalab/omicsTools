# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Fill the DESCRIPTION ----
## Add meta data about your application
##
## /!\ Note: if you want to change the name of your app during development,
## either re-run this function, call golem::set_golem_name(), or don't forget
## to change the name in the app_sys() function in app_config.R /!\
##
golem::fill_desc(
  pkg_name = "omicsTools", # The Name of the package containing the App
  pkg_title = "Omics Data Process Toolbox", # The Title of the package containing the App
  pkg_description = "Processing and analyzing omics data from genomics, transcriptomics, proteomics, and metabolomics platforms. It provides functions for preprocessing, normalization, visualization, and statistical analysis, as well as machine learning algorithms for predictive modeling. 'omicsTools' is an essential tool for researchers working with high-throughput omics data in fields such as biology, bioinformatics, and medicine.The QC-RLSC (quality control–based robust LOESS signal correction) algorithm is used for normalization. Dunn et al. (2011) <doi:10.1038/nprot.2011.335>.", # The Description of the package containing the App
  author_first_name = "Yaoxiang", # Your First Name
  author_last_name = "Li", # Your Last Name
  author_email = "liyaoxiang@outlook.com", # Your Email
  author_orcid = "0000-0001-9200-1016",
  pkg_version = "1.1.4",
  repo_url = "https://github.com/YaoxiangLi/omicsTools" # The URL of the GitHub Repo (optional)
)

p <- c(person(given = "Yaoxiang",
              family = "Li",
              role = c("cre", "aut"),
              comment = c(ORCID = "0000-0001-9200-1016"),
              email = "liyaoxiang@outlook.com"),
       person(given = "Meth",
              family = "Jayatilake",
              role = c("aut"),
              comment = c(ORCID = "0000-0002-5780-9391"),
              email = "mmj61@georgetown.edu"),
       person(given = "Zihao",
              family = "Zhang",
              role = c("aut"),
              email = "zz351@georgetown.edu"),
       person(given = "Amrita",
              family = "Cheema",
              role = c("aut"),
              comment = c(ORCID = "0000-0003-4877-7583"),
              email = "akc27@georgetown.edu"))
desc::desc_set_authors(p)


## Set {golem} options ----
golem::set_golem_options()

## Create Common Files ----
## See ?usethis for more information
# usethis::use_mit_license("Yaoxiang Li") # You can set another license here
usethis::use_agpl_license(version = 3, include_future = TRUE)
usethis::use_readme_rmd(open = FALSE)
# Note that `contact` is required since usethis version 2.1.5
# If your {usethis} version is older, you can remove that param
usethis::use_code_of_conduct(contact = "Yaoxiang Li")
usethis::use_lifecycle_badge("Experimental")
usethis::use_news_md(open = FALSE)

## Adding dependencies
usethis::use_package("dplyr")
usethis::use_package("DT")
usethis::use_package("magrittr")
usethis::use_package("readr")
usethis::use_package("tibble")
usethis::use_package("bs4Dash")
usethis::use_package("shinymanager")
usethis::use_package("matrixStats")
usethis::use_package("tidyr")
usethis::use_package("pcva")
usethis::use_package("ggplot2")

usethis::use_package("spelling", type = "Suggests")
usethis::use_pipe(export = TRUE)


## dependencies for utils_features
usethis::use_package("moments")
usethis::use_package("outliers")
usethis::use_package("MASS")
usethis::use_package("dplyr")
usethis::use_package("dbscan")
usethis::use_package("ggplot2")
usethis::use_package("UpSetR")
usethis::use_package("tidyr")
usethis::use_package("cli")
usethis::use_package("progress") # consider remove this


## Use git ----
usethis::use_git()

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Favicon ----
# If you want to change the favicon (default is golem's one)
golem::use_favicon() # path = "path/to/ico". Can be an online file.
# golem::remove_favicon() # Uncomment to remove the default favicon

## Add helper functions ----
golem::use_utils_ui(with_test = TRUE)
golem::use_utils_server(with_test = TRUE)

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile("dev/02_dev.R")
