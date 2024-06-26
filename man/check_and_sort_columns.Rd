% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_qtrap.R
\name{check_and_sort_columns}
\alias{check_and_sort_columns}
\title{Check and Sort Columns, Compare Values}
\usage{
check_and_sort_columns(area_data, area_txt)
}
\arguments{
\item{area_data}{A tibble containing the data to be checked and sorted.}

\item{area_txt}{A tibble containing the reference data for column order and value comparison.}
}
\value{
Prints messages indicating whether the tibbles have the same column names and whether all values are the same.
}
\description{
This function checks if two tibbles have the same column names, sorts the columns of one tibble to match the order of the other,
and then checks if all values in both tibbles are the same.
}
\examples{
\dontrun{
area_data <- read.delim("path/to/All_txt.txt", check.names = FALSE)
area_txt <- read.delim("path/to/area.txt", check.names = FALSE)
check_and_sort_columns(area_data, area_txt)
}
}
\author{
Yaoxiang Li
}
