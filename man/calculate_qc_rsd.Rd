% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_qc_rsd.R
\name{calculate_qc_rsd}
\alias{calculate_qc_rsd}
\title{Calculate QC Statistics and RSD}
\usage{
calculate_qc_rsd(
  data,
  qc_string = "Pooled QC|Pooled_QC",
  nist_string = "NIST",
  id_col = "sample_id",
  ignore_na = TRUE
)
}
\arguments{
\item{data}{A tibble with the first column as sample IDs and other columns as features.}

\item{qc_string}{A string pattern to filter Pooled QC samples. Default is "Pooled QC|Pooled_QC".}

\item{nist_string}{A string pattern to filter NIST samples. Default is "NIST".}

\item{id_col}{The name of the sample ID column. Default is "sample_id".}

\item{ignore_na}{Logical, whether to ignore NA values in calculations. Default is TRUE.}
}
\value{
A list containing tibbles with calculated statistics for Pooled QC and NIST samples. If no matching samples are found, returns NULL for that category.
}
\description{
This function calculates the mean, standard deviation, and coefficient of variation
for QC data, optionally ignoring NA values. It also calculates the relative standard deviation (RSD)
for both Pooled QC and NIST samples. The input data should be a tibble where
the first column is the sample ID and all other columns are features.
}
\examples{
# Generate example data
example_data <- tibble::tibble(
  sample_id = c("Pooled QC 1", "Pooled QC 2", "NIST 1", "NIST 2", "Sample 1"),
  feature1 = c(10, 15, 20, NA, 5),
  feature2 = c(20, NA, 25, 30, 10),
  feature3 = c(NA, 25, 30, 35, 15)
)

# Calculate QC statistics and RSD
rsd_stats <- calculate_qc_rsd(example_data)

# Access results
pooled_qc_rsd <- rsd_stats$Pooled_QC_RSD
nist_rsd <- rsd_stats$NIST_RSD
}
\author{
Yaoxiang Li
}
