
<!-- README.md is generated from README.Rmd. Please edit that file -->

# omicsTools

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

The goal of omicsTools is to provide tools for processing and analyzing
omics data from genomics, transcriptomics, proteomics, and metabolomics
platforms. It provides functions for preprocessing, normalization,
visualization, and statistical analysis, as well as machine learning
algorithms for predictive modeling. omicsTools is an essential tool for
researchers working with high-throughput omics data in fields such as
biology, bioinformatics, and medicine.

License: MIT + file LICENSE

## Installation

You can install the development version of omicsTools like so:

``` r
devtools::install_github("https://github.com/YaoxiangLi/omicsTools.git")
```

## Example

This is a basic example which allows you to start using it:

``` r
library(omicsTools)
#> 
#> This is omicsTools version 1.0.4.
#> omicsTools is free software and comes with ABSOLUTELY NO WARRANTY.
#> Please use at your own risk.
omicsTools::run_app()
```

<div style="width: 100% ; height: 400px ; text-align: center; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box;" class="muted well">Shiny applications not supported in static R Markdown documents</div>

To keep `README.md` up-to-date. `devtools::build_readme()`
