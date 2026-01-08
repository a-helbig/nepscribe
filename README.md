
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NEPScribe

<!-- badges: start -->

<!-- badges: end -->

NEPScribe is a Shiny application for browsing metadata of NEPS SUF
datasets across all starting cohorts. It helps users identify and
explore variables and their associated metadata. The app also provides
functionality for comparing variables and metadata across cohorts.
Another main purpose of NEPScribe is to generate dynamic Stata or R
scripts that transform NEPS SUF datasets into a person-year format, a
structure often required for various data analysis projects. This can be
challenging for new NEPS users due to the structure of the data, which
combines lifecourse data in spell format with repeated cross-sectional
data.

## Installation

You can install NEPScribe from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes") - uncomment this row in case you havent installed package "remotes" yet
remotes::install_github("a-helbig/nepscribe")
```

## App Start

In order to start the app, use:

``` r
NEPScribe::run_app("nepscribe")
```
