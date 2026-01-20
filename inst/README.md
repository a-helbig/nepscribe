
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NEPScribe

<!-- badges: start -->

<!-- badges: end -->

NEPScribe is a Shiny application that generates dynamic Stata and R
scripts to transform NEPS Scientific Use File (SUF) datasets into a
person-year format — a data structure commonly required for many
analytical tasks. This transformation and the underlying data
preparation can be challenging for new NEPS users because NEPS data
combine lifecourse information stored in spell format with panel data in
a wave format.

Furthermore, NEPScribe enables systematic exploration of Scientific Use
Files (SUFs) — including variables and accompanying metadata — by
utilizing the publicly available semantic SUF files supplied by the
NEPS.

## Installation

You can install NEPScribe from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes") - uncomment this row in case you havent installed package "remotes" yet
remotes::install_github("a-helbig/nepscribe")
```

## App Start

In order to start the app, use:

``` r
NEPScribe::run_app()
```
