
<!-- README.md is generated from README.Rmd. Please edit that file -->

# scaler

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/giperbio/scaler/workflows/R-CMD-check/badge.svg)](https://github.com/giperbio/scaler/actions)
[![Codecov test
coverage](https://codecov.io/gh/giperbio/scaler/branch/main/graph/badge.svg)](https://codecov.io/gh/giperbio/scaler?branch=main)
[![License:
MIT](https://img.shields.io/badge/license-MIT-green)](https://choosealicense.com/licenses/mit/)
[![Contributor
Covenant](https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-ff69b4.svg)](https://giperbio.github.io/scaler/CODE_OF_CONDUCT.html)
<!-- badges: end -->

## Overview

`scaler` is an R package that provides a set of tools to deal with
health measurement scales. Its aim is to facilitate the work of
researchers with these kind of data and to improve reproducibility in
research.

Some scales require a dedicated package. If you’re looking for tools to
process the Munich ChronoType Questionnaire
([MCTQ](https://doi.org/10.1177/0748730402239679)), please check the
[`mctq`](https://github.com/ropensci/mctq) package.

## Prerequisites

You need to have some familiarity with the [R programming
language](https://www.r-project.org/) to use `scaler` main functions.

If you don’t feel comfortable with R, we strongly recommend checking
Hadley Wickham and Garrett Grolemund’s free and online book [R for Data
Science](https://r4ds.had.co.nz/) and the Coursera course from John
Hopkins University [Data Science: Foundations using
R](https://www.coursera.org/specializations/data-science-foundations-r)
(free for audit students).

## Installation

You can install `scaler` with:

``` r
# install.packages("remotes")
remotes::install_github("giperbio/scaler")
```

## Citation

If you use `scaler` in your research, please consider citing it. We put
a lot of work to build and maintain a free and open-source R package.
You can find the `scaler` citation below.

``` r
citation("scaler")
#> 
#> To cite {scaler} in publications use:
#> 
#>   Vartanian, D. (2023). {scaler}: tools to process health measurement
#>   scales. R package version 0.0.0.9000.
#>   https://giperbio.github.io/scaler/
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Unpublished{,
#>     title = {{scaler}: tools to process health measurement scales},
#>     author = {Daniel Vartanian},
#>     year = {2023},
#>     url = {https://giperbio.github.io/scaler/},
#>     note = {R package version 0.0.0.9000},
#>   }
```

## Contributing

We welcome contributions, including bug reports.

Take a moment to review our [Guidelines for
Contributing](https://giperbio.github.io/scaler/CONTRIBUTING.html).

<br>

Become an `scaler` supporter!

Click [here](https://github.com/sponsors/danielvartan) to make a
donation. Please indicate the `scaler` package in your donation message.
