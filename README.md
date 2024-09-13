
<!-- README.md is generated from README.Rmd. Please edit that file -->

# survSAKK <a href="https://sakk-statistics.github.io/survSAKK/"><img src="man/figures/logo.png" align="right" height="138"/></a>

## Overview

The `survSAKK` R package provides the `surv.plot()` function,
facilitating Kaplan-Meier survival analysis. Designed with
user-friendliness and efficiency in mind. Offering a robust tool for
analysing survival data. It utilises the functionalites of
`survival::survfit()`.

## Installation

Install the current version of `survSAKK` with:

``` r
# install.packages("devtools")
devtools::install_github("SAKK-Statistics/survSAKK",
                         build_vignettes = TRUE,
                         force = TRUE)
```

## Usage

``` r
# Load required library
library(survSAKK)
library(survival)

# Fit survival object 
fit <- survfit(Surv(lung$time/365.25*12, status) ~ sex, data = lung)

# Generate surival plot
surv.plot(fit = fit, 
          time.unit = "month",
          legend.name =  c("Male", "Female"))
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

## Getting help

### Documentation

``` r
# R Documentation
## survSAKK: Create publication ready Kaplan-Meier plot
help("survSAKK-package")

## Publication Ready Kaplan-Meier Plot
help("surv.plot")
```

### Vignette

Webpage of the vignette:
[survSAKK](https://sakk-statistics.github.io/survSAKK/articles/surv.plot.html)

``` r
# Vignette
vignette("surv.plot", package = "survSAKK")
```

[![pages-build-deployment](https://github.com/SAKK-Statistics/survSAKK/actions/workflows/pages/pages-build-deployment/badge.svg?branch=main)](https://github.com/SAKK-Statistics/survSAKK/actions/workflows/pages/pages-build-deployment)
[![codecov](https://codecov.io/gh/SAKK-Statistics/survSAKK/branch/main/graph/badge.svg?token=7Q8ASW3OVM)](https://app.codecov.io/gh/SAKK-Statistics/survSAKK)[![CRAN
status](https://www.r-pkg.org/badges/version/survSAKK)](https://CRAN.R-project.org/package=survSAKK)
