
<!-- README.md is generated from README.Rmd. Please edit that file -->

# survSAKK <a href="https://sakk-statistics.github.io/survSAKK"><img src="man/figures/logo.png" align="right" height="138"/></a>

## Overview

The goal of `survSAKK` is to create an open source, user-friendly and
flexible `R` package, which will incorporate various statistics and
layout customization options to enhance the efficiency and adaptability
of the Kaplan-Meier plot.

## Installation

You can install the current version of `survSAKK` from [GitHub
Pages](file:///C:/Users/vithersans/Desktop/survSAKK/docs/index.html):

``` r
# install.packages("devtools")

# At the moment, while repository is private:
devtools::install_github("SAKK-Statistics/survSAKK",
                         auth_token = "ask_for_the_password",
                         build_vignettes = TRUE,
                         force = TRUE)

# Later, when the repository is public:
devtools::install_github("SAKK-Statistics/survSAKK",
                         build_vignettes = TRUE)
```

## Usage

``` r
library(survSAKK)

surv.plot(fit)
```

## Getting help

The documentation of the function.

### Documentation

- `?survSAKK`

### Vigniette

- `browseVignettes("survSAKK")`

- You can read a specific vignette with: `vignette("surv.plot", package = "survSAKK")`  

- [Vignette
  Page](https://sakk-statistics.github.io/survSAKK/articles/surv.plot.html)
