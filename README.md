
<!-- README.md is generated from README.Rmd. Please edit that file -->

# drilldowndemo

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/edgararuiz/drilldowndemo/branch/main/graph/badge.svg)](https://codecov.io/gh/edgararuiz/drilldowndemo?branch=main)
[![R-CMD-check](https://github.com/edgararuiz/drilldowndemo/workflows/R-CMD-check/badge.svg)](https://github.com/edgararuiz/drilldowndemo/actions)
<!-- badges: end -->

This package contains the `shiny` examples that show how to build apps
with ever more complex drill-down capabilities.

## Install

``` r
devtools::install_github("edgararuiz/drilldowndemo")
```

## Usage

Use `drilldowndemo_run()` to select and run one of the Shiny apps in the
package.

``` r
library(drilldowndemo)

drilldowndemo_run()
```

    Available apps:
     1-intro
     2-tabs
     3-navigation
     4-details
    Enter the app number to select: 

To see the code, use `drilldowndemo_open()`. It will open the script in
RStudio.
