
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Diviana

Diviana is a Shiny app for complex *Disaster victim identification*. In
particular it handles references families with multiple missing persons.

## Installation

Diviana is currently under development and not yet available on CRAN.

To install the latest version from GitHub, use:

``` r
# install.packages("remotes")
paks = paste0("magnusdv/", c("pedtools", "ribd", "forrel", "dvir", "verbalisr", "diviana"))
remotes::install_github(paks, dependencies = TRUE)
```

Once the installations are complete, you can launch the app with:

``` r
diviana::launchApp()
```
