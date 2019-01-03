
[![Travis build
status](https://travis-ci.com/muschellij2/talkr.svg?branch=master)](https://travis-ci.com/muschellij2/talkr)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/muschellij2/talkr?branch=master&svg=true)](https://ci.appveyor.com/project/muschellij2/talkr)
[![Coverage
status](https://codecov.io/gh/muschellij2/talkr/branch/master/graph/badge.svg)](https://codecov.io/gh/muschellij2/talkr)
<!-- README.md is generated from README.Rmd. Please edit that file -->

# talkr Package:

The goal of `talkr` is to provide use natural language commands to talk
through data analysis using `dplyr`. Currently, natural language
processing is not used but a simple grammar.

## Installation

You can install `talkr` from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("muschellij2/talkr")
```

## Example

We will pass in a command to sort the

``` r
library(talkr)
library(tibble)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
df = tibble::rownames_to_column(mtcars, var = "car")
is.unsorted(df$mpg)
#> [1] TRUE
res = df %>%
  talk("Sort df by mpg")
#> Warning in talk_arrange(.data = structure(list(car = c("Mazda RX4", "Mazda
#> RX4 Wag", : Some words not allowed! Removed
#> [[1]]
#> [1] "df"  "mpg"
is.unsorted(res$mpg)
#> [1] FALSE
```

If you do not specify the order, it assumes ascending, as
`dplyr::arrange` does:

``` r
cmds = c(
  "Sort by mpg ascending and hp decreasing",
  "Sort by mpg and hp decreasing")
res = lapply(cmds, talk_arrange, .data = df)
all.equal(res[[1]], res[[2]])
#> [1] TRUE
```
