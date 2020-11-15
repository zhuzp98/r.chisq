
# r.chisq

<!-- badges: start -->
<!-- badges: end -->

The goal of r.chisq is to calculate the reduced chi-squared of a fitted model. It is based on the R summary() function which doesn’t directly provide a reduced chi-square for the model fitted. 

Note: The reduced chi-square statistic is used to show the goodness of model fitting. 

## Installation

You can install the released version of r.chisq from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("r.chisq")
```

## Basic Usage

This is a basic example which shows you how to solve a common problem:

``` r
library(r.chisq)

data1 <- tibble::tibble(x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
                y = c(3.1, 5.1, 7.3, 9.3, 11.7, 13.4, 15.5, 17.8, 19.9, 21.8))
model1 <- lm(y ~ x, data = data1)
r.chi_cal(data1$y, model1)
```

