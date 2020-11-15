testthat::test_that("reduced chi-squared function match actual values, one parameter", {
  data1 <- tibble::tibble(x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                          y = c(3.1, 5.1, 7.3, 9.3, 11.7, 13.4, 15.5, 17.8, 19.9, 21.8))

  model1 <- lm(y ~ x, data = data1)

  r.chi_cal(data1$y, model1)

  se1 <- summary(model1)$sigma
  chisq_val1 <- sum(((data1$y - predict(model1)) / se1) ^ 2)
  df1 <- length(data1$y) - summary(model1)$df[1] + 1
  r.chi_sq1 <- chisq_val1 / df1

  testthat::expect_equal(r.chi_sq1, r.chi_cal(data1$y, model1))
})

testthat::test_that("reduced chi-squared function match actual values, two parameters", {
  data2 <- tibble::tibble(x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
                          y = c(3.1, 5.1, 7.3, 9.3, 11.7, 13.4, 15.5, 17.8, 19.9, 21.8, 23.9, 26.1, 28.7, 30.4, 32.6),
                          z = c(0.3, 0.5, 0.8, 0.2, 0.4, 0.1, 0.6, 0.3, 0.2, 0.8, 0.1, 0.3, 0.5, 0.2, 0.4))

  model2 <- lm(y ~ x + z, data = data2)

  r.chi_cal(data2$y, model2)

  se2 <- summary(model2)$sigma
  chisq_val2 <- sum(((data2$y - predict(model2)) / se2) ^ 2)
  df2 <- length(data2$y) - summary(model2)$df[1] + 1
  r.chi_sq2 <- chisq_val2 / df2

  testthat::expect_equal(r.chi_sq2, r.chi_cal(data2$y, model2))
})

testthat::test_that("reduced chi-squared function match actual values, nonlinear fitting", {
  library(tidyverse)

  # The data for second peak around wavelength of 589.6nm.
  peak2 <- read.csv("peak2.csv")

  # Y is the intensity value in A.U.
  y <- peak2$Y
  # X is the pixel count by CCD monitor
  x <- peak2$X

  # Define a Gaussian function (of four parameters).
  f <- function(x, theta)  {
    m <- theta[1]
    s <- theta[2]
    a <- theta[3]
    b <- theta[4]

    a * exp(- 0.5 * ((x - m) / s)^2) + b
  }

  # Estimate some starting values.
  m.0 <- x[which.max(y)]
  s.0 <- (max(x)-min(x))/4
  b.0 <- min(y)
  a.0 <- (max(y)-min(y))

  # Do the fit.
  p2m <- nls(y ~ f(x,c(m,s,a,b)), data.frame(x,y), start=list(m=m.0, s=s.0, a=a.0, b=b.0))

  # Record the predicted values (fitted values)
  peak2 <- peak2 %>% mutate(fit=predict(p2m))

  # Let's see the reduced chi-squared value for this nonlinear fitting
  r.chi_cal(peak2$Y, p2m)

  # actual calculation by steps
  se3 <- summary(p2m)$sigma
  chisq_val3 <- sum(((peak2$Y - predict(p2m)) / se3) ^ 2)
  df3 <- length(peak2$Y) - summary(p2m)$df[1] + 1
  r.chi_sq3 <- chisq_val3 / df3
  testthat::expect_equal(r.chi_sq3, r.chi_cal(peak2$Y, p2m))
})
