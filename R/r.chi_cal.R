r.chi_cal <- function(y, model){
  if(!is.numeric(y)) {
    stop('I am so sorry, but this function only works for numeric response variable\n',
         'You have provided an object of class: ', class(y)[1])
  }

  if(class(model) != 'lm' & class(model) != 'nls') {
    stop('I am so sorry, but this function only works for linear or nonlinear least-squares models\n',
         'You have provided an object of class: ', class(model)[1])
  }

  se <- summary(model)$sigma
  chisq_val <- sum(((y - predict(model)) / se) ^ 2)
  df <- length(y) - summary(model)$df[1] + 1
  r.chi_val <- chisq_val / df
  r.chi_val
}
