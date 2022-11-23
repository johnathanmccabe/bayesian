# Frequency Bayesian


library(tidyverse)


number_of_trial = c(0,1, 5, 10, 20)
actual = rpois(n = max(number_of_trial), lambda = 62)
expected = 75




mean = expected
cv = 0.1

results <- purrr::map(number_of_trial, function(n){

  
  a = ifelse(n > 0, sum(actual[1:n]), 0)
  
  # initital prior
  alpha = 1/(cv*cv)
  beta =  alpha / mean
  
  # posterior
  alpha = alpha + a
  beta = beta + n
  
  r = alpha
  p = beta / (1 + beta)
  
  x = seq(0,max(actual) * 2, 1)
  posterior = dnbinom(x = x,
                      size = r,
                      prob = p)
  
  mean = r * (1-p) / p
  
  
  print(glue::glue("n:{n}; Mean: {mean}; CV: {1/(p)}"))
  
  posterior_plot <- ggplot2::ggplot(data = data.frame(x = x, y = posterior)) +
    ggplot2::geom_area(ggplot2::aes(x,y), fill = "light blue") +
    ggplot2::geom_line(ggplot2::aes(x,y), color = "blue") +
    geom_vline(xintercept = mean, linetype = "dashed") +
    ggplot2::theme_minimal() +
    ggplot2::ggtitle(label = glue::glue("N:{n}; A:{round(ifelse(n > 0, a/n, 0), 1)}; E:{round(mean, 1)}")) +
    scale_y_continuous(labels = NULL,
                       name = "Density")
  
})
  



cowplot::plot_grid(plotlist = results)


ppois(3,7.8)


