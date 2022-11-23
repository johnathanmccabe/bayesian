#' Bayesian coin flipping



library(ggplot2)




number_of_trials = c(0, 2, 10, 20, 50, 500,5000)

prob = runif(1)
data = purrr::rbernoulli(p = prob, n = tail(number_of_trials, 1))

x = seq(from = 0,to = 1, length.out = 101)


results <- purrr::map(number_of_trials, function(n){
  
  ## number of heads
  heads = ifelse(n > 0, sum(data[1:n]), 0)
  
  # posterior distribution  
  y = dbeta(x = x,
            shape1 = 1 + heads,
            shape2 = 1 + n - heads)
  
  ## plot output
  plot <- ggplot2::ggplot(data = data.frame(x = x, y = y)) +
    ggplot2::geom_area(ggplot2::aes(x,y), fill = "light blue") +
    ggplot2::geom_line(ggplot2::aes(x,y), color = "blue") +
    ggplot2::theme_minimal() +
    ggplot2::ggtitle(label = glue::glue("{n} trials, {heads} heads")) +
    scale_y_continuous(labels = NULL,
                       name = "Density",
                       limits = c(0, max(2, max(y))))
  
  
  return(plot)
})


print(prob)
cowplot::plot_grid(plotlist = results)
