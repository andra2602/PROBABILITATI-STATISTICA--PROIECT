sum_normal <- function(mean, sd, n, simulations = 1000) {
  replicate(simulations, sum(rnorm(n, mean = mean, sd = sd)))
}
