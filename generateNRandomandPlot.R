myPDF <- function(N, mu, sigma) {
  x <- rnorm(n = N, mean = mu, sd = sigma)
  y <-
    1 / (sigma * sqrt(2 * pi)) * exp(-(x - mu) ^ 2 / (2 * sigma ^ 2))
  plot(x, y)
}

myPDF(1000, 0, 1)
