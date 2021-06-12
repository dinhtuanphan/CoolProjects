myPDF <- function(x, mu, sigma) {
  return(1 / (sigma * sqrt(2 * pi)) * exp(-(x - mu) ^ 2 / (2 * sigma ^ 2)))
}

mu = 0
sigma = 1
x <- rnorm(n = 1000, mean = mu, sd = sigma)
y <- myPDF(x, mu = 0, sigma = 1)
hist(x)
plot(x , y)


x <- runif(n = 1000, min = -3, max = 3)
qqnorm(x)
qqline(x)
