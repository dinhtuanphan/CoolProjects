nDays = 200

price <- vector()
price[1] <- 20

stockPrice <- function() {
  gainRate <- rnorm(nDays, mean = 1.001, sd = 0.005)
  for (i in 2:nDays) {
    price[[i]] <- price[[i - 1]] * gainRate[i]
  }
  return(price[nDays])
}

minPrice <- min(replicate(1000, stockPrice()))
maxPrice <- max(replicate(1000, stockPrice()))
intervalPrice <-
  quantile(replicate(1000, stockPrice()), probs = c(0.05, 0.95))
iqrPrice <- IQR(replicate(1000, stockPrice()), na.rm = TRUE)


minPrice
maxPrice
intervalPrice
iqrPrice
