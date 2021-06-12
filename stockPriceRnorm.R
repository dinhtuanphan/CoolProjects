nDays = 200
gainRate <- rnorm(nDays, mean = 1.001, sd = 0.005)

price <- vector()
price[1] <- 20

for (i in 2:nDays) {
  price[[i]] <- price[[i - 1]] * gainRate[i]
}

plot(price)
