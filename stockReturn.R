returns <- rnorm(1:1000, mean = 7, sd = 15)
hist(returns, 20)

years <- 4
runs <- 10000

stock <- matrix(ncol = years, nrow = runs)

for (year in 1:years) {
  for (i in 1:runs) {
    stock[i, year] <- sample(returns, 1)
  }
}

avgReturn <- sapply(as.data.frame(stock), mean)
print(avgReturn)
