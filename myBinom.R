myBinom <- function(x, n, p) {
  factorial(n)/(factorial(x) * factorial(n-x)) * p^x * (1-p)^(n -x)
}

n = 6
p = 0.5
x <- rbinom(100, 6, p)
y <- dbinom(x, n, p)
#y <- myBinom(x , n, p)
par(mfrow = c(1,3))
hist(x)
hist(y)
plot(x , y)
