x <- rnorm(100)
y <- rnorm(100)
ttest <- t.test(x, y)
ttest$statistic

ts <- replicate(1000, t.test(rnorm(10), rnorm(10))$statistic)
hist(ts)
range(ts)
qqnorm(ts)
qqline(ts)
abline(0,1)
probs <- c(0.9, 0.95, 0.99)
quantile(ts, probs)
qt(probs,df = 18)
qnorm(probs)


t.test(x, y, var.equal = FALSE)

tps <- replicate(1000, t.test(rnorm(10), rnorm(10))$p.value)
plot(density(tps))
tps
plot(tps)
hist(tps)
plot(density(tps))
lines(density(tps))
qqnorm(tps)
qqplot(tps, runif(1000))
