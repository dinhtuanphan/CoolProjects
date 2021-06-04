pbinom(3,10,0.5, lower.tail = FALSE)
sample(x = c(0,1), size = 1000, replace = TRUE)
sum(.Last.value == 1)

run <- 10000
one.trail <- function(){
  sum(sample(c(0,1), size = 10, replace = TRUE)) > 3
}

for (i in 1:run){
  totalThree <- 0
  if (one.trail() == TRUE){
    totalThree <- totalThree + 1
  }
}

totalThree
totalThree/run


mc.binom <-sum(replicate(run, one.trail()))/run
mc.binom


set.seed(2820)

sweetOne <- c(rnorm(100, mean = 14, sd = 0.3))
sweetTwo <- c(rnorm(100, mean = 13, sd = 0.2))

t.test(sweetOne, sweetTwo, paired = TRUE)


set.seed(0)

shopOne <- rnorm(50, mean = 140, sd = 4.5)
shopTwo <- rnorm(50, mean = 150, sd = 4)

t.test(shopOne, shopTwo, var.equal = TRUE)


x <- rnorm(100)
y <- rnorm(100)

t.test(x,y)



