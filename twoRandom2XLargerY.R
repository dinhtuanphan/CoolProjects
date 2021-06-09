twoRandom <- function() {
  x <- rnorm(1, mean = 0, sd = 1)
  y <- rnorm(1, mean = 0, sd = 1)
  if (2 * x > y)
    return(TRUE)
  else
    return(FALSE)
}

runs <- 1000
sum(replicate(n = runs, twoRandom())) / runs
