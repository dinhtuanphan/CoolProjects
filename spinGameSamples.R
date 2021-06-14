runs <- 100000

spin <- function() {
  expectation <-
    sum(sample(c(1/2, -1/4, 1/2), size = 10, replace = TRUE))
  if (expectation < 0) {
    return(1)
  } else {
    return(0)
  }
}

prob <- sum(replicate(runs, spin())) / runs
print(prob)
