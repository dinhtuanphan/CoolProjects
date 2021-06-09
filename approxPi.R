approxPi <- function(r, N) {
  totalPoints <- 0
  for (i in 1:N) {
    x <- runif(1, min = -r, max = r)
    y <- runif(1, min = -r, max = r)
    if (x ^ 2 + y ^ 2 < r ^ 2) {
      totalPoints <- totalPoints + 1
    }
  }
  ratio <- totalPoints / N
  pi <- ratio * 4
  return(pi)
}
