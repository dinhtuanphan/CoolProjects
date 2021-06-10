prob <- function(p, q) {
  p <- p / 100
  q <- q / 100
  probTwoHeads <- (q ^ 2 + p ^ 2) / (q + p)
  return(probTwoHeads)
}
