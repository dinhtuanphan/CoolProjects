floorSquare <- function(x) {
  if (x == 0 | x == 1) {
    # take care base cases
    return(x)
  } else {
    total = 1
    i = 1
    while (total <= x) {
      i = i + 1
      total <- i ^ 2
    }
    return(i - 1)
  }
}

x = 11
print(floorSquare(x))