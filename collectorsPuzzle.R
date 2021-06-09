sampleList <- c(seq(1:14))

pickASample <- function() {
  sample(sampleList, size = 1, replace = TRUE)
}

ownedSample <- c()

numBuy <- function() {
  while (length(unique(ownedSample)) < 14) {
    ownedSample <- c(ownedSample, pickASample())
  }
  length(ownedSample)
}

trials <- 1000
avgBuy <- mean(replicate(trials, numBuy()))
print(avgBuy)
