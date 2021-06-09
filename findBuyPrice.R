priceList <- c(20, 7, 8, 9, 15, 15)
findBuyPrice <- function(priceList) {
  tempDiff <- 0
  
  for (i in 1:length(priceList)) {
    for (j in i:length(priceList)) {
      diffPrice <- priceList[j] - priceList[i]
      if (diffPrice > tempDiff) {
        tempDiff <- diffPrice
        buyPrice <- priceList[i]
      }
    }
  }
  print(buyPrice)
}
