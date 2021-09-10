samples <- rbinom(n = 1000 * 50, size = 10, prob = 0.3)

matrixData <- matrix(samples, nrow = 50)

colSum <- apply(matrixData, 2, sum)

resMatrix <- matrixData/colSum  # wrong!
colSums(resMatrix)


# correct ways to do (For matrices, there is no such thing as division)

# method 1
resMatrix2 <- sweep(matrixData, 2, colSum, '/')
colSums(resMatrix2)

# method 2
resMatrix3 <- t(t(matrixData)/colSum)
colSums(resMatrix3)

# method 3
resMatrix4 <- prop.table(matrixData, 2)
colSums(resMatrix4)
