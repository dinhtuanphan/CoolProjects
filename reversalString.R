set.seed(007)
DNA <-
  paste(sample(c("A", "T", "G", "X"), 100, replace = TRUE),
        collapse = "")
splittedDNA <- strsplit(DNA, "")[[1]]
reversedDNA <- paste(rev(splittedDNA), collapse = "")
print(reversedDNA)
