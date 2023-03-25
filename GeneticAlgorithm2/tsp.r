library("gramEvol")

distances <- read.csv2(file = "distances.csv", row.names = 1)

evalFunc <- function(solution) {
  # solution <- 1:48
  if (length(solution) > length(unique(solution))) {
    return(1e10)
  }
  
  # get the total distance
  total <- 0
  for (i in 2:length(solution)) {
    # get the distance between city1 and city2, city2 and city3 (from solution[i-1] to solution[i])
    total <- total + distances[solution[i-1], solution[i]]
  }
  return(total)
}


monitorFunc <- function(result) {
  cat("Best of gen: ", min(result$best$cost), "-", result$best$genome, "\n")
}

# first try with allowrepeat = TRUE
x <- GeneticAlg.int(genomeLen = 48, codonMin = 1, codonMax = 48,
                    allowrepeat = FALSE, terminationCost = 10000,
                    monitorFunc = monitorFunc, evalFunc = evalFunc,
                    iterations = 10, popSize = 100)
x <- GeneticAlg.int(genomeLen = 48, codonMin = 1, codonMax = 48,
                    allowrepeat = FALSE, terminationCost = 10000,
                    monitorFunc = NULL, evalFunc = evalFunc,
                    iterations = 10, popSize = 1000, mutationChance = 0.5)
print(x)

solution <- x$best$genome
# Best solution
# solution <- c(1, 8, 38, 31, 44, 18, 7, 28, 6, 37, 19, 27, 17, 43, 30, 36, 46, 33, 20, 47, 21, 32, 39, 48, 5, 42, 24, 10, 45, 35, 4, 26, 2, 29, 34, 41, 16, 22, 3, 23, 14, 25, 13, 11, 12, 15, 40, 9)
total <- 0
for (i in 2:length(solution)) {
  # get the distance between city1 and city2, city2 and city3 (from solution[i-1] to solution[i])
  total <- total + distances[solution[i-1], solution[i]]
}
print(total)

print(x$population$best)
plot(x$population$best)
