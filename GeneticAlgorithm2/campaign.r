library("gramEvol")

maximum_campaign_cost <- 150000
cost_few <- c(5600, 4600, 5100, 2400, 2200, 3400, 1300, 2500)
cost_many <- c(3400, 2400, 2400, 1400, 1200, 3400, 1300, 2500)
audience <- c(200000, 130000, 145000, 75000, 55000, 210000, 150000, 20000)

evalFunc <- function(X) {
  # X <- 1:8
  total_campaign_cost <- 0
  for (i in 1:length(X)) {
    if (X[i] <= 3) {
      # cost_few
      total_campaign_cost <- total_campaign_cost + X[i]*cost_few[i]
    } else {
      # cost_many
      total_campaign_cost <- total_campaign_cost + X[i]*cost_many[i]
    }
  }
  
  if (total_campaign_cost <= maximum_campaign_cost) {
    # OK, return minus the audience
    return(-sum(audience*X))
  }
  return(0)
}


monitorFunc <- function(result) {
  cat("Best of gen: ", min(result$best$cost), "-", result$best$genome, "\n")
}

# first try
result <- GeneticAlg.int(genomeLen = 8, codonMin = 0, codonMax = 125,
                    allowrepeat = TRUE, terminationCost = -1e12,
                    monitorFunc = NULL, evalFunc = evalFunc,
                    iterations = 100, popSize = 2000,
                    suggestions = list(rep(1,times=8)))
print(result)
X <- result$best$genome
# solution: 0 0 0 0 0 0 115 0
total_campaign_cost <- 0
for (i in 1:length(X)) {
  if (X[i] <= 3) {
    # cost_few
    total_campaign_cost <- total_campaign_cost + X[i]*cost_few[i]
  } else {
    # cost_many
    total_campaign_cost <- total_campaign_cost + X[i]*cost_many[i]
  }
}
total_campaign_cost

print(result$population$best)
plot(result$population$best)
