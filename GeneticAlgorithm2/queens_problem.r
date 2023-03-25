library("genalg")

N <- 5

# Measuring fitness (function)
evalFunc <- function(X) {
  # X = 1:(N*N)
  solution <- matrix(X, nrow = N, ncol = N, byrow = TRUE)
  if (sum(solution) != N) {
    return(1e5)
  }
  
  if (sum(rowSums(solution) > 1) > 0) {
    return(1e5)
  }

  if (sum(colSums(solution) > 1) > 0) {
    return(1e5)
  }

  for (k in -N:N) {
    tmp_sum <- solution[col(solution) - row(solution) == k]
    if (sum(tmp_sum) > 1) {
      return(1e5)
    }
  }
  
  for (k in 2:(2*N)) {
    tmp_sum <- solution[col(solution) + row(solution) == k]
    if (sum(tmp_sum) > 1) {
      return(1e5)
    }
  }
  
  return(0)
}


monitorFunc <- function(result) {
  cat("Best of gen: ", min(result$evaluations), "\n")
}

GAmodel <- rbga.bin(size = N*N, popSize = 10000, iters = 100, mutationChance = 0.3, elitism = TRUE, evalFunc = evalFunc, monitorFunc = monitorFunc)
# GAmodel <- rbga.bin(size = N*N, popSize = 10000, iters = 100, mutationChance = 0.3, elitism = TRUE, evalFunc = evalFunc, monitorFunc = NULL)
summary(GAmodel, echo=TRUE)

# examplary solution
X <- c(0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0)
solution <- matrix(X, nrow = N, ncol = N, byrow = TRUE)
solution
