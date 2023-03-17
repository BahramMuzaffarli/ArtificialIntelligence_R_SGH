library("gramEvol")

evalFunc <- function(x=c()) {
  return(abs(2 - as.numeric(paste0(x[1], ".", paste0(x[-1], collapse = "")))^2))
}


monitorFunc <- function(result) {
  cat("approximation = ", as.numeric(paste0(result$best$genome[1], ".", paste0(result$best$genome[-1], collapse = ""))),
      "\nerror = ", min(result$best$cost), "\n")
}

# number of digits
n <- 5 
x <- GeneticAlg.int(genomeLen = n, codonMin = 0, codonMax = 9,
                    allowrepeat = TRUE, terminationCost = 0.00001,
                    monitorFunc = monitorFunc, evalFunc = evalFunc,
                    iterations = 100, popSize = 100)
print(x)
cat("sqrt(2) = ", sqrt(2), 
    "\napproximation = ", as.numeric(paste0(x$best$genome[1], ".", paste0(x$best$genome[-1], collapse = ""))),
    "\nerror = ", abs(2 - as.numeric(paste0(x$best$genome[1], ".", paste0(x$best$genome[-1], collapse = "")))^2), "\n")
 
