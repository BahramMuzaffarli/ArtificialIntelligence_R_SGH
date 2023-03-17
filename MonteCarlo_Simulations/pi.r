# Generate an approximation of pi by Monte Carlo simulations
# Idea:
# 1) we draw points from a unit square (uniform distribution)
# 2) we count how many of them hit the quarter circle inscribed in the square
# 3) an average number of hits is the ratio of the areas of the two figures equals pi/4
# 4) as an approximation of pi, we can therefore take four times the average number of hits

generate <- function(n = 10) {
    # generating x coordinates
    x <- runif(n,0,1)
    # generating y coordinates
    y <- runif(n,0,1)
    # check which of the points lie inside a quarter circle
    shoot <- ifelse(sqrt(x^2+y^2)<=1, TRUE, FALSE)
    # number of hits
    hit <- mean(shoot)
    # approximation of the pi value
    my_pi <- 4*hit
    # information
    cat(paste('Approximation for n =', n, ' pi =', my_pi, '\n'))
    # graph
    jpeg(filename = paste0("graph_n", n, ".jpg"))
    plot(x[shoot], y[shoot], main=paste('n=', n), xlim=c(0,1), ylim=c(0,1), col='blue', xlab='x-coordinates', ylab='y-coordinates', pch=1)
    points(x[!shoot], y[!shoot], col='red', pch=3)
    dev.off()
}

generate(50)
generate(500)
generate(5000)

