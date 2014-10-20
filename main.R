source("hw1015.R")

# Parameters:

# Plot specific success/error points for each k?
plot.point.performance <- TRUE  

n.pts <- 2000  # Total number of points (divided equally between classes)
f1.xmean <- 2
f1.ymean <- 0
f2.xmean <- 0
f2.ymean <- 2
max.k <- 201   # Highest (odd) k to try.




pause <- function(...) {
    cat(...," (Press ENTER.) ",sep="")
    readline()
}

ds <- build.dataset(f1.xmean, f1.ymean, f2.xmean, f2.ymean, n.pts)

plot.dataset(ds,FALSE)
pause("Training points.")
plot.dataset(ds,TRUE)
pause("Test points.")
if (plot.point.performance) {
    for (k in seq(1,max.k,2)) {
        plot.knn.point.results(ds$train,ds$test,k)
        pause("Performance for k=",k,".")
    }
}

plot.knn.aggregate.results(ds, seq(1,max.k,2))
