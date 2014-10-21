source("hw1015.R")

library(dplyr)

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

plot.dataset(ds)
pause("Data set.")
if (plot.point.performance) {
    for (k in seq(1,max.k,2)) {
        plot.knn.point.results(filter(ds,group=="train"),
            filter(ds,group=="test"),k)
        pause("Performance for k=",k,".")
    }
}

plot.knn.aggregate.results(ds, seq(1,max.k,2))
