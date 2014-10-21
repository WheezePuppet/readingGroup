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

print(get.dataset.plot(ds))
pause("Data set.")
if (plot.point.performance) {
    for (k in seq(1,max.k,2)) {
        print(get.knn.point.results.plot(filter(ds,group=="train"),
            filter(ds,group=="test"),k))
        pause("Performance for k=",k,".")
    }
}

agg.plot <- get.knn.aggregate.results.plot(ds, seq(1,max.k,2))

deg.freedom.logit <- 5   #?????

logit.pt <- data.frame(x=deg.freedom.logit,
   y=logit.success.rate(
        filter(ds,group=="train"), filter(ds,group=="test")))

print(agg.plot + 
    geom_point(data=logit.pt, aes(x=x,y=y),color="red",size=5) +
    annotate("text", x=logit.pt$x, y=logit.pt$y,
        label="logit classification"))
