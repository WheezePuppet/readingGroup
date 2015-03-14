require(tidyr)
require(ggplot2)
require(doParallel)
registerDoParallel(8)

max.iter <- 2000

# Elizabeth: plot all the separators (for the same data set.)


run.til.convergence <- function(num.data.points=100, plot=FALSE) {

    # Create data points: inputs.
    data <- data.frame(
        x1=runif(num.data.points,-1,1),x2=runif(num.data.points,-1,1))

    # Compute target function.
    rand.pt.1 <- c(runif(1,-1,1),runif(1,-1,1))
    rand.pt.2 <- c(runif(1,-1,1),runif(1,-1,1))
    f.slope <- (rand.pt.2[2] - rand.pt.1[2]) / (rand.pt.2[1] - rand.pt.1[1])
    f.intercept <- rand.pt.1[2] - f.slope * rand.pt.1[1]

    # Compute data points: outputs.
    data$y <- ifelse(data$x1 * f.slope + f.intercept > data$x2, 1, -1)

    # Plot points.
    if (plot) {
        p <- ggplot(data, aes(x=x1, y=x2, col=factor(y))) + 
            geom_abline(intercept=f.intercept, slope=f.slope, color="purple") +
            #geom_point() + 
            geom_text(aes(x=x1,y=x2,label=row.names(data))) +
            scale_color_manual(values=c("red","blue"))
        print(p)
    }

    # Initial weights.
    w <- rep(0,3)
    w.history <- matrix(w,nrow=1)
    colnames(w.history) <- c("w0","w1","w2")

    iter <- 1
    repeat {

        data$h <- sign(as.vector(t(as.matrix(w)) %*% rbind(1,data$x1,data$x2)))

        misclass.pts <- filter(data,y!=h)

        if (nrow(misclass.pts) <= 0) {
            cat("Converged!\n")
            break
        } else {
            cat(nrow(misclass.pts), " misclassified points (",
                paste(row.names(data)[data$y != data$h],
                    collapse=","),").\n",sep="")
        }
        misclass.pt <- misclass.pts[sample(1:nrow(misclass.pts),1),]

        w <- w + misclass.pt$y * c(1, misclass.pt$x1, misclass.pt$x2)
        w.history <- rbind(w.history, w)

        iter <- iter + 1
        if (iter > max.iter) {
            break
        }
    }

    w.history <- cbind(iter=1:nrow(w.history), w.history)

    return(w.history)
}



measure.convergence.times <- function(num.runs=1000,num.data.points=100,
                                                                plot=TRUE) {
    results <- foreach(run=1:num.runs, .combine=c) %dopar% {
        nrow(run.til.convergence(num.data.points=num.data.points))
    }
    if (plot) {
        p <- ggplot(data.frame(num.iter=results),aes(x=num.iter)) + 
            geom_bar(stat="bin",fill="yellow",col="black")
        print(p)
    }
    results
}


plot.w.history <- function(w.history) {
    w.hist.df <- as.data.frame(w.history)
    w.hist.df <- gather(w.hist.df, w.coord, val, w0:w2)
    p <- ggplot(w.hist.df, aes(x=iter)) +
        geom_line(aes(y=val, col=w.coord)) +
        labs(x="iteration", y="value", title="Weights over time")
    print(p)
}
