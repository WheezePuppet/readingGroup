require(tidyr)
require(ggplot2)
require(doParallel)
registerDoParallel(8)

max.iter <- 2000

# Elizabeth: plot all the separators (for the same data set.)


run.til.convergence <- function(num.data.points=100, plot=FALSE,
    verbose=TRUE) {

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


    # Initial weights.
    w <- rep(0,3)
    w.history <- matrix(w,nrow=1)
    colnames(w.history) <- c("w0","w1","w2")

    iter <- 1
    repeat {

        data$h <- sign(as.vector(t(as.matrix(w)) %*% rbind(1,data$x1,data$x2)))

        misclass.pts <- filter(data,y!=h)

        if (nrow(misclass.pts) <= 0) {
            if (verbose) cat("Converged!\n")
            break
        } else {
            if (verbose) cat(nrow(misclass.pts), " misclassified points (",
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

    # Plot points and separator.
    if (plot) {
        p <- ggplot(data, aes(x=x1, y=x2, col=factor(y))) + 
            geom_abline(intercept=f.intercept, slope=f.slope, color="purple",
                size=1.2) +
            geom_point() + 
            scale_color_manual(name="Class",values=c("red","blue")) +
            geom_abline(intercept=linear.parameters.for(w)["intercept"],
                        slope=linear.parameters.for(w)["slope"]) +
            expand_limits(x=c(-1.5,1.5),y=c(-1.5,1.5))

        cols <- colorRampPalette(c("white","black"))(nrow(w.history))
        for (iter in 1:nrow(w.history)) {
            p <- p + geom_abline(
                intercept=linear.parameters.for(w.history[iter,])["intercept"],
                        slope=linear.parameters.for(w.history[iter,])["slope"],
                col=ifelse(iter==nrow(w.history),"green",cols[iter]))
        }
        print(p)
    }

    w.history <- cbind(iter=1:nrow(w.history), w.history)

    return(w.history)
}



# Run the perceptron algorithm num.runs times on num.data.points each, and
# return a vector of the number of iterations each one took to converge.
measure.convergence.times <- function(num.runs=1000,num.data.points=100,
                                                                plot=TRUE) {
    results <- foreach(run=1:num.runs, .combine=c) %dopar% {
        nrow(run.til.convergence(num.data.points=num.data.points))
    }
    if (plot) {
        p <- ggplot(data.frame(num.iter=results),aes(x=num.iter)) + 
            geom_bar(stat="bin",fill="yellow",col="black",
                binwidth=num.data.points/10) +
            geom_vline(xintercept=mean(results),color="blue") +
            scale_x_continuous(breaks=seq(0,1,.1)*max(results))+
            annotate("text",x=mean(results),y=Inf,vjust=2,col="blue",
                label=paste0("mean=",mean(results))) +
            labs(x="Number of iterations to converge")
        print(p)
    }
    results
}


plot.w.history <- function(w.history) {
    w.hist.df <- as.data.frame(w.history)
    w.hist.df <- gather(w.hist.df, w.coord, val, w0:w2)
    p <- ggplot(w.hist.df, aes(x=iter)) +
        geom_line(aes(y=val, col=w.coord)) +
        labs(x="iteration", y="value", title="Weights over time") +
        scale_color_discrete(name="Weight coordinate",
            labels=c(expression(w[0]),expression(w[1]),expression(w[2])))
    print(p)
}


# Given a 3-element weight vector, return a named vector with the
# corresponding slope and x2-intercept.
linear.parameters.for <- function(w) {
    if (!is.atomic(w) || length(w) != 3) {
        stop("w not a three element vector.")
    }
    names(w) <- NULL

    # Remember: the equation for determining which class the point belongs to
    # has its boundary at:
    #    w0 + w1*x1 + w2*x2 = 0
    # which means that the slope and x2-intercept of this line are:
    #    -w2*x2 = w1*x1 + w0
    #    x2 = (w1*x1)/-w2 + w0/-w2
    #    x2 = (w1/-w2)*x1 + w0/-w2
    c(slope=-w[2]/w[3], intercept=-w[1]/w[3])
}
