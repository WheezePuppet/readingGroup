
num.data.pts <- 20
max.iter <- 2000

# Create data points: inputs.
data <- data.frame(x1=runif(num.data.pts,-1,1),x2=runif(num.data.pts,-1,1))

# Compute target function.
rand.pt.1 <- c(runif(1,-1,1),runif(1,-1,1))
rand.pt.2 <- c(runif(1,-1,1),runif(1,-1,1))
f.slope <- (rand.pt.2[2] - rand.pt.1[2]) / (rand.pt.2[1] - rand.pt.1[1])
f.intercept <- rand.pt.1[2] - f.slope * rand.pt.1[1]

# Compute data points: outputs.
data$y <- ifelse(data$x1 * f.slope + f.intercept > data$x2, 1, -1)

# Plot points.
p <- ggplot(data, aes(x=x1, y=x2, col=factor(y))) + 
    geom_abline(intercept=f.intercept, slope=f.slope, color="purple") +
    #geom_point() + 
    geom_text(aes(x=x1,y=x2,label=row.names(data))) +
    scale_color_manual(values=c("red","blue"))
print(p)

# Initial weights.
w <- rep(0,3)

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
    cat("w now: ",round(w,2),"\n")

    iter <- iter + 1
    if (iter > max.iter) {
        break
    }
}
