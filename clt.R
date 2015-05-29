
# Empirical proof of Central Limit Theorem (p.77).

library(ggplot2)
require(doParallel)
registerDoParallel(8)


N.PTS <- 1e4
N.TRIALS <- 1e3
BLOCK.SIZE <- 1e2

normal.rv <- function() {
    return(rnorm(1,15,2.4))
}

weird.rv <- function() {
    split.it <- runif(1,0,1)
    if (split.it < .3) {
        return(rnorm(1,273,15.4))
    } else if (split.it < .4) {
        return(-35)
    } else {
        return(runif(1,15,27))
    }
}

gen.variates <- function(rv.func, num.variates=1e4) {
    sapply(1:num.variates, function(n) {
            rv.func()
        }
    )
}

some.normal.variates <- gen.variates(normal.rv,N.PTS)
some.weird.variates <- gen.variates(weird.rv,N.PTS)

create.hist <- function(a.vector, title="",binwidth=5) {
    df <- data.frame(a.vector)
    names(df) <- "x"
    return(ggplot(df,aes(x=x)) +
        geom_histogram(fill="yellow",color="black",binwidth=binwidth) +
        ggtitle(paste0(title,"(",length(a.vector),")")))
}

normal.variates.hist <- create.hist(some.normal.variates,"normal variates")
weird.variates.hist <- create.hist(some.weird.variates,"weird variates")



gen.means <- function(rv.func, num.means=1e4, verbose=FALSE) {
    foreach (block=1:(num.means/BLOCK.SIZE), .combine=c) %dopar% {
        if (verbose) cat("Starting block ",block,"...\n",sep="")
        means <- integer(BLOCK.SIZE)
        for (element in 1:BLOCK.SIZE) {
            means[element] <- mean(gen.variates(rv.func,N.PTS))
        }
        return(means)
    }
}

if (!exists("normal.means")) {
    gen.means(normal.rv, N.TRIALS, verbose=TRUE) -> normal.means
}
if (!exists("gen.means")) {
    gen.means(weird.rv, N.TRIALS, verbose=TRUE) -> weird.means
}

normal.means.hist <- create.hist(normal.means,"normal means")
weird.means.hist <- create.hist(weird.means,"weird means")


show.plots <- TRUE
if (show.plots) {
    plot(normal.variates.hist)
    readline()
    plot(weird.variates.hist)
    readline()
    plot(normal.means.hist)
    readline()
    plot(weird.means.hist)
    readline()
    for (num in 10:length(weird.means)) {
        weird.means.hist <- create.hist(weird.means[1:num],
            paste0("weird means"), binwidth=.2)
        plot(weird.means.hist)
    }
}
