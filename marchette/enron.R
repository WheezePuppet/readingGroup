
library(tcltk)


enronQ <- function(edges,remove.dups=TRUE,alpha=0.00001,interactive=TRUE)
{
    ## Remove self-loops.
    a <- which(edges[,2]==edges[,3])
    if(length(a)>0) edges <- edges[-a,]

    ## Remove duplicate edges. Some emails have the same
    ## people in the to and cc fields, and so these people
    ## show up more than once in the same email.
    if(remove.dups){
        edges <- unique(edges)
    }

    ## Make each edge have a unique time, so that
    ## we can pretend the edges are coming in at distinct
    ## times. So, an email from a to b,c,d will end up being
    ## three edges in a random order, all sent during the same second
    times <- edges$time+runif(nrow(edges))
    edges <- edges[order(times),]
    times <- sort(times)

    # Q -- the (evolving) centrality matrix. Each time through the loop,
    # include the factor for the graph with (only) the ith edge.
    # A -- the factor for the ith edge. This is (I - alpha*A_i)^-1, where A_i
    # is the adjacency matrix that includes only the ith edge.
    Q <- diag(184)
    A <- diag(184)

    if (interactive) {
        pb <- tkProgressBar(title=paste0("remove.dups=",remove.dups,
            ", alpha=",alpha),min=0,max=length(times))
    }

    out <- data.frame(time=times,
                      most.influenced.user=rep(0L,length(times)),
                      most.influenced.value=rep(0.0,length(times)),
                      most.influential.user=rep(0L,length(times)),
                      most.influential.value=rep(0.0,length(times)))
    for(i in 1:length(times)){

        if (interactive) {
            setTkProgressBar(pb,i,label=paste0("Computing Katz centrality: ",
                round(100*i/length(times),0),"%"))
        }

        # Build the factor for the ith edge, namely (I - alpha*A_i)^-1, where
        # A_i is the adjacency matrix that includes only the ith edge.

        # As it happens, we can compute this inverse without actually doing a
        # matrix operation: the matrix with 1's on diagonal, x in one other
        # element, and 0's everwhere else has as its inverse the same matrix
        # but with -x in that element. So the factor for the ith edge is
        # simply the matrix with 1's on diagonal, -(-alpha) in the ith edge's
        # position, and 0's everywhere else.
        A[edges[i,2],edges[i,3]] <- alpha

        # Multiply the ith edge's factor into Q, and reset A.
        Q <- Q%*%A
        A[edges[i,2],edges[i,3]] <- 0

        # Compute the "most influenced" (receiving) vertex and "most
        # influential" (sending) vertex, if the dynamic graph were to stop at
        # this point in time.
        influenced.metrics <- apply(Q,1,sum)
        influencing.metrics <- apply(Q,2,sum)
        out$most.influenced.user[i] <- which.max(influenced.metrics)
        out$most.influenced.value[i] <- max(influenced.metrics)
        out$most.influential.user[i] <- which.max(influencing.metrics)
        out$most.influential.value[i] <- max(influencing.metrics)
    }
    if (interactive) {
        close(pb)
    }
    out
}

enronQAgg <- function(edges,remove.dups=TRUE,alpha=0.00001,interactive=TRUE,
                      timestep=3600*24*7)
{
	 start <- min(edges[,1])
	 omega <- max(edges[,1])
	 gtimes <- seq(start,omega+timestep,by=timestep)
    ## Remove self-loops.
    a <- which(edges[,2]==edges[,3])
    if(length(a)>0) edges <- edges[-a,]

    ## Remove duplicate edges. Some emails have the same
    ## people in the to and cc fields, and so these people
    ## show up more than once in the same email.
    if(remove.dups){
        edges <- unique(edges)
    }

	 ntimes <- length(gtimes)-1

    # Q -- the (evolving) centrality matrix. Each time through the loop,
    # include the factor for the graph with (only) the ith edge.
    # A -- the factor for the ith edge. This is (I - alpha*A_i)^-1, where A_i
    # is the adjacency matrix that includes only the ith edge.
    Q <- diag(184)

    if (interactive) {
        pb <- tkProgressBar(title=paste0("remove.dups=",remove.dups,
            ", alpha=",alpha),min=0,max=ntimes)
    }

    out <- data.frame(time=gtimes[1:ntimes],
                      most.influenced.user=rep(0L,ntimes),
                      most.influenced.value=rep(0.0,ntimes),
                      most.influential.user=rep(0L,ntimes),
                      most.influential.value=rep(0.0,ntimes))
	 for(i in 1:ntimes){

        if (interactive) {
            setTkProgressBar(pb,i,label=paste0("Computing Katz centrality: ",
                round(100*i/length(gtimes),0),"%"))
        }

		  A <- diag(184)
		  e <- unique(edges[edges[,1]>=gtimes[i] & edges[,1]<gtimes[i+1],
		              2:3,drop=FALSE])
		  for(j in 1:nrow(e)) A[e[j,1],e[j,2]] <- -alpha
		  B <- solve(A)

        # Multiply the ith edge's factor into Q, and reset A.
        Q <- Q%*%B

        # Compute the "most influenced" (receiving) vertex and "most
        # influential" (sending) vertex, if the dynamic graph were to stop at
        # this point in time.
        influenced.metrics <- apply(Q,1,sum)
        influencing.metrics <- apply(Q,2,sum)
        out$most.influenced.user[i] <- which.max(influenced.metrics)
        out$most.influenced.value[i] <- max(influenced.metrics)
        out$most.influential.user[i] <- which.max(influencing.metrics)
        out$most.influential.value[i] <- max(influencing.metrics)
    }
    if (interactive) {
        close(pb)
    }
    out
}

# Load enron.RData and plot the max Katz Centrality over time, removing dup
# edges and using baseline alpha.
test.stephen <- function() {
    load("enron.RData")
    out <- enronQ(edges)
    par(mfrow=c(2,1))
	 pchs <- (out$most.influenced.user%%20)+1
    plot(out$time,out$most.influenced.value,pch=pchs,
        col=out$most.influenced.user,xlab="Time",
        ylab="Katz Centrality: influenced")
    legend('topleft',legend=users[unique(out$most.influenced.user),1],
        col=unique(out$most.influenced.user), pch=unique(pchs))
	 pchs <- (out$most.influential.user%%20)+1
    plot(out$time,out$most.influential.value,pch=pchs,
        col=out$most.influential.user,xlab="Time",
        ylab="Katz Centrality: influential")
    legend('topleft',legend=users[unique(out$most.influential.user),1],
        col=unique(out$most.influential.user), pch=unique(pchs))
}

test.dave <- function(timestep=3600*24*7) {
    load("enron.RData")
    out <- enronQAgg(edges,timestep=timestep)
    par(mfrow=c(2,1))
	 pchs <- (out$most.influenced.user%%20)+1
    plot(out$time,out$most.influenced.value,pch=pchs,
        col=out$most.influenced.user,xlab="Time",
        ylab="Katz Centrality: influenced")
    legend('topleft',legend=users[unique(out$most.influenced.user),1],
        col=unique(out$most.influenced.user), pch=unique(pchs))
	 pchs <- (out$most.influential.user%%20)+1
    plot(out$time,out$most.influential.value,pch=pchs,
        col=out$most.influential.user,xlab="Time",
        ylab="Katz Centrality: influential")
    legend('topleft',legend=users[unique(out$most.influential.user),1],
        col=unique(out$most.influential.user), pch=unique(pchs))
}

test <- function()
{
    load("enron.RData")
    print(system.time(out <- enronQ(edges)))
    plot(out$time,out$most.influenced.value,pch=20,
        col=out$most.influenced.user,xlab="Time",ylab="Katz Centrality")
    legend('topleft',legend=users[unique(out$most.influenced.user),1],
        col=unique(out$most.influenced.user),pch=20)

    set.seed(222)
    e <- edges[sample(nrow(edges),10000),]
    print(system.time(out2 <- enronQ(e)))
    x11()
    plot(out2$time,out2$most.influenced.value,pch=20,
        col=out2$most.influenced.user,xlab="Time",ylab="Katz Centrality")
    legend('topleft',legend=users[unique(out2$most.influenced.user),1],
        col=unique(out2$most.influenced.user),pch=20)

    print(system.time(out3 <- enronQ(edges,remove.dups=FALSE)))
    x11()
    plot(out3$time,out3$most.influenced.value,pch=20,
        col=out3$most.influenced.user,xlab="Time",ylab="Katz Centrality")
    legend('topleft',legend=users[unique(out3$most.influenced.user),1],
        col=unique(out3$most.influenced.user),pch=20)
}

test1 <- function(n=1000,alpha=0.01)
{
    load("enron.RData")
    edges2 <- edges[1:n,]
    out <- enronQ(edges2,alpha)
}

test2 <- function(n=1000,alpha=0.01)
{
    set.seed(222)
    load("enron.RData")
    pfrom <- table(c(1:184,edges$from))-1
    pfrom <- pfrom/sum(pfrom)
    pto <- table(c(1:184,edges$to))-1
    pto <- pfrom/sum(pto)
    edges2 <- data.frame(time=sort(sample(1:100,n,replace=TRUE)),
                            from=sample(1:184,n,prob=pfrom,replace=TRUE),
                            to=sample(1:184,n,prob=pto,replace=TRUE))
    
   out <- enronQ(edges2,alpha)
}
