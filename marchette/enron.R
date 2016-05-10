

enronQ <- function(edges,remove.dups=TRUE,alpha=0.00001)
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

	out <- data.frame(time=times,
                      most.influenced.user=rep(0L,length(times)),
	                  most.influenced.value=rep(0.0,length(times)),
                      most.influential.user=rep(0L,length(times)),
	                  most.influential.value=rep(0.0,length(times)))
	for(i in 1:length(times)){

        if (i %% 100 == 0) cat(i,"...\n",sep="")

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
	out
}

# Load enron.RData and plot the max Katz Centrality over time, removing dup
# edges and using baseline alpha.
test.stephen <- function() {
	load("enron.RData")
	out <- enronQ(edges)
    par(mfrow=c(2,1))
	plot(out$time,out$most.influenced.value,pch=20,
        col=out$most.influenced.user,xlab="Time",
        ylab="Katz Centrality: influenced")
	legend('topleft',legend=users[unique(out$most.influenced.user),1],
        col=unique(out$most.influenced.user), pch=20)
	plot(out$time,out$most.influential.value,pch=20,
        col=out$most.influential.user,xlab="Time",
        ylab="Katz Centrality: influential")
	legend('topleft',legend=users[unique(out$most.influential.user),1],
        col=unique(out$most.influential.user), pch=20)
}

test <- function()
{
	load("enron.RData")
	print(system.time(out <- enronQ(edges)))
	plot(out$time,out$most.influenced.value,pch=20,col=out$most.influenced.user,xlab="Time",ylab="Katz Centrality")
	legend('topleft',legend=users[unique(out$most.influenced.user),1],col=unique(out$most.influenced.user),pch=20)
	set.seed(222)
	e <- edges[sample(nrow(edges),10000),]
	print(system.time(out2 <- enronQ(e)))
	x11()
	plot(out2$time,out2$most.influenced.value,pch=20,col=out2$most.influenced.user,xlab="Time",ylab="Katz Centrality")
	legend('topleft',legend=users[unique(out2$most.influenced.user),1],col=unique(out2$most.influenced.user),pch=20)

	print(system.time(out3 <- enronQ(edges,remove.dups=FALSE)))
	x11()
	plot(out3$time,out3$most.influenced.value,pch=20,col=out3$most.influenced.user,xlab="Time",ylab="Katz Centrality")
	legend('topleft',legend=users[unique(out3$most.influenced.user),1],col=unique(out3$most.influenced.user),pch=20)
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
