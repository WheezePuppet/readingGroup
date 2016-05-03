# Davies code
# Modified by DJM 
# Search for a random graph that has a duplicate eigenvalue for its algebraic
# connectivity.

library(igraph)
library(doParallel)
registerDoParallel(8)

set.seed(1234)

# Given a matrix (presumably the Laplacian of some graph), return its
# algebraic connectivity and the multiplicity of its algebraic connectivity.
alg.conn <- function(laplacian,TOLERANCE=1e-10) {   
    evals <- round(eigen(laplacian)$values, -log10(TOLERANCE))
    alg.conn.eval <- rev(unique(evals))[2]
    return(list(eval=alg.conn.eval,
        multiplicity=sum(evals == alg.conn.eval)))
}

generate.random.graph <- function(connected.only=TRUE) {
    g <- erdos.renyi.game(sample(10:40,1), runif(1,min=.25,max=.5))
	 if(connected.only){
	    cls <- components(g)
		 if(cls$no>1){
		    comp <- which.max(cls$csize)
			 g <- induced_subgraph(g,which(cls$membership==comp))
		 }
	 }
	 g
}

search.for.dups <- function(limit=1e4) {
    ctr <- 0
    g <- generate.random.graph()
    alg.conn.g <- alg.conn(laplacian_matrix(g))
    while (alg.conn.g$multiplicity == 1  &&  ctr < limit) {

        ctr <- ctr + 1
        if (ctr %% 1000 == 0) {
            cat("Tried", ctr,"graphs...\n")
        }

        g <- generate.random.graph()
        alg.conn.g <- alg.conn(laplacian_matrix(g))
    }

    if (ctr < limit) {
        cat("FOUND ONE!\n")
        save.file <- tempfile(pattern="dupConn", tmpdir=getwd(),
            fileext=".RData")
        save(g, file=save.file)
        return(1)
    }
    cat("None.\n")
    return(0)
}

# Begin a quest to find random graphs with duplicate alg conns, using all
# available cores. The number of attempts (chunks of searches) and the limit
# (maximum number of searches per attempt) can both be specified.
#
# Returns the number of attempts that successfully found such a graph. The
# graph itself will be written to a (randomly-named) .RData file starting with
# "dupConn".
search.lots <- function(num.attempts=50, limit.per.attempt=1e4) {
    foreach(attempt=1:num.attempts, .combine=`+`) %dopar% {
        cat("Attempt #",attempt,"...\n",sep="")
        search.for.dups(limit.per.attempt)
    }
}

plot.dup.graphs <- function(to.files=FALSE) {
    invisible(
        lapply(list.files(pattern="dupConn.*RData"), function(fn) {
            load(fn)
            alg.conn.g <- alg.conn(laplacian_matrix(g))
            if (to.files) {
                pdf(tempfile(pattern="plot",tmpdir=getwd(),fileext=".pdf"))
            }
            plot(g,
                main=bquote(lambda[2]==.(round(alg.conn.g$eval,4)) ~ 
                    "of multiplicity" ~ .(round(alg.conn.g$multiplicity))))
            if (to.files) {
                dev.off()
            } else {
                readline()
            }
        })
    )
}

get.dup.graphs <- function(){
    invisible(
        lapply(list.files(pattern="dupConn.*RData"), function(fn) {
				load(fn)
            g
        })
    )
}

plot.fiedlers <- function(g,...){
	layout(rbind(c(1,1,1,0),
	             c(1,1,1,0),
	             c(1,1,2,2),
	             c(0,0,2,2)))
	plot(g,...)
   a <- alg.conn(laplacian_matrix(g))
   ev <- eigen(laplacian_matrix(g))
	n <- gorder(g)
	m <- a$multiplicity
	vects <- ev$vectors[,n-(1:m)]
	plot(1:m,type='n',xlim=c(.9,m+.1),ylim=range(vects),xlab="",ylab="",axes=FALSE)
	text(rep(1,n),vects[,1],1:n,pos=2)
	text(rep(m,n),vects[,m],1:n,pos=4)
	for(i in 1:m) points(rep(i,n),vects[,i],pch=20)
	for(i in 1:(m-1)){
	   segments(rep(i,n),vects[,i],rep(i+1,m),vects[,i+1])
	}
	abline(h=0,col=gray(.8))
	box()
}

# test to see if a graph has the property
# that, for each of the Fiedler vectors:
# 1) the induced subgraph of the non-negative vertices is connected.
# 2) adding negative vertices in decreasing order keeps the graph connected.
test.fiedler <- function(g,TOLERANCE=1e-10)
{
   a <- alg.conn(laplacian_matrix(g))
   ev <- eigen(laplacian_matrix(g))
	n <- gorder(g)
	m <- a$multiplicity
	vects <- ev$vectors[,n-(1:m)]
	for(i in 1:m){
		v <- vects[,i]
		v <- round(v, -log10(TOLERANCE))
		if(no.clusters(induced_subgraph(g,which(v>=0)))>1){
		   cat(paste0(m,":"),"Failed at 0\n")
			return(FALSE)
		}
		vals <- v[which(v<0)]
		for(val in vals){
		   if(no.clusters(induced_subgraph(g,which(v>=val)))>1){
				cat(paste0(m,":"),"Failed at",val,"\n")
				return(FALSE)
			}
		}
	}
	TRUE
}

