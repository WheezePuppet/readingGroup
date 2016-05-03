
# Search for a random graph that has a duplicate eigenvalue for its algebraic
# connectivity.

library(igraph)
library(doParallel)
registerDoParallel(8)

set.seed(1234)

# Given a matrix (presumably the Laplacian of some graph), return its
# algebraic connectivity and the multiplicity of its algebraic connectivity.
TOLERANCE <- 1e-10
alg.conn <- function(laplacian) {
    evals <- round(eigen(laplacian)$values, -log10(TOLERANCE))
    alg.conn.eval <- rev(unique(evals))[2]
    return(list(eval=alg.conn.eval,
        multiplicity=sum(evals == alg.conn.eval)))
}

generate.random.graph <- function() {
    erdos.renyi.game(sample(10:40,1), runif(1,min=.25,max=.5))
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

plot.dup.graphs <- function() {
    invisible(
        lapply(list.files(pattern="dupConn"), function(fn) {
            load(fn)
            alg.conn.g <- alg.conn(laplacian_matrix(g))
            plot(g,
                main=bquote(lambda[2]==.(round(alg.conn.g$eval,4)) ~ 
                    "of multiplicity" ~ .(round(alg.conn.g$multiplicity))))
            readline()
        })
    )
}
