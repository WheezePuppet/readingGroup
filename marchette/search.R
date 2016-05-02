
# Search for a random graph that has a duplicate eigenvalue for its algebraic
# connectivity.

library(igraph)

set.seed(1234)

# Given a matrix (presumably the Laplacian of some graph), return its
# algebraic connectivity and the multiplicity of its algebraic connectivity.
TOLERANCE <- 1e-10
alg.conn <- function(laplacian) {
    evals <- eigen(laplacian)$values
    alg.conn.eval <- rev(unique(evals[which(abs(evals) > TOLERANCE)]))[2]
    return(list(eval=alg.conn.eval,
        multiplicity=sum(abs(evals - alg.conn.eval) < TOLERANCE)))
}

generate.random.graph <- function() erdos.renyi.game(20, .15)

LIMIT <- 1e4
alg.conns <- vector(length=LIMIT)
ctr <- 0
g <- generate.random.graph()
alg.conn.g <- alg.conn(laplacian_matrix(g))
while (alg.conn.g$multiplicity == 1  &&  ctr < LIMIT) {

    ctr <- ctr + 1
    if (ctr %% 100 == 0) {
        cat("Tried", ctr,"graphs...\n")
    }

    g <- generate.random.graph()
    alg.conn.g <- alg.conn(laplacian_matrix(g))

    # Just for interest...
    alg.conns[ctr] <- alg.conn.g$eval
}

if (alg.conn.g$multiplicity > 1) {
    plot(g, 
        main=bquote(lambda[2]==.(round(alg.conn.g$eval,5)) ~ "of multiplicity"
        ~ .(round(alg.conn.g$multiplicity))))
    print(g)
    readline()
}

hist(alg.conns, breaks=20, main="algebraic connectivities")
