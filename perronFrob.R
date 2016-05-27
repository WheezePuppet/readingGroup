
# Verify that I'm doing the Perron-Frobenius centrality calculation right.
# Stephen

library(igraph)

set.seed(1234)

g <- erdos.renyi.game(10,.4)

evcent.pf.vector <- evcent(g)$vector/norm(evcent(g)$vector,type="2")

eigen.stuff <- eigen(as.matrix(get.adjacency(g)))
manual.pf.vector <- eigen.stuff$vectors[,1]

if (isTRUE(all.equal(evcent.pf.vector, manual.pf.vector))) {
    cat("I'm doing it right.\n")
} else {
    cat("I'm not doing it right.\n")
}
