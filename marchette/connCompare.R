
# Plot algebraic vs. vertex connectivity.

library(igraph)
library(doParallel)
registerDoParallel(8)

source("search.R")

NUM.GRAPHS <- 1e4
NUM.VERTICES.RANGE <- c(10,40)
EDGE.PROB.RANGE <- c(.25,.5)
CONNECTED.ONLY <- TRUE

CHUNK.SIZE <- 1e2
results <- foreach (chunk=1:(NUM.GRAPHS/CHUNK.SIZE), .combine=rbind) %dopar% {

    cat("Starting chunk",chunk,"of",NUM.GRAPHS/CHUNK.SIZE,"...\n")

    alg.conns <- vector(length=CHUNK.SIZE)
    vertex.conns <- vector(length=CHUNK.SIZE)

    for (i in 1:CHUNK.SIZE) {
        g <- generate.random.graph(connected.only=CONNECTED.ONLY,
            num.vertices.range=NUM.VERTICES.RANGE,
            edge.prob.range=EDGE.PROB.RANGE)
        alg.conns[i] <- alg.conn(laplacian_matrix(g))$eval
        vertex.conns[i] <- vertex.connectivity(g)
    }

    return(data.frame(alg.conns, vertex.conns))
}

hist(results$alg.conns, breaks=20, main="algebraic connectivities",
    sub=paste0(NUM.GRAPHS, ifelse(CONNECTED.ONLY," connected",""),
        " E-R graphs with ",NUM.VERTICES.RANGE[1],"-",
        NUM.VERTICES.RANGE[2]," vertices and (",EDGE.PROB.RANGE[1],
        ",",EDGE.PROB.RANGE[2],") edge connection probs"))
readline("Press ENTER.")
plot(results$vertex.conns, results$alg.conns, xlab="vertex connectivity",
    ylab="algebraic connectivity",
    main="It's mysterious, but true...")
abline(0,1)
