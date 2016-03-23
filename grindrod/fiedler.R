
# Laplacian and Fiedler eigenvector (Grindrod sec. 1.5-1.6)
# Stephen - 3/21/16

library(igraph)

force.clique <- function(clique.participants) {

    # The idiomatic R way, which doesn't work
    #   my.g[clique.participants, clique.participants] <<- 1
    #   for (i in clique.participants) my.g[i,i] <<- 0

    # The caveman way, which does.
    for (i in clique.participants) {
        for (j in clique.participants) {
            if (i != j) {
                my.g[i,j] <<- 1
            }
        }
    }
}


num.vertices <- 12
my.g <- erdos.renyi.game(num.vertices, .15, type="gnp", directed=FALSE, 
    loops=FALSE)

# Force a clique.
clique.participants <- sample(1:num.vertices,5)
cat("Forcing a clique among:",clique.participants,"\n")
force.clique(clique.participants)

plot(my.g)
num.components <- count_components(my.g)
cat("\nThere are",num.components,"components.\n")
readline("Press enter.")

my.g.adj <- as_adjacency_matrix(my.g)
cat("\nThe adjacency matrix:\n")
print(my.g.adj)
readline("Press enter.")

my.g.lap <- laplacian_matrix(my.g)
cat("\nThe Laplacian matrix:\n")
print(my.g.lap)
readline("Press enter.")

eigen.return <- eigen(my.g.lap)
my.g.eigenvals <- eigen.return$values
my.g.eigenvecs <- eigen.return$vectors

plot(sort(my.g.eigenvals,decreasing=TRUE),pch=19,main="eigenvalues")
fiedler.eigenval <- sort(my.g.eigenvals,decreasing=TRUE)[
    num.vertices - num.components]
fiedler.eigenvec <- my.g.eigenvecs[,num.vertices - num.components]
abline(h=fiedler.eigenval,lty="dotted")
cat("\nFiedler eigenvalue is ",fiedler.eigenval,".\n",sep="")
cat("\nThe Fiedler eigenvector is:\n")
print(round(fiedler.eigenvec,2))
readline("Press enter.")

cat("\nThe permuted matrix, according to Fiedler eigenvector, is:\n")
print(my.g.adj[order(fiedler.eigenvec),order(fiedler.eigenvec)])

