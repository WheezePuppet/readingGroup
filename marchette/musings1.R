
# Implementation of Marchette Musings vol. I.

require(igraph)

set.seed(123)

d <- 5
num.pts <- 12

# Generate a random d-dimensional vector whose Euclidean length is at most
# 1.
gen.vector <- function() {
    vec <- runif(d,0,1)
    return(vec/norm(as.matrix(vec),type="F")*runif(1,.2,1))
}

# X is a matrix whose columns are d-dimensional vectors, each representing
# one of our points/vertices.
X <- matrix(rep(0,num.pts*d),nrow=d)
for (i in 1:num.pts) {
    X[,i] <- gen.vector()
}

# These better all be between 0 and 1, or something's wrong.
cat("\nvector lengths:",apply(X,2,function(x) round(sum(x^2),3)),"\n")

# Create a random dot-product graph. Add an edge between vertices i and j
# with probability equal to the dot-product of i and j.
g <- make_empty_graph(num.pts, directed=FALSE)
for (i in 1:(num.pts-1)) {
    for (j in (i+1):num.pts) {
        if (runif(1) < X[,i] %*% X[,j]) {
            g <- g + edge(i,j)
        }
    }
}
plot(g)

A <- as_adjacency_matrix(g)
eigen.stuff <- eigen(A)
evals <- eigen.stuff$values
evecs <- eigen.stuff$vectors

cat("\nEigenvalues:",round(evals,3),"\n")
if (sum(evals>0) < d) {
    stop("Not enough positive eigenvalues.")
}
U.d <- evecs[,1:d]
Lambda <- diag(evals[1:d])
A.approx <- U.d %*% Lambda %*% t(U.d)
cat("\nAdjacency matrix:\n")
print(A)
cat("\nApproximation to adjacency matrix:\n")
print(round(A.approx,1))

cat("The Frobenius norm is",sqrt(sum((A - A.approx)^2)),"\n")

X.approx <- U.d %*% sqrt(Lambda)

cat("\nActual vectors:\n")
print(round(X,2))
cat("\nApproximation to vectors:\n")
print(round(t(X.approx),2))

# Want to compare X to X.approx. Could use Procrustes analysis here.
