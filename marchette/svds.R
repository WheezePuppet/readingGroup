
# Verify that the number of non-zero singular values is the rank of the
# matrix.

A <- matrix(sample(-10:20,16),nrow=4)
for (i in 1:(nrow(A)-1)) {
    for (j in (i+1):ncol(A)) {
        A[j,i] <- A[i,j]
    }
}

eigen(A) -> eigen.stuff

# This should be diagonal:
D <- solve(eigen.stuff$vectors) %*% A %*% eigen.stuff$vectors
D[abs(D) < .000001]  <- 0  # roundoff details...
cat("A has rank",.rank(A),"and the sing vals are:",diag(D),"\n")

