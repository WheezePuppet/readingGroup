
# Start with Wasserman.
source("xtx.R")

# Redefine things from the Wasserman way to the way Yaser does them.
y <- targets$weight
hat.m <- x %*% xtxi %*% t(x)

# Does hat.m^2 = hat.m? NO.
if (!isTRUE(all.equal(hat.m %*% hat.m, hat.m))) {
    cat("Yaser is wrong??\n")
    print(all.equal(hat.m %*% hat.m, hat.m))
} else {
    cat("Yaser is right.\n")
}

y.hat <- as.vector(hat.m %*% y)
cat("RSS =",sum((y-y.hat)^2),"\n")


# For Dave:
# "In the language of linear algebra, the hat matrix is the orthogonal
# projection onto the column space of the design matrix X."
# "For linear models, the trace of the hat matrix is equal to the rank of X,
# which is the number of independent parameters of the linear model."
# "For other models such as LOESS that are still linear in the observations y,
# the hat matrix can be used to define the effective degrees of freedom of the
# model."
#
# The whole notion of "features" == non-linear transformation! (This isn't a
# weird SVM thing.)
#
# the standard translation to vector notation
#
# "it's linear in the w's, not just the x's"
