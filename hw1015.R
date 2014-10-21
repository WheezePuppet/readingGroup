
# Stephen's and Chris's homework for reading group 10/15

library(MASS)
library(ggplot2)
require(doParallel)
registerDoParallel(16)

# I. Generate data.
#    - f1 and f2 should each contain some number of two-dimensional points.
#    - f1 will have mean (3,0) and f2 will have mean (0,3).
#    - Both f1 and f2 will have identity covar matrix (which means - correct 
#    me if I'm wrong Chris - that each element of the vectors will have stdev
#    0 and will not be related to the other element).
#    - Assemble this all in a data frame with columns x1, x2, and class.
#    - (All this should be configurable.)

data.gen <- function(xmean, ymean, n, label, stdev = 1.0, correl = 0.0) {
	# CG: Generates a data distribution based on the inputs. 
	#     It is not required that SD = 0 for ident. cov. matrix.
	# CG: **UPDATE (10/20) -- Added in capability to specify correlation between x and y
	#       which enables specification of arbitrary cov. matrix. Didn't use the MASS package.
	xv = rnorm(n)
	rd = rnorm(n)
	yv = sapply(1:n, function(i){(correl * xv[i]) + (sqrt(1 - correl * correl) * rd[i])})
	xv <- sapply(xv, function(z) {(z * stdev) + xmean})
	yv <- sapply(yv, function(z) {(z * stdev) + ymean})
	data.frame(x = xv, y = yv, label = rep(label, n))
}

# II. Randomly separate data into test and training sets (perhaps by adding
#    another column to the data frame specifying which is which.)
build.dataset <- function(f1.xmean, f1.ymean, f2.xmean, f2.ymean, 
                           n, train.proportion = 0.7, stdev = 1.0, 
						   correl.f1 = 0.0, correl.f2 = 0.0) {
	# CG: Builds training and test sets as a list, with same number of f1 and f2 observations.
	#     Data in f1 will have label of 0 and f2 data will have label of 1.
	# CG: **UPDATE (10/20) - Added in ability to specify correlations for f1 and f2.
	f1 <- data.gen(f1.xmean, f1.ymean, n/2, 0, stdev, correl.f1)
	f2 <- data.gen(f2.xmean, f2.ymean, n/2, 1, stdev, correl.f2)
	all.data <- rbind(f1, f2)
	train.inds <- sample(1:(nrow(f1) + nrow(f2)), train.proportion * n)
	test.inds <- setdiff(1:(nrow(f1) + nrow(f2)), train.inds)
	list(train = all.data[train.inds,], test = all.data[test.inds,])
	# NOTE: Access list elements using "$" - just like a data frame.

    # SD: I'd like to be able to have f1 and f2 have different numbers of
    # points. We could either pass an n1 and n2, or pass an n and a
    # f1.proportion.
}


# III. Build and test knn classifiers.
#    1. Build the interpoint distance matrix for all points, training and
#    test.
#    2. For each (odd) value of k in some range,
#       a. For each test point,
#          i. Find the k nearest training neighbors to that test point, using
#          the interpoint distance matrix.
#          ii. Predict for this test point whichever class is the majority 
#          class for those neighbors.
#          iii. Compare that prediction to what the class actually is for that
#          test point.
#          iv. Count it as a success or failure.
#       b. Record in some data structure the percentage of successes.

library(class) # Load package for KNN.

successes <- function(predicted, actual) {
	# CG: Get the number of successes in a result set. Assumes length(predicted) == length(actual).
	sum(sapply(1:length(predicted), function(i) { as.numeric(predicted[i] == actual[i]) }))
}

knn.success.rate <- function(train, test, kval) {
	# CG: Take in a set of training and test sets and a k. Return the resulting percent success.
	predicted <- knn(subset(train, select = -c(3)), subset(test, select = -c(3)), 
	                 cl = as.factor(train$label), k = kval)
	actual <- as.factor(test$label)
	successes(predicted, actual) / length(predicted) # Return the proportion of successes.
}

knn.success.rates <- function(train, test, kvals) {
	# CG: Get the success rates for each k in kvals as a k x 2 matrix (each row is a k).
    rates <- foreach(k=kvals, .combine=rbind) %dopar% {
	    c(k=k,success.rate=knn.success.rate(train, test, k))
    }
    rates
}


# IV. Build and test linear classifier. (are we doing logistic regression
# here?)
#    1. Compute the training line coefficients for the line which optimally
#    separates the training points. (I confess I'm a bit fuzzy on how to do
#    this.)
#    2. For each test point,
#       a. Figure out which side of the training line it's on.
#       b. Compare that to what the class actually is for that test point.
#       c. Count it as a success or failure.

# CG: We actually don't need the coefficients to do the classification (at least here). 
#     However, if we want to do this manually (in case I misunderstood your intent), this is how:
#     1) Create model: mod <- glm(label ~ ., data = train, family = "binomial")
#     2) Get coefficients: b0 <- coefficients(model)["(Intercept)"] # intercept
#                          b1 <- coefficients(model)["x"] # gives x-coeff 
#                          b2 <- coefficients(model)["y"] # gives y coef.
#     3) Predicted probability of being class 1: prob(x,y) = 1 / (1 + exp(-(b0 + b1x + b2y))
#     4) Classification: if prob(x, y) <= 0.5 then class = 0, otherwise class = 1.

logit.success.rate <- function(train, test) {
	# CG: Gets the success rate for the given training and test sets using logistic regression.
	model <- glm(label ~ ., data = train, family = "binomial")
	# Prediction: Rounds to 0 or 1 based on predicted probability of being a 1:
	predicted <- sapply(predict(model, newdata = test, type = "response"), function(x) {round(x)})
	actual <- test$label
	successes(predicted, actual) / length(predicted) # Return the proportion of successes.
}

# V. Plot results.
#    1. Plot k vs. test error.
#    2. Add a point to the plot for the linear classifier test error, using
#    the correct number of degrees of freedom.

plot.dataset <- function(dataset, plot.test=FALSE) {
    if (plot.test) {
        to.plot <- dataset$test
        title <- "Test points"
    } else {
        to.plot <- dataset$train
        title <- "Training points"
    }
    xpts <- c(dataset$train$x,dataset$test$x)
    ypts <- c(dataset$train$y,dataset$test$y)
    p <- ggplot(to.plot,aes(x=x,y=y,color=as.factor(label)))
    print(p + geom_point() + ggtitle(title) + labs(color="Label") +
        xlim(min(xpts),max(xpts)) +
        ylim(min(xpts),max(xpts)))
}

plot.knn.point.results <- function(train, test, kval) {
	# SD: Take in a set of training and test sets and a k. Plot correct and
    # incorrect classifications.
	predicted <- knn(subset(train, select = -c(3)), subset(test, select = -c(3)), 
	                 cl = as.factor(train$label), k = kval)
	actual <- as.factor(test$label)

    test$correct <- predicted != actual

    p <- ggplot(test,
        aes(x=x,y=y,color=as.factor(label),shape=correct,size=correct)) +
        scale_size_discrete(range=c(2,3)) +
        scale_shape_manual(values=c(20,15))
    print(p + geom_point() + ggtitle(paste0("KNN Results (k=",kval,")")) +
        labs(color="Label"))
}

plot.knn.aggregate.results <- function(dataset, kvals) {
    # SD: Plot k vs. success rate for each k value in the kvals vector.
    p <- ggplot(as.data.frame(
            knn.success.rates(dataset$train, dataset$test, kvals)),
        aes(x=k,y=success.rate))
    print(p + geom_line())
}
