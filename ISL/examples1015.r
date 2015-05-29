source("hw1015.R")

# CG: **UPDATE (10/20) - I added some examples and comments for specifying x-y correlations (i.e. cov. matrix)

# Example: Generate a set of 100000 (x,y) pairs with xmean=5, ymean=10, label = "good", sd = 0, 
# and correlation between x and y = 0.99
d1 <- data.gen(5, 10, 100000, 1, 3, 0.99)

# Show data is as expected:
print(mean(d1$x))
print(sd(d1$x))
print(mean(d1$y))
print(sd(d1$y))
print(cor(d1$x, d1$y))

# Example: Build training and test data sets (as a list). Use default train proportion, stdev, and correlations.
# Usage: build.dataset(f1_xmean, f1_ymean, f2_xmean, f2_ymean, n_observations, 
#                      train_proportion [= 0.7], stdev [=1.0], correl.f1 [=0.0], correl.f2 [=0.0])
td <- build.dataset(3, 0, 0, 3, 1000)

# Print sample of training and test datasets
message("Sample of training and test sets:")
print(head(td$train)) 
print(head(td$test))
message("\n")

# Print success rate KNN on test set (based on training set) for single k = 3
message("KNN success rate on test data for k = 3:")
print(knn.success.rate(td$train, td$test, 3))
message("\n")

# Print success rate KNN on test set (based on training set) for all odd k in 1..10
message("KNN success rate on test data for k = 1, 3, 5, 7, 9:")
#print(knn.success.rates(td$train, td$test, seq(1,10,by=2)))
message("\n")

# Print success rate for logistic regression on test set (based on training set).
message("Logistic regression success rate on test data:")
print(logit.success.rate(td$train, td$test))
message("\n")

