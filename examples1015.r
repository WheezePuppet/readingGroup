source("hw1015.R")

# Example: Build training and test data sets (as a list).
# Usage: build.dataset(f1_xmean, f1_ymean, f2_xmean, f2_ymean, n_observations, train_proportion [= 0.7], stdev [=1.0])
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
print(knn.success.rates(td$train, td$test, seq(1,10,by=2)))
message("\n")

# Print success rate for logistic regression on test set (based on training set).
message("Logistic regression success rate on test data:")
print(logit.success.rate(td$train, td$test))
message("\n")

