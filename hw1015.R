
# Stephen's and Chris's homework for reading group 10/15

# I. Generate data.
#    - f1 and f2 should each contain some number of two-dimensional points.
#    - f1 will have mean (3,0) and f2 will have mean (0,3).
#    - Both f1 and f2 will have identity covar matrix (which means - correct 
#    me if I'm wrong Chris - that each element of the vectors will have stdev
#    0 and will not be related to the other element).
#    - Assemble this all in a data frame with columns x1, x2, and class.
#    - (All this should be configurable.)

# II. Randomly separate data into test and training sets (perhaps by adding
#    another column to the data frame specifying which is which.)

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

# IV. Build and test linear classifier. (are we doing logistic regression
# here?)
#    1. Compute the training line coefficients for the line which optimally
#    separates the training points. (I confess I'm a bit fuzzy on how to do
#    this.)
#    2. For each test point,
#       a. Figure out which side of the training line it's on.
#       b. Compare that to what the class actually is for that test point.
#       c. Count it as a success or failure.

# V. Plot results.
#    1. Plot k vs. test error.
#    2. Add a point to the plot for the linear classifier test error, using
#    the correct number of degrees of freedom.
