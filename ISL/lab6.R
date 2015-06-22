
# lab 6 for my 305 grade data.


#################### First part: subset selection

require(leaps)
#fullfit <- regsubsets(Final~quizzes+HWs+Labs,data=grades)
#summary(fullfit)$bic
#summary(fullfit)$adjr2


grades.nas.removed <- na.omit(grades)

k <- 5
folds <- sample(1:k,nrow(grades.nas.removed),replace=TRUE)
cv.errors <- matrix(NA,k,3)


predict.regsubsets <- function(object, newdata, id, ...) {
    form <- as.formula(object$call[[2]])
    mat <- model.matrix(form, newdata)
    coefi <- coef(object, id=id)
    xvars = names(coefi)
    mat[,xvars] %*% coefi
}

# For each fold...
for (j in 1:k) {
    best.fit <- regsubsets(Final~quizzes+HWs+Labs, data=grades.nas.removed[folds != j,])
    # For each number-of-predictors...
    for (i in 1:3) {
        pred <- predict(best.fit, grades.nas.removed[folds == j,],id=i)
        cv.errors[j,i] <- mean((grades.nas.removed$Final[folds == j] - pred)^2)
    }
}

# Choose the model size that goes with the lowest MSE across all the folds.
best.size.model <- which.min(apply(cv.errors, 2, mean))

cat("regsubsets recommends a ", best.size.model, "-variable model.\n", sep="")

# Now that we've decided on a particular sized model, run it with all the data
# to get the best coefficients for that size.
best.model.run <- regsubsets(Final~quizzes+HWs+Labs, data=grades.nas.removed,
    nvmax=best.size.model)  # (nvmax setting just says "no point in going 
                            # higher than the number we're going to pick")

cat("Those coefficients are:\n")
print(coef(best.model.run, best.size.model))

p <- ggplot(grades.nas.removed, aes(x=quizzes, y=Final)) +
    geom_point(shape=20, aes(color=gender)) +
    geom_abline(intercept=43.131784, slope=4.277465) +
    expand_limits(x=c(0,10), y=c(0,100))
print(p)

# Dave: and what is the estimate of the test MSE here, so we can compare
# apples-to-apples with ridge and lasso?

cat("\n")



#################### Second part: shrinkage methods (ridge & lasso)

library(glmnet)

alphas <- c(0,1)   # 0=ridge, 1=lasso
names <- c("Ridge","Lasso")

# Need to make x matrix and y vector since glmnet interface is weird
x <- model.matrix(Final~quizzes+HWs+Labs+gender, data=grades.nas.removed)
y <- grades.nas.removed$Final

for (alpha in alphas) {

    # Note: this approach seems better than their previous, since you can
    # control the size of the training vs. test sets precisely.
    train <- sample(1:nrow(x), nrow(x)/2)
    test <- -train

    # Just for fun, let's manually give it the lambda values we want it to
    # try.
    lambda.values <- 10^seq(10,-2,length=100)

    cv.shrink.mod <- cv.glmnet(x[train,], y[train], alpha=alpha,
        lambda=lambda.values, nfolds=5)
    best.lambda <- cv.shrink.mod$lambda.min

    shrink.predictions <- predict(cv.shrink.mod, s=best.lambda, newx=x[test,])
    cat(names[alpha+1],"'s MSE for best lambda (", best.lambda, ") is ", 
        mean((shrink.predictions - y[test])^2), "\n", sep="")
    shrink.predictions <- predict(cv.shrink.mod, s=0, newx=x[test,], exact=TRUE)
    cat(names[alpha+1],"'s MSE for unpenalized least squares (0) is ", 
        mean((shrink.predictions - y[test])^2), "\n", sep="")
    shrink.predictions <- predict(cv.shrink.mod, s=Inf, newx=x[test,])
    cat(names[alpha+1],"'s MSE for null model (inf) is ", 
        mean((shrink.predictions - y[test])^2), "\n", sep="")

    # Weirdly, the way you get the coefficients of your model is to run 
    # predict()! (with a type="coefficients" argument).
    print(predict(cv.shrink.mod, s=best.lambda, type="coefficients")[1:6,])

}
