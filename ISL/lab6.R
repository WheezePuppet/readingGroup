
# lab 6 for my 305 grade data.

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
for (j in 1:k) {
    best.fit <- regsubsets(Final~quizzes+HWs+Labs, data=grades.nas.removed[folds != j,])
    for (i in 1:3) {
        pred <- predict(best.fit, grades.nas.removed[folds == j,],id=i)
        cv.errors[j,i] <- mean((grades.nas.removed$Final[folds == j] - pred)^2)
    }
}

best.size.model <- which.min(apply(cv.errors, 2, mean))

cat("regsubsets recommends a ", best.size.model, "-variable model.\n", sep="")

best.model.run <- regsubsets(Final~quizzes+HWs+Labs, data=grades.nas.removed,
    nvmax=best.size.model)
cat("Those coefficients are:\n")
print(coef(best.model.run, best.size.model))

p <- ggplot(grades.nas.removed, aes(x=quizzes, y=Final)) +
    geom_point(shape=20, aes(color=gender)) +
    geom_abline(intercept=43.131784, slope=4.277465) +
    expand_limits(x=c(0,10), y=c(0,100))
print(p)

