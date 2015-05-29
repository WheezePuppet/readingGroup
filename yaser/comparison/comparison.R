library(MASS)
library(dplyr)

N.PTS <- 2000

generate.variate <- function(num.data.points, gaussianness) {
    gaussianness * rnorm(num.data.points, mean=0, sd=1) +
       (1-gaussianness) * runif(num.data.points, -1, 1)
}

# (Helper function, called by generate.data.)
generate.data.set <- function(num.class.1, mean.1, covar.1, 
    num.class.neg1, mean.neg1, covar.neg1, gaussianness) {

# TODO: implement gaussianness

    # Create class 1 data.
    data.1 <- data.frame(
        cbind(
            mvrnorm(num.class.1, mean.1,
                matrix(c(1,covar.1,covar.1,1),nrow=2)),
            1
        )
    )

    # Create class -1 data.
    data.neg1 <- data.frame(
        cbind(
            mvrnorm(num.class.neg1, mean.neg1,
                matrix(c(1,covar.neg1,covar.neg1,1),nrow=2)),
            -1
        )
    )

    ret.val <- rbind(data.1, data.neg1)
    names(ret.val) <- c("x1","x2","y")
    ret.val$y <- as.factor(ret.val$y)
    ret.val
}

# Return a list with two data frames:
#   "training": num.pts (x1, x2, y) tuples. 
#   "test": num.pts (x1, x2, y) tuples. 
#
# covar.1: the covariance between x1 and x2 for class 1.
# covar.neg1: the covariance between x1 and x2 for class -1.
# mean.x1.neg1: the mean of the x1 variable for class -1. (The mean for class
#   1 is always 0.)
# mean.x2.neg1: the mean of the x2 variable for class -1. (The mean for class
#   1 is always 0.)
# prior: the fraction of num.pts that are in class 1.
# gaussianness: the contribution towards x1 and x2 from a normal distro
#   (1-gaussianness will be from a uniform[-1,1].)  
generate.data <- function(num.pts=N.PTS, covar.1=0, covar.neg1=0, 
    mean.x1.neg1=1, mean.x2.neg1=1, prior=.5, gaussianness=1.0) {

    num.class.1 <- trunc(prior * num.pts)
    num.class.neg1 <- num.pts - num.class.1

    mean.1 <- c(0,0)
    mean.neg1 <- c(mean.x1.neg1,mean.x2.neg1)

    list(
        training=generate.data.set(num.class.1, mean.1, covar.1,
            num.class.neg1, mean.neg1, covar.neg1, gaussianness),
        test=generate.data.set(num.class.1, mean.1, covar.1, num.class.neg1, 
            mean.neg1, covar.neg1, gaussianness)
    )
}


lda.accuracy <- function(data) {
    
# TODO: interaction terms?

    lda.model <- lda(y~x1+x2,data$training)
    sum(
        predict(lda.model,data$test[,1:2])$class == data$test$y
    ) / nrow(data$test)
}

qda.accuracy <- function(data) {
    
    qda.model <- qda(y~x1+x2,data$training)
    sum(
        predict(qda.model,data$test[,1:2])$class == data$test$y
    ) / nrow(data$test)
}

log.reg.accuracy <- function(data) {
    
    log.reg.model <- glm(y~x1+x2,data=data$training, family=binomial(logit))
    # Need type="response" in the predict() call to get probabilities in the
    # [0,1] range; see ?predict.glm.
    predictions <- as.factor(
        ifelse(predict(log.reg.model,data$test[,1:2],type="response") > .5,
            1, -1))
    sum(
        predictions == data$test$y
    ) / nrow(data$test)
}

main <- function() {
    data <- generate.data()
    p <- ggplot(data$training) + geom_point(aes(x=x1, y=x2, color=y))
    print(p)
    cat("LDA accuracy:",lda.accuracy(data),"\n")
    cat("QDA accuracy:",qda.accuracy(data),"\n")
    cat("Log accuracy:",log.reg.accuracy(data),"\n")
}    
