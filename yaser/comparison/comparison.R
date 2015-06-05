library(MASS)
library(dplyr)

N.PTS <- 2000
PRIOR <- .5

generate.variate <- function(num.data.points, gaussianness) {
    gaussianness * rnorm(num.data.points, mean=0, sd=1) +
       (1-gaussianness) * runif(num.data.points, -1, 1)
}


# Return a list with two data frames:
#   "training": num.pts (x1, x2, y) tuples. 
#   "test": num.pts (x1, x2, y) tuples. 
#
# covar.neg1: the 2x2 covariance matrix for class -1. (class 1 is identity.)
# mean.neg1: the 2-d mean for class -1. (class 1 is always mean=c(0,0).)
# prior: the fraction of num.pts that are in class 1.
# gaussianness: the contribution towards x1 and x2 from a normal distro
#   (1-gaussianness will be from a uniform[-1,1].)  
generate.data <- function(num.pts=N.PTS, covar.neg1=diag(2), 
    mean.neg1=c(0,0), prior=.5, gaussianness=1.0) {

    num.class.1 <- trunc(prior * num.pts)
    num.class.neg1 <- num.pts - num.class.1

    mean.1 <- c(0,0)
    covar.1 <- diag(2)

    # (helper function)
    generate.data.set <- function(num.class.1, mean.1, covar.1, 
        num.class.neg1, mean.neg1, covar.neg1, gaussianness) {

        # (helper helper function)
        generate.data.set.for.class <- function(num.pts, mean.of.pts, covar) {

            # TODO: replace with Dave's way (Cauchy)
            uniform <- matrix(c(
                # 2 std dev's each way? maybe?
                runif(num.pts, mean.of.pts[1]-2, mean.of.pts[1]+2),
                runif(num.pts, mean.of.pts[2]-2, mean.of.pts[2]+2)),
                nrow=num.pts,byrow=FALSE)
            gaussian <- mvrnorm(num.pts, mean.of.pts, covar)
            data.frame(
                uniform * (1-gaussianness) +
                gaussian * (gaussianness)
            )
        }

        # Create class 1 data.
        data.1 <- generate.data.set.for.class(num.class.1, mean.1, covar.1)
        data.1$y <- 1

        # Create class -1 data.
        data.neg1 <- generate.data.set.for.class(num.class.neg1, mean.neg1,
            covar.neg1)
        data.neg1$y <- -1

        ret.val <- rbind(data.1, data.neg1)
        names(ret.val) <- c("x1","x2","y")
        ret.val$y <- as.factor(ret.val$y)
        ret.val
    }

    list(
        training=generate.data.set(num.class.1, mean.1, covar.1,
            num.class.neg1, mean.neg1, covar.neg1, gaussianness),
        test=generate.data.set(num.class.1, mean.1, covar.1, num.class.neg1, 
            mean.neg1, covar.neg1, gaussianness)
    )
}


run.lda <- function(data) {
    
# TODO: interaction terms?

    lda.model <- lda(y~x1+x2,data$training)
    list(model=lda.model,
        accuracy=sum(
            predict(lda.model,data$test[,1:2])$class == data$test$y
        ) / nrow(data$test)
    )
}

run.qda <- function(data) {
    
    qda.model <- qda(y~x1+x2,data$training)
    list(model=qda.model,
        accuracy=sum(
            predict(qda.model,data$test[,1:2])$class == data$test$y
        ) / nrow(data$test)
    )
}

get.posterior.probs <- function(model) {
    if ("lda" %in% class(model)) {
        return(predict(model)$posterior[,2])
    } else if ("glm" %in% class(model)) {
        return(predict(model))
    } else if ("qda" %in% class(model)) {
        stop("QDA posterior probs not supported.")
    }
}

run.log.reg <- function(data, prior=.5) {
    
    log.reg.model <- glm(y~x1+x2,data=data$training, family=binomial(logit))
    # Need type="response" in the predict() call to get probabilities in the
    # [0,1] range; see ?predict.glm.
    predictions <- as.factor(
        ifelse(predict(log.reg.model,data$test[,1:2],type="response") > PRIOR,
            1, -1))
    list(model=log.reg.model,
        accuracy=sum(
            predictions == data$test$y
        ) / nrow(data$test)
    )
}


compute.slope.intercept <- function(coeff, PRIOR) {
    stopifnot(is.numeric(coeff), length(coeff) == 3)
    list(slope=-coeff[2]/coeff[3],
        intercept=(-PRIOR-coeff[1])/coeff[3])
}

main <- function() {

    data <- generate.data(prior=PRIOR,mean.neg1=c(5,2))

    lda.run <- run.lda(data)
    qda.run <- run.qda(data)
    log.reg.run <- run.log.reg(data,PRIOR)

    cat("LDA accuracy:",lda.run$accuracy,"\n")
    cat("QDA accuracy:",qda.run$accuracy,"\n")
    cat("Log accuracy:",log.reg.run$accuracy,"\n")

    p <- ggplot(data$training) + geom_point(aes(x=x1, y=x2, color=y))

    # Plot the Log Reg decision boundary in black.
    # (How to do for LDA?? No clue.)
    sl.int <- compute.slope.intercept(coef(log.reg.run$model),PRIOR)
    p <- p + geom_abline(intercept=sl.int$intercept,slope=sl.int$slope,
        col="black",size=1.1)

    print(p)
}    
