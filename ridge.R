
# An attempt by Stephen to figure out what happens when you do ridge
# regression and have two closely-correlated variables. Do one or both of them
# get "zeroed-out" faster?

library(mvtnorm)
library(MASS)
library(ggplot2)
library(tidyr)

NUM.PTS <- 200
true.beta <- c(.4,.4,.4)
sigma <- matrix(c(
    1, .8,  0,
    .8, 1., 0,
    0,  0,  1), nrow=3)
inputs <- rmvnorm(NUM.PTS, mean=rep(0,3), sigma=sigma)
epsilon <- rnorm(NUM.PTS, 0, .5)
output <- inputs %*% true.beta + epsilon

results <- data.frame()
for (lambda in seq(0,50,.1)) {
    lm.ridge(output ~ inputs, lambda=lambda) -> lm.stuff
    results <- rbind(results,c(lambda,coef(lm.stuff)))
}
names(results) <- c("lambda","intercept",paste0("input",1:3))
results <- gather(results, input, coef, input1:input3)
p <- ggplot(results, aes(x=lambda,y=coef,color=input)) + 
    geom_point(size=.5) + 
    geom_smooth() +
    ylim(0,max(results$coef)) +
    xlab(expression(lambda)) +
    ylab(expression(hat(beta)[i]))
print(p)
