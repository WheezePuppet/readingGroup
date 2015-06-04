
# R-squared is "the fraction of variance explained by the model". True or
# false: the sum of the R-squared's for different models that include (only
# one of) the different variables is 1?

# Answer: if the predictors are independent, it all seems to act as you would
# expect. In the example below,


include.dependent.predictor <- FALSE
n.pts <- 5e5

# Two predictors, x and y, independent.
total <- data.frame(x=rnorm(n.pts,5,3),y=rnorm(n.pts,5,3))

if (!include.dependent.predictor) {
    # The true response is twice x plus y plus noise of magnitude equal to y.
    total <- mutate(total,z = 2*x + y + rnorm(n.pts,5,3))
} else {
    # Add a third predictor which is partially correlated with y.
    total <- mutate(total,y2=.5*y+.5*rnorm(n.pts,5,3))
    # The true response is twice x plus y plus y2 plus noise of magnitude 
    #   equal to y.
    total <- mutate(total,z= 2*x + y + y2 + rnorm(n.pts,5,3))
}

# Four linear models: use only x, use only y, use both x and y, use both x and
# y and an interaction.
lm.xonly <- lm(z~x,data=total)
lm.yonly <- lm(z~y,data=total)
lm.xy <- lm(z~x+y,data=total)
lm.xyint <- lm(z~x*y,data=total)
if (!include.dependent.predictor) {
    rm(list=grep("^lm\\..*y2",ls(),value=TRUE))
} else {
    lm.y2only <- lm(z~y2,data=total)
    lm.yy2 <- lm(z~y+y2,data=total)
    lm.yy2int <- lm(z~y*y2,data=total)
    lm.xyy2 <- lm(z~x+y+y2,data=total)
    lm.xyy2int <- lm(z~x*y*y2,data=total)
}

models <- lapply(as.list(grep("^lm\\.[xy]",ls(),value=TRUE)), get)

lapply(models, function(m) {
    sm <- summary(m)
    cat(as.character(sm$call)[[2]], ":", round(sm$adj.r.squared,3), "\n")
})
