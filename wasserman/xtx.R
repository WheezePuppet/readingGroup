
do.plots <- FALSE

# Linear regression, manually.

predictors <- data.frame(
    intercept=1,
    name=c("Daddy","Mommy","Lizzy","TJ","Johnny"),
    age=c(45,45,15,13,11),
    height=c(6*12+2,5*12+5,5*12+1,5*12+5,4*12+6),
    gender=factor(c("M","F","F","M","M"))
)

targets <- data.frame(
    weight=c(200,140,120,110,80)
)

numeric.predictors <- select(predictors,-name)
numeric.predictors$gender <- as.integer(numeric.predictors$gender)

x <- unname(as.matrix(numeric.predictors))
xtxi <- solve(t(x) %*% x)

betas <- xtxi %*% t(x) %*% targets$weight

predicted <- x %*% betas
results <- data.frame(
    pred=predicted,
    error=unname(predicted-targets)
)
rm(predicted)

p <- ggplot(cbind(predictors, targets, results),aes(x=age,y=weight))
p <- p + facet_grid(gender ~ .) + 
    geom_text(aes(label=name), col="blue", size=3, vjust=-.7) +
    geom_point() +
    geom_abline(intercept=betas[1],slope=betas[2])
if (do.plots) print(p)
