
library(ISLR)
require(splines)
require(doParallel)
registerDoParallel(8)


# The ggplot way. 4th-order polynomial, step functions, splines, whatevs.
pts.plot <- ggplot(Wage, aes(x=age,y=wage)) + geom_point(pch=20)

formulas <- list("4th-order polynomial"=y~poly(x,4),
    "step function"=y~cut(x,5),
    "cubic spline function (knots=25,40,60)"=y~bs(x,knots=c(25,40,60)),
    "cubic spline function (unspecified knots)"=y~bs(x),
    "natural spline function (unspecified knots)"=y~ns(x,df=4),
    "5th-order spline function (unspecified knots)"=y~bs(x,degree=5),
    "2nd-order spline function (unspecified knots)"=y~bs(x,degree=2),
    "1st-order spline function (unspecified knots)"=y~bs(x,degree=1)
)

lapply(names(formulas), function(name) {
    print(ggplot(Wage, aes(y=wage,x=age)) + geom_point(shape=20) +
        stat_smooth(method="lm", formula=as.formula(formulas[[name]])) +
        ggtitle(name))
    readline(paste0("(",name,"). Press ENTER."))
})
    
# Not sure how to use smooth.spline() with ggplot?
# See: https://groups.google.com/forum/#!topic/ggplot2/FJ36CJH-ODo
#print(ggplot(Wage, aes(y=wage,x=age)) + geom_point(shape=20) +
#    stat_smooth(method="smooth.spline") +
#    ggtitle("smoothing spline"))
#readline("(smoothing spline.) Press ENTER.")



# Now let's compare 1st through 5th degree polynomials.
fits <- lapply(1:5,
    function(deg) {
        lm(wage~poly(age,deg),data=Wage)
    }
)

anova(fits[[1]],fits[[2]],fits[[3]],fits[[4]],fits[[5]])
# (turns out no justification for going to 4th or higher. Cubic is fine.)



# k-fold CV
k <- 10
max.deg <- 8
num.per.fold <- nrow(Wage)/k
folds <- sample(rep(1:k,each=num.per.fold))

avg.MSEs <- foreach(degree=1:max.deg, .combine=c) %do% {
    MSEs <- foreach(fold=1:k, .combine=c) %do% {
        model <- lm(wage~poly(age,degree),data=Wage[folds!=fold,])
        preds <- predict(model,newdata=list(age=Wage[folds==fold,"age"]))
        MSE <- mean((preds - Wage[folds==fold,"wage"])^2)
        MSE
    }
    mean(MSEs)
}
names(avg.MSEs) <- paste0("deg-",1:max.deg)
print(avg.MSEs)
