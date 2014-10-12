
# Linear discriminant analysis (ISL ch.4)
# Stephen - 10/11/14

library(ggplot2)

class.1.true.mean <- 12*5+9   # men's height in inches
class.2.true.mean <- 12*5+4   # women's height in inches
common.true.sd <- 6
class.1.true.n <- 6
class.2.true.n <- 9

sample.class.1 <- rnorm(class.1.true.n,class.1.true.mean, common.true.sd)
sample.class.2 <- rnorm(class.2.true.n,class.2.true.mean, common.true.sd)

data <- data.frame(
    rbind(
        cbind(sample.class.1,1),
        cbind(sample.class.2,2)
    )
)
names(data) <- c("pred","true.class")
data$true.class <- as.factor(data$true.class)

the.plot <- ggplot(data,aes(true.class,pred)) + geom_boxplot() +
    scale_y_continuous(limits=c(0,100))
print(the.plot)
