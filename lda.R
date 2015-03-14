
# Linear disc analysis (ISL ch.4)
# Stephen - 10/11/14

library(ggplot2)

class.1.true.mean <- 12*5+9   # men's height in inches
class.2.true.mean <- 12*5+4   # women's height in inches
common.true.sd <- 6
class.1.true.n <- 6
class.2.true.n <- 9

# Are we allowed to see this??
prior.prob.class.1 <- class.1.true.n / (class.1.true.n + class.2.true.n)
prior.prob.class.2 <- class.2.true.n / (class.1.true.n + class.2.true.n)

training.class.1 <- rnorm(class.1.true.n,class.1.true.mean, common.true.sd)
training.class.2 <- rnorm(class.2.true.n,class.2.true.mean, common.true.sd)
training.points <- c(training.class.1,training.class.2)

data <- data.frame(
    rbind(
        cbind(training.class.1,1),
        cbind(training.class.2,2)
    )
)
names(data) <- c("pred","true.class")
data$true.class <- as.factor(data$true.class)

raw.data.plot <- ggplot(data,aes(true.class,pred)) + geom_boxplot() +
    scale_y_continuous(limits=c(0,100))
print(raw.data.plot)
cat("Press ENTER.")
readline()

class.1.sample.mean <- mean(training.class.1)
class.2.sample.mean <- mean(training.class.2)
common.sample.sd <- sd(training.points)

class.1.disc.slope <- class.1.sample.mean / common.sample.sd^2
class.1.disc.intercept <- -class.1.sample.mean^2 / (2*common.sample.sd^2) +
    log(prior.prob.class.1)
class.2.disc.slope <- class.2.sample.mean / common.sample.sd^2
class.2.disc.intercept <- -class.2.sample.mean^2 / (2*common.sample.sd^2) +
    log(prior.prob.class.2)

plot.x.vals = seq(min(training.points),max(training.points),.1)
disc.line.plot <- ggplot(data.frame(x=plot.x.vals,
    y=class.1.disc.slope*plot.x.vals+class.1.disc.intercept))
print(plot.x.vals + geom_abline(class.1.disc.slope,class.1.disc.intercept))
    

