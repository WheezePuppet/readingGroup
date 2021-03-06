require(ISLR)
require(MASS)
data(Boston)
options(width=190)
Boston <- tbl_df(Boston)
confint(lm.fit)
predict(lm.fit,data.frame(lstat=5),interval="confidence")
confint(lm.fit)
confint(lm.fit,level=.6)
ggplot(lm.fit,aes(lstat,medv)) + geom_point() + geom_smooth(method="lm")
ggplot(data.frame(fit=predict(lm.fit),res=rstudent(lm.fit)),aes(fit,res)) + geom_point()
hatvalues(lm.fit)
ggplot(data.frame(hv=hatvalues(lm.fit),val=Boston$lstat),aes(val,hv)) + geom_point()
lm.fit <- lm(medv~poly(lstat,10), data=Boston)
summary(lm.fit)
tp <- ggplot(data.frame(dplyr::select(Boston,lstat,medv),fit=fitted(lm.fit)),aes(lstat,medv))
tp + geom_point() + geom_line(aes(y=fit),col="blue")
