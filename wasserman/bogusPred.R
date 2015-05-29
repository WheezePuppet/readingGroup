
require(dplyr)

# Add a bogus predictor. Does it help? (Hopefully not!)

N.PTS <- 50
N.ITER <- 500

generate.rsss <- function(x.transformer) {
    RSSs <- numeric(N.ITER)
    for (iter in 1:N.ITER) {
        x <- as.data.frame(cbind(
            x1=rnorm(N.PTS,0,1),
            x2=rnorm(N.PTS,1,3),
            bogus=rnorm(N.PTS,0,2),
            x3=runif(N.PTS,-1,1)
        ))

        true.y <- x$x1 * 2.5 + x$x2 * 1.6 + x$x3 * 2.7 
        measured.y <- true.y + rnorm(N.PTS,0,.5)

        x <- x.transformer(x)
        xtxi <- solve(t(x) %*% x)
        hat.m <- x %*% xtxi %*% t(x)

        y.hat <- hat.m %*% measured.y
        RSSs[iter] <- sum((y-y.hat)^2)
    }
    RSSs
}


rsss.with.all.good.preds <- generate.rsss(function(x) 
    as.matrix(select(x,-bogus)))
attr(rsss.with.all.good.preds,"text") <- "all good preds"

rsss.adding.bad.pred <- generate.rsss(as.matrix)
attr(rsss.adding.bad.pred,"text") <- "with bad pred included"

rsss.missing.good.pred <- generate.rsss(function(x) 
    as.matrix(select(x,-bogus,-x2)))
attr(rsss.missing.good.pred,"text") <- "missing one good pred"

rsss.bad.not.good <- generate.rsss(function(x) as.matrix(select(x,-x2)))
attr(rsss.bad.not.good,"text") <- "worst of both worlds"

print.diff <- function(vec.1, vec.2) {
    test.results <- t.test(vec.1, vec.2)
    conf.int <- round(test.results$conf.int,0)
    verdict <- ifelse(prod(conf.int) < 0, "NO", "YES")
    cat("'",attr(vec.1,"text"),"' diff from '",attr(vec.2,"text"),"'? ",
        verdict," (", conf.int[1],",",conf.int[2],")\n",sep="")
}
cat("RSS with all good preds:",mean(rsss.with.all.good.preds),"\n")
cat("RSS with bad pred included:",mean(rsss.adding.bad.pred),"\n")
cat("RSS missing one good pred:",mean(rsss.missing.good.pred),"\n")
cat("RSS worst of both worlds:",mean(rsss.bad.not.good),"\n")

print.diff(rsss.with.all.good.preds, rsss.adding.bad.pred)
print.diff(rsss.with.all.good.preds, rsss.missing.good.pred)
print.diff(rsss.adding.bad.pred, rsss.bad.not.good)
