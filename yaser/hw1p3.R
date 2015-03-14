
bag.1 <- c("black","black")
bag.2 <- c("black","white")

for (i in 1:num.trials) {
    choose.bag <- sample(1:2,1)
    if (choose.bag == 1) {
        choose.ball <- sample(bag.1, 1)
        if (choose.ball == "white") {
            cat("Nm.\n")
        } else {

        }
    } else {

    }
}
