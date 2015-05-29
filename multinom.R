
library(ggplot2)
# (rmultinom is in stats package.)

NUM.SAMPLES <- 1e4
NUM.COLORS <- 5
NUM.BALLS.DRAWN <- 12

probs <- runif(NUM.COLORS,0,1)

# Make prob of color #4 arbitrarily big, to see effect.
probs[4] <- 2    

# (Normalize to sum to 1, even though rmultinom does it.)
probs <- probs / sum(probs)  

data.frame(t(rmultinom(NUM.SAMPLES, NUM.BALLS.DRAWN, probs))) -> the.data

# If you want, plot the # of balls of the first two colors against each other.
p <- ggplot(the.data,aes(x=X1,y=X2)) + 
    geom_jitter() +
    scale_x_discrete(breaks=1:NUM.BALLS.DRAWN,
        labels=paste0("",1:NUM.BALLS.DRAWN)) +
    scale_y_discrete(breaks=1:NUM.BALLS.DRAWN,
        labels=paste0("",1:NUM.BALLS.DRAWN)) 

covar.mat <- cov(the.data)

lapply(1:NUM.COLORS, function(color1.num) {
        lapply(1:NUM.COLORS, function(color2.num) {
                if (color1.num == color2.num) {
                    cat("var(",color1.num,") = ",
                        covar.mat[color1.num,color1.num],
                        "; theoretical = ",
                                NUM.BALLS.DRAWN * 
                                probs[color1.num] *
                                (1 - probs[color1.num]),
                        "\n", sep="")
                } else if (color1.num < color2.num) {
                    cat("covar(",color1.num,",",color2.num,") = ",
                        covar.mat[color1.num,color2.num],
                        "; theoretical = ",
                                -NUM.BALLS.DRAWN *
                                probs[color1.num] *
                                probs[color2.num],
                        "\n", sep="")
                }
            }
        )
    }
)
