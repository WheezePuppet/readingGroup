
library(shiny)


shinyServer(function(input,output,session) {

    source("comparison.R")

    output$mainPlot <- renderPlot({

        if (input$runit < 1) return(NULL)

        isolate({
            the.data.set <<- generate.data(input$npts, covar.1=input$cov1,
                covar.neg1=input$covn1, 
                mean.x1.neg1=input$classn1meanx1,
                mean.x2.neg1=input$classn1meanx2,
                prior=input$prior,
                gaussianness=input$gaussianness)
            p <- ggplot(the.data.set$training) + 
                geom_point(aes(x=x1, y=x2, color=y)) +
                xlim(-3,6) +
                ylim(-3,6) +
                ggtitle("Training points")
            print(p)
            
        })
    })

    output$performancePlot <- renderPlot({
        if (input$runit < 1) return(NULL)
        plot.data <- data.frame(alg=factor(c("LDA","QDA","Log reg"),
                levels=c("LDA","QDA","Log reg")),
            perf=c(lda.accuracy(the.data.set),qda.accuracy(the.data.set),
                                                log.reg.accuracy(the.data.set))
        )
        p <- ggplot(plot.data) + 
            geom_bar(aes(x=alg,fill=alg,y=perf),stat="identity") +
            ylim(0,1) + scale_fill_manual(values=c("green","blue","red")) +
            ggtitle("Classifier performance")
        print(p)
    })
})
