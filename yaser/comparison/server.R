
library(shiny)


shinyServer(function(input,output,session) {

    source("comparison.R")

    values <- reactiveValues(
        the.data.set=NULL
    )

    observeEvent(input$runit, {
        values$the.data.set <- generate.data(input$npts, 
            covar.neg1=matrix(c(
                input$classn1var1,
                input$classn1covar12,
                input$classn1covar12,
                input$classn1var2), nrow=2),
            mean.neg1=c(input$classn1meanx1,input$classn1meanx2),
            prior=input$prior,
            gaussianness=input$gaussianness)
    })

    output$mainPlot <- renderPlot({

        if (input$runit < 1) return(NULL)

        isolate({
            p <- ggplot(values$the.data.set$training) + 
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
            perf=sapply(list(run.lda(values$the.data.set),
                          run.qda(values$the.data.set),
                          run.log.reg(values$the.data.set)),
                function(thing) thing$accuracy)
        )
        p <- ggplot(plot.data) + 
            geom_bar(aes(x=alg,fill=alg,y=perf),stat="identity") +
            ylim(0,1) + scale_fill_manual(values=c("green","blue","red")) +
            ggtitle("Classifier performance")
        print(p)
    })

    output$posteriorPlot <- renderPlot({


    })

})
