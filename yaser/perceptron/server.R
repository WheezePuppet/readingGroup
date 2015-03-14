
library(shiny)


shinyServer(function(input,output,session) {

    source("hw1perceptron.R")

    output$mainPlot <- renderPlot({

        if (input$runit < 1) return(NULL)

        isolate({
            w.history <<- run.til.convergence(input$npts,plot=TRUE)
        })
    })
    
    output$historyPlot <- renderPlot({
        if (input$runit < 1) return(NULL)
        isolate({
            plot.w.history(w.history)
        })
    })

    output$convergencePlot <- renderPlot({
        if (input$runit < 1) return(NULL)
        isolate({
            measure.convergence.times(input$ntrials, input$npts, plot=TRUE)
        })
    })

})
