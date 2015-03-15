
library(shiny)


shinyServer(function(input,output,session) {

source("hw1perceptron.R")
    output$mainPlot <- renderPlot({

        if (input$runit < 1) return(NULL)

        isolate({
            run.til.convergence(input$npts,plot=TRUE)
        })
    })
    
})
