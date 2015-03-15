

library(shiny)

shinyUI(fluidPage(

    tags$head(tags$link(rel="stylesheet", type="text/css", href="shiny.css")),

    titlePanel("Perceptrondawg"),

    sidebarLayout(sidebarPanel(
        sliderInput("npts","Number of points",min=10,max=200,value=40),
        actionButton("runit",label="Run")
        ),

        mainPanel(
            tabsetPanel(
                tabPanel("Points",
                    plotOutput("mainPlot",height="600px")
                ),
                tabPanel("Weights History",
                    plotOutput("historyPlot",height="600px")
                ),
                tabPanel("Convergence times",
                    HTML("<div style=\"margin-top:2em;margin-bottom:3em;font-size:large;\">This is for <a href=\"https://work.caltech.edu/homework/hw1.pdf\">homework problems 7-10</a>."),
                    sliderInput("ntrials","Number of trials",min=0,max=1000,
                        value=100),
                    plotOutput("convergencePlot",height="400px")
                )
            )
        )
    )
))
