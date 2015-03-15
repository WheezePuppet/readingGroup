

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
                )
            )
        )
    )
))
