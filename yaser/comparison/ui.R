

library(shiny)

shinyUI(fluidPage(

    tags$head(tags$link(rel="stylesheet", type="text/css", href="shiny.css")),

    titlePanel("Classifier comparison"),

    sidebarLayout(sidebarPanel(
        sliderInput("npts","Number of points",min=10,max=4000,value=2000),
        sliderInput("prior","Prior (class 1)",min=0,max=1,value=.5,step=.05),
        sliderInput("cov1","Covariance for class 1",
            min=-1,max=1,value=0,step=.05),
        sliderInput("covn1","Covariance for class -1",
            min=-1,max=1,value=0,step=.05),
        fluidRow(
            column(width=6,
                numericInput("classn1meanx1","Mean x1 for class -1",1)
            ),
            column(width=6,
                numericInput("classn1meanx2","Mean x2 for class -1",1)
            )
        ),
        sliderInput("gaussianness","\"Gaussianness\"",
            min=0,max=1,value=1,step=.05),
        actionButton("runit",label="Run"),
        width=3
        ),

        mainPanel(
            fluidRow(
                column(width=7,
                    plotOutput("mainPlot",height="500px")
                ),
                column(width=5,
                    plotOutput("performancePlot",height="500px")
                )
            )
        )
    )
))
