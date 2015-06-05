

library(shiny)

shinyUI(fluidPage(

    tags$head(tags$link(rel="stylesheet", type="text/css", href="shiny.css")),

    titlePanel("Classifier comparison"),

    sidebarLayout(sidebarPanel(
        sliderInput("npts","Number of points",min=10,max=4000,value=2000),
        sliderInput("prior","Prior (class 1)",min=0,max=1,value=.5,step=.05),

        h4("Mean for class -1:"),
        fluidRow(
            column(width=4,
                numericInput("classn1meanx1","μ₁",1)
            ),
            column(width=4,
                numericInput("classn1meanx2","μ₂",1)
            )
        ),

        h4("Covariance for class -1:"),
        fluidRow(
            column(width=4,
                numericInput("classn1var1","σ₁²",1,step=.1)
            ),
            column(width=4,
                numericInput("classn1covar12","σ₁₂",0,step=.1)
            ),
            column(width=4,
                numericInput("classn1var2","σ₂²",1,step=.1)
            )
        ),

        sliderInput("gaussianness","\"Gaussianness\"",
            min=0,max=1,value=1,step=.05),
        actionButton("runit",label="Run"),
        width=3
        ),

        mainPanel(
            tabsetPanel(
                tabPanel("Performance",
                    fluidRow(
                        column(width=7,
                            plotOutput("mainPlot",height="500px")
                        ),
                        column(width=5,
                            plotOutput("performancePlot",height="500px")
                        )
                    )
                ),
                tabPanel("Posteriors",
                    fluidRow(
                        column(width=6,
                            plotOutput("posteriorLDALogPlot",height="500px")
                        ),
                        column(width=6,
                            plotOutput("posteriorLDAQDAPlot",height="500px")
                        )
                    )
                )
            )
        )
    )
))
