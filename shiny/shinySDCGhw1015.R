
library(ggplot2)

source("../hw1015.R")

shinySDCGhw1015.ui <- function() {

    tabsetPanel(
        tabPanel("k vs successRate",
            sidebarLayout(
                sidebarPanel(
                    h4("Data"),
                    fluidPage(
                    fluidRow(
                        column(4,numericInput("n","Num pts",value=2000)),
                        column(4,sliderInput("prop.f1","Prop. in class 1",
                            min=0,max=1,value=.5)),
                        column(4,sliderInput("train.prop","Training prop.",
                            min=0,max=1,value=.7))
                    )),
                    h4("Class 1"),
                    fluidPage(
                    fluidRow(
                        column(2,numericInput("f1.xmean","x mean",value=3)),
                        column(2,numericInput("f1.xstdev","x stdev",value=1)),
                        column(2,numericInput("f1.ymean","y mean",value=0)),
                        column(2,numericInput("f1.ystdev","y stdev",value=1)),
                        column(2,numericInput("f1.correl","correlation",
                            value=0, min=-1,max=1,step=.1))
                    )),
                    h4("Class 2"),
                    fluidPage(
                    fluidRow(
                        column(2,numericInput("f2.xmean","x mean",value=0)),
                        column(2,numericInput("f2.xstdev","x stdev",value=1)),
                        column(2,numericInput("f2.ymean","y mean",value=3)),
                        column(2,numericInput("f2.ystdev","y stdev",value=1)),
                        column(2,numericInput("f2.correl","correlation",
                            value=0, min=-1,max=1,step=.1))
                    )),
                    h4("Classifiers"),
                    sliderInput("kvals","k values",
                        min=1,max=201,value=c(5,51)),
                    actionButton("plotDataset","Plot dataset"),
                    actionButton("plotKVsSuccessRate","Plot k vs. rate")
                ),
                mainPanel(
                    plotOutput("dataset"),
                    plotOutput("kVsSuccessRate")
                )
            )
        ),
        tabPanel("classifier comparison",
            sidebarLayout(
                sidebarPanel(
                    numericInput("k1","k1",min=1,max=201,value=5),
                    numericInput("k2","k1",min=1,max=201,value=7)
                ),
                mainPanel(
                    plotOutput("classifierComparison")
                )
            )
        )
    )
}

shinySDCGhw1015.server <- function(input,output,session) {
    
    ds <- reactive({
        input$plotDataset 
        input$plotKVsSuccessRate
        isolate({
            build.dataset(input$f1.xmean, input$f1.ymean, 
                input$f2.xmean, input$f2.ymean, input$f1.xstdev, 
                input$f1.ystdev, input$f2.xstdev, input$f2.ystdev,
                correl.f1=input$f1.correl, correl.f2=input$f2.correl,
                n=input$n, train.proportion=input$train.prop,
                prop.class.1=input$prop.f1)
        })
    })

    output$dataset <- renderPlot({
        if (input$plotDataset >= 1) {
            isolate({
                print(get.dataset.plot(ds()) + ggtitle("Raw data") +
                    geom_hline(xintercept=0) + geom_vline(yintercept=0))
            })
        }
    })
    
    output$kVsSuccessRate <- renderPlot({
        if (input$plotKVsSuccessRate >= 1) {
            isolate({
                agg.plot <- get.knn.aggregate.results.plot(ds(),
                    seq(input$kvals[[1]],input$kvals[[2]],2))
                print(agg.plot + ggtitle("KNN: k vs. (test) success rate"))
            })
        }
    })
}
