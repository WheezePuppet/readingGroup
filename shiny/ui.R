
library(shiny)

source("../hw1015.R")
source("shinySDCGhw1015.R")

shinyUI(fluidPage(

    tags$head(tags$link(rel="stylesheet", type="text/css", href="shiny.css")),

    titlePanel("Reading Group apps"),

    tabsetPanel(
        tabPanel("SD/CG 10/15/14",
            shinySDCGhw1015.ui()
        )
    )
))
