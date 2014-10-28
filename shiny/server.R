
library(shiny)


shinyServer(function(input,output,session) {

    source("shinySDCGhw1015.R")

    shinySDCGhw1015.server(input,output,session)
})
