library(shiny)
library(ggplot2)

#Loading data and model
#data <- read.csv()
#model <- 


ui = fluidPage(
    titlePanel(h1(id = "title","Get your hotel stars right now!",align = "center")),
    tags$style(HTML("#title{font-size: 50px;font-family: Georgia;}")),
    hr(),
    fluidRow(column(4, h2("Input")), column(4, h2("Output"))),
    sidebarLayout(
      sidebarPanel(
        p("Please provide the following information and then click Calculate Button:"),
        #Four Category below
        h4("Facillty"),
        helpText(),
        fluidRow(),
        
        h4("Location"),
        helpText(),
        fluidRow(),
        
        h4("Atmosphere"),
        helpText(),
        fluidRow(),
        
        h4("Service"),
        helpText(),
        fluidRow(),
        
        actionButton("calculate_botton", "Calculate", style = "color: white; background-color: #4040ff" ),
        width=4
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Prediction"),
          tabPanel("Plot"),
          #The plot should be wordcloud based on python
          
          tabPanel("Contact us")
        )
      )
    )
)