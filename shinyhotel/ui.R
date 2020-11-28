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
        p("Please select your hotel below:"),
        fluidRow(column(12,style=list("padding-left: 5px;"),
                        selectInput("Hotel_name",label="Select your Hotel!",
                                    choices=c('Budget Host Aloha Inn Motel',
                                              'Rodeway Inn & Suites WI Madison-Northeast',
                                              'Super 8 by Wyndham Madison East',
                                              'Americas Best Value Inn Madison',
                                              'Baymont Inn And Suites Madison',
                                              'Red Roof Inn Madison, WI',
                                              'Magnuson Grand Hotel Madison',
                                              'Howard Johnson Plaza Hotel by Wyndham Madison',
                                              'Crowne Plaza Hotel - Madison',
                                              'Candlewood Suites Madison - Fitchburg',
                                              'Microtel Inn & Suites by Wyndham Madison East',
                                              'Baymont by Wyndham Madison West/Middleton WI West',
                                              'Comfort Inn & Suites Madison - Airport',
                                              'Holiday Inn Hotel & Suites Madison West',
                                              'Econo Lodge',
                                              'Holiday Inn Express',
                                              'Days Inn & Suites by Wyndham Madison',
                                              'Quality Inn & Suites',
                                              'University Inn Madison',
                                              'Residence Inn Madison East',
                                              'Radisson Hotel - Madison',
                                              'AC Hotel by Marriott Madison Downtown',
                                              'La Quinta by Wyndham Madison American Center',
                                              'Clarion Suites at the Alliant Energy Center',
                                              'AmericInn by Wyndham Madison West',
                                              'Comfort Inn Madison - Downtown',
                                              'Holiday Inn Madison at The American Center',
                                              'Fairfield Inn & Suites by Marriott Madison East',
                                              'Hyatt Place Madison Downtown',
                                              'Cambria Hotel Madison East',
                                              'Staybridge Suites Madison-East',
                                              'Homewood Suites by Hilton Madison West',
                                              'The Madison Concourse Hotel and Governor\'s Club',
                                              'Hampton Inn Madison East Towne Mall Area',
                                              'Sleep Inn & Suites',
                                              'DoubleTree by Hilton Hotel Madison',
                                              'Super 8 by Wyndham Madison South',
                                              'Holiday Inn Express & Suites Madison',
                                              'Hotel Ruby Marie',
                                              'Tru by Hilton Madison West',
                                              'Hampton Inn & Suites Madison-West',
                                              'Hampton Inn & Suites Madison / Downtown',
                                              'Holiday Inn Express & Suites Madison Central',
                                              'SpringHill Suites Madison',
                                              'Hotel Indigo Madison Downtown'), selected='AC Hotel by Marriott Madison Downtown', multiple=F)
        )),
        p("Please provide the following information and then click Calculate Button:"),
        checkboxGroupInput("variable", "Which service available?",
                           c("Parking lot",
                             "Free WiFi",
                             "Reservation",
                             "Alcohol",
                             "Wheelchair",
                             "Creditcard")),
        
        actionButton("calculate_botton", "Calculate", style = "color: white; background-color: #4040ff" ),
        width=4
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Prediction"),
          tabPanel("Plot"),
          #The plot should be wordcloud based on python
          
          tabPanel("Tips",
                   h3(htmlOutput("att1")),
                   h3(htmlOutput("att2")),
                   h3(htmlOutput("att3")),
                   h3(htmlOutput("att4")),
                   h3(htmlOutput("att5")),
                   h3(htmlOutput("att6"))),
          tabPanel("Contact us")
        )
      )
    )
)