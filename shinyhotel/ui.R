library(shiny)
library(ggplot2)

ui = fluidPage(
    titlePanel(h1(id = "title","Hotel Analysis based on Yelp ",align = "center")),
    tags$style(HTML("#title{font-size: 50px;font-family: Georgia;}")),
    hr(),
    fluidRow(column(4, h2("Input")), column(4, h2("Output"))),
    sidebarLayout(
      sidebarPanel(
        p("Please select your hotel below:"),
        fluidRow(column(12,style=list("padding-left: 5px;"),
                        selectInput("Hotel_name",label="Hotel Name List",
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
        fluidRow(column(6,style=list("padding-left: 5px;"),checkboxGroupInput("variable1", "Service",c("Accept Reservation","Proper Price","Good Staff","Decent Manager","Provide Breakfast","Have Desk"))),
                 column(6,style=list("padding-left: 5px;"),checkboxGroupInput("variable2", "Facility",c("Nice Wall","Parking Lot Available")))),
        fluidRow(column(6,style=list("padding-left: 5px;"),checkboxGroupInput("variable3", "Location",c("Near Bar","Downtown","Near Restaurant","Nice Location"))),
                 column(6,style=list("padding-left: 5px;"),checkboxGroupInput("variable4", "Atmosphere",c("Clean","Comfortable","Spacious","Quiet","Smell good","Modern","Pretty","Comfy","Dirty")))),
        
        
        actionButton("calculate_botton", "Calculate", style = "color: white; background-color: #4040ff" ),
        width=4
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Customers' Evaluation", h3("Overall:"),textOutput("Overall"),plotOutput(outputId="barplot"),h3("Service:"),textOutput("Service"),h3("Facility"),textOutput("Facility"),h3("Location"),textOutput("Location"),h3("Atmosphere"),textOutput("Atmosphere")),
          tabPanel("Plot"),
          #The plot should be wordcloud based on python
          
          tabPanel("Tips",
                   h3(htmlOutput("att1")),
                   h3(htmlOutput("att2")),
                   h3(htmlOutput("att3")),
                   h3(htmlOutput("att4")),
),
          tabPanel("Contact us")
        )
      )
    )
)

