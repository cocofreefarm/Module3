library(shiny)
library(ggplot2)
library(wordcloud2)

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
        
        
        p("Click and go to Details tab to see the details of :"),
        fluidRow(column(6,style=list("padding-left: 5px;"),radioButtons("variable1", "Categories", c("Service","Facility","Location","Atmosphere"))))
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Customers' Evaluation", 
                   h3("Overall:"),
                   textOutput("Overall1"),
                   textOutput("Overall2"),
                   plotOutput(outputId="barplot"),
                   h3("Service:"),textOutput("Service"),h3("Facility:"),textOutput("Facility"),h3("Location:"),textOutput("Location"),h3("Atmosphere:"),textOutput("Atmosphere")),
          tabPanel("Word Clouds for the selected hotel",
                   wordcloud2Output(outputId="wordcloud",height = "400px",width = "400px"),
                   ),
          tabPanel("Details",
                   textOutput("Details"), tags$style("#Details {font-size:26px;}"),
                   plotOutput("boxplots")
                   
                   
          ),
          #The plot should be wordcloud based on python
          
          tabPanel("Tips",
                   h2("*Service:"),
                   h4("Reservation:"),
                   textOutput("book"),
                   h4("Price:"),
                   textOutput("price"),
                   h4("Employee:"),
                   textOutput("employee"),
                   h4("Breakfast:"),
                   textOutput("breakf"),
                   h4("Desk:"),
                   textOutput("desk"),
                   h2("*Facility:"),
                   h4("Wall:"),
                   textOutput("wall"),
                   h4("Parking:"),
                   textOutput("parking"),
                   h2("*Location:"),
                   h4("Location:"),
                   textOutput("location"),
                   h4("Restaurant:"),
                   textOutput("restaurant"),
                   h4("Bar:"),
                   textOutput("bar"),
                   h2("*Atmosphere:"),
                   h4("Cleanliness:"),
                   textOutput("clean"),
                   h4("Comfortable:"),
                   textOutput("comfortable"),
                   h4("Spacious:"),
                   textOutput("spacious"),
                   h4("Smell:"),
                   textOutput("smell"),
                   h4("Modern:"),
                   textOutput("modern"),
                   h4("Pretty:"),
                   textOutput("pretty"),
                   h4("Dirty:"),
                   textOutput("dirty")
                   
),
          tabPanel("Contact us",h3("Contact:"),htmlOutput("Contact"))
        )
      )
    )
)

