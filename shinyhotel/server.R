#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

#Create variables
attribute =  c("Parking lot",
                   "Free WiFi",
                   "Reservation",
                   "Alcohol",
                   "Wheelchair",
                   "Creditcard")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$att1 = renderText({
        if(length(input$variable1) != 0){
            
        } else {
            paste("<font color=\"#FF0000\"><b>", "Warning: ", "</b></font>", "Please select from checkbox right side.")
        }
    })
    output$att2 = renderText({
        if(length(input$variable2) != 0){
            
        } else {
            paste("<font color=\"#FF0000\"><b>", "Warning: ", "</b></font>", "Please select from checkbox right side.")
        }
    })
    output$att3 = renderText({
        if(length(input$variable3) != 0){
            
        } else {
            paste("<font color=\"#FF0000\"><b>", "Warning: ", "</b></font>", "Please select from checkbox right side.")
        }
    })
    output$att4 = renderText({
        if(length(input$variable4) != 0){
            
        } else {
            paste("<font color=\"#FF0000\"><b>", "Warning: ", "</b></font>", "Please select from checkbox right side.")
        }
    })
})
