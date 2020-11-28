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
            index = vector(mode = "numeric")
    for (i in 1:length(input$variable)) {
        if(input$variable[i] == attribute[1]){
            index = c(index,1)
        } else if(input$variable[i] == attribute[2]){
            index = c(index,2)
        } else if(input$variable[i] == attribute[3]){
            index = c(index,3)
        } else if(input$variable[i] == attribute[4]){
            index = c(index,4)
        } else if(input$variable[i] == attribute[5]){
            index = c(index,5)
        } else if(input$variable[i] == attribute[6]){
            index = c(index,6)
        }
    }
    idx = c(T,T,T,T,T,T)
    idx[index] = F
        if (idx[1] == T) {
            paste("If you", "<font color=\"#FF0000\"><b>", "have Parking lot", "</b></font>", ", your star will increase by",999, ".")
        }
    })
    output$att2 = renderText({
        index = vector(mode = "numeric")
        for (i in 1:length(input$variable)) {
            if(input$variable[i] == attribute[1]){
                index = c(index,1)
            } else if(input$variable[i] == attribute[2]){
                index = c(index,2)
            } else if(input$variable[i] == attribute[3]){
                index = c(index,3)
            } else if(input$variable[i] == attribute[4]){
                index = c(index,4)
            } else if(input$variable[i] == attribute[5]){
                index = c(index,5)
            } else if(input$variable[i] == attribute[6]){
                index = c(index,6)
            }
        }
        idx = c(T,T,T,T,T,T)
        idx[index] = F
        if (idx[2] == T) {
            paste("If you", "<font color=\"#FF0000\"><b>", "have Free WiFi", "</b></font>", ", your star will increase by",999, ".")
        }
    })
    output$att3 = renderText({
        index = vector(mode = "numeric")
        for (i in 1:length(input$variable)) {
            if(input$variable[i] == attribute[1]){
                index = c(index,1)
            } else if(input$variable[i] == attribute[2]){
                index = c(index,2)
            } else if(input$variable[i] == attribute[3]){
                index = c(index,3)
            } else if(input$variable[i] == attribute[4]){
                index = c(index,4)
            } else if(input$variable[i] == attribute[5]){
                index = c(index,5)
            } else if(input$variable[i] == attribute[6]){
                index = c(index,6)
            }
        }
        idx = c(T,T,T,T,T,T)
        idx[index] = F
        if (idx[3] == T) {
            paste("If you", "<font color=\"#FF0000\"><b>", "accept Reservation", "</b></font>", ", your star will increase by",999, ".")
        }
    })
    output$att4 = renderText({
        index = vector(mode = "numeric")
        for (i in 1:length(input$variable)) {
            if(input$variable[i] == attribute[1]){
                index = c(index,1)
            } else if(input$variable[i] == attribute[2]){
                index = c(index,2)
            } else if(input$variable[i] == attribute[3]){
                index = c(index,3)
            } else if(input$variable[i] == attribute[4]){
                index = c(index,4)
            } else if(input$variable[i] == attribute[5]){
                index = c(index,5)
            } else if(input$variable[i] == attribute[6]){
                index = c(index,6)
            }
        }
        idx = c(T,T,T,T,T,T)
        idx[index] = F
        if (idx[4] == T) {
            paste("If you", "<font color=\"#FF0000\"><b>", "have Alcohol", "</b></font>", ", your star will increase by",999, ".")
        }
    })
    output$att5 = renderText({
        index = vector(mode = "numeric")
        for (i in 1:length(input$variable)) {
            if(input$variable[i] == attribute[1]){
                index = c(index,1)
            } else if(input$variable[i] == attribute[2]){
                index = c(index,2)
            } else if(input$variable[i] == attribute[3]){
                index = c(index,3)
            } else if(input$variable[i] == attribute[4]){
                index = c(index,4)
            } else if(input$variable[i] == attribute[5]){
                index = c(index,5)
            } else if(input$variable[i] == attribute[6]){
                index = c(index,6)
            }
        }
        idx = c(T,T,T,T,T,T)
        idx[index] = F
        if (idx[5] == T) {
            paste("If you", "<font color=\"#FF0000\"><b>", "accept Wheelchair", "</b></font>", ", your star will increase by",999, ".")
        }
    })
    output$att6 = renderText({
        index = vector(mode = "numeric")
        for (i in 1:length(input$variable)) {
            if(input$variable[i] == attribute[1]){
                index = c(index,1)
            } else if(input$variable[i] == attribute[2]){
                index = c(index,2)
            } else if(input$variable[i] == attribute[3]){
                index = c(index,3)
            } else if(input$variable[i] == attribute[4]){
                index = c(index,4)
            } else if(input$variable[i] == attribute[5]){
                index = c(index,5)
            } else if(input$variable[i] == attribute[6]){
                index = c(index,6)
            }
        }
        idx = c(T,T,T,T,T,T)
        idx[index] = F
        if (idx[6] == T) {
            paste("If you", "<font color=\"#FF0000\"><b>", "accept Credit cards", "</b></font>", ", your star will increase by",999, ".")
        }
    })

})
