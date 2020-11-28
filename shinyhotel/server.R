#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

dat <- read.csv("wordembedding.csv",header=TRUE)

shinyServer(function(input, output) {
    
    avg.hotel <- sort(tapply(as.numeric(dat$stars),dat$name,mean))
    
    part.dat <- reactive({dat[which(dat$name==input$Hotel_name),]})
    part.words <- reactive({apply(part.dat[,4:dim(part.dat())[2]],2,sum)})
    part.freq <- reactive({names(sort(part.words(), decreasing= TRUE)[1:30])})
    
    service.avg <- round(mean(as.numeric(dat$stars[which(dat[,"booked"]==1|dat[,"money"]==1|dat[,"staff"]==1|dat[,"manager"]==1|dat[,"breakfast"]==1|dat[,"desk"]==1)])),1)
    facility.avg <- round(mean(as.numeric(dat$stars[which(dat[,"wall"]==1|dat[,"parking"]==1)])),1)
    location.avg <- round(mean(as.numeric(dat$stars[which(dat[,"bar"]==1|dat[,"downtown"]==1|dat[,"restaurant"]==1|dat[,"location"]==1)])),1)
    atmosphere.avg <- round(mean(as.numeric(dat$stars[which(dat[,"clean"]==1|dat[,"quiet"]==1|dat[,"comfortable"]==1|dat[,"spacious"]==1|dat[,"quiet"]==1|dat[,"smell"]==1|dat[,"modern"]==1|dat[,"pretty"]==1|dat[,"comfy"]==1|dat[,"dirty"]==1)])),1)

    service.part.avg <- reactive({round(mean(as.numeric(part.dat()$stars[which(part.dat()[,"booked"]==1|part.dat()[,"money"]==1|part.dat()[,"staff"]==1|part.dat()[,"manager"]==1|part.dat()[,"breakfast"]==1|part.dat()[,"desk"]==1)])),1)})
    facility.part.avg <- reactive({round(mean(as.numeric(part.dat()$stars[which(part.dat()[,"wall"]==1|part.dat()[,"parking"]==1)])),1)})
    location.part.avg <- reactive({round(mean(as.numeric(part.dat()$stars[which(part.dat()[,"bar"]==1|part.dat()[,"downtown"]==1|part.dat()[,"restaurant"]==1|part.dat()[,"location"]==1)])),1)})
    atmosphere.part.avg <- reactive({round(mean(as.numeric(part.dat()$stars[which(part.dat()[,"clean"]==1|part.dat()[,"quiet"]==1|part.dat()[,"comfortable"]==1|part.dat()[,"spacious"]==1|part.dat()[,"quiet"]==1|part.dat()[,"smell"]==1|part.dat()[,"modern"]==1|part.dat()[,"pretty"]==1|part.dat()[,"comfy"]==1|part.dat()[,"dirty"]==1)])),1)})

    freq.words <- reactive({names(sort(part.words, decreasing= TRUE)[1:30])})
    
    output$Overall <- renderText({   
        paste(input$Hotel_name,"got",length(which(dat$name==input$Hotel_name)),"reviews from the customers")
        if (round(avg.hotel[input$Hotel_name],1)<round(mean(avg.hotel),1)){
            paste(input$Hotel_name,"obtained",round(avg.hotel[input$Hotel_name],1),"average rate which is below the average rate(",round(mean(avg.hotel),1),")of hotels in Madison Area")
        }else if (round(avg.hotel[input$Hotel_name],1)==round(mean(avg.hotel),1)){
            paste(input$Hotel_name,"obtained",round(avg.hotel[input$Hotel_name],1),"average rate which is the same as average rate(",round(mean(avg.hotel),1),")of hotels in Madison Area")
        }else{
            paste(input$Hotel_name,"obtained",round(avg.hotel[input$Hotel_name],1),"average rate which is above the average rate(",round(mean(avg.hotel),1),") of hotels in Madison Area")
        }
        #paste("Most frequently appeared 30 words in reviews for",input$Hotel_name,"are")
    })

    output$barplot <- renderPlot({
        barplot(table(dat$stars[which(dat$name==input$Hotel_name)]), xlab="Stars",ylab="Number of Review", main=input$Hotel_name)
    })
    
    reactive({if(length(which(dat$name==input$Hotel_name))>5){
        output$Service <- renderText({
            if(service.part.avg()>service.avg){
                paste(input$Hotel_name,"obtained",service.part.avg(),"star rate for service which is above the average rate(",service.avg,")of hotels in Madison Area")
            }else if(service.part.avg()==service.avg){
                paste(input$Hotel_name,"obtained",service.part.avg(),"star rate for service which is the same as the average rate(",service.avg,")of hotels in Madison Area")
            }else{
                paste(input$Hotel_name,"obtained",service.part.avg(),"star rate for service which is below the average rate(",service.avg,")of hotels in Madison Area")
            }
        })
        output$Facility <- renderText({
            if(facility.part.avg()>facility.avg){
                paste(input$Hotel_name,"obtained",facility.part.avg(),"star rate for facility which is above the average rate(",facility.avg,")of hotels in Madison Area")
            }else if(facility.part.avg() == facility.avg){
                paste(input$Hotel_name,"obtained",facility.part.avg(),"star rate for facility which is the same as the average rate(",facility.avg,")of hotels in Madison Area")
            }else {
                paste(input$Hotel_name,"obtained",facility.part.avg(),"star rate for facility which is below the average rate(",facility.avg,")of hotels in Madison Area")
            }
        })
        output$Location <- renderText({
            if(location.part.avg()>location.avg){
                paste(input$Hotel_name,"obtained",location.part.avg(),"star rate for location which is above the average rate(",location.avg,")of hotels in Madison Area")
            }else if(location.part.avg() == location.avg){
                paste(input$Hotel_name,"obtained",location.part.avg(),"star rate for location which is the same as the average rate(",location.avg,")of hotels in Madison Area")
            }else{
                paste(input$Hotel_name,"obtained",location.part.avg(),"star rate for location which is below the average rate(",location.avg,")of hotels in Madison Area")
            }
        })
        output$Atmosphere <- renderText({
            if(atmosphere.part.avg()>atmosphere.avg){
                paste(input$Hotel_name,"obtained",atmosphere.part.avg(),"star rate for atmosphere which is above the average rate(",atmosphere.avg,")of hotels in Madison Area")
            }else if(atmosphere.part.avg() == atmosphere.avg){
                paste(input$Hotel_name,"obtained",atmosphere.part.avg(),"star rate for atmosphere which is the same as the average rate(",atmosphere.avg,")of hotels in Madison Area")
            }else{
                paste(input$Hotel_name,"obtained",atmosphere.part.avg(),"star rate for atmosphere which is below the average rate(",atmosphere.avg,")of hotels in Madison Area")
            }
        })
    }else{
        
        output$Service <- renderText({
            paste("Not enough reviews from customers")
        })
        output$Facility <- renderText({
            paste("Not enough reviews from customers")
        })
        output$Location <- renderText({
            paste("Not enough reviews from customers")
        })
        output$Atmosphere <- renderText({
            paste("Not enough reviews from customers")
        })
    }})
    
    
    output$Contact <- renderUI({
        HTML('<br>
           Further imformation can be obtained from: <br> 
          <br>
          Hangyu Kang hkang98@wisc.edu <br> 
          <br>
          Xiangyu Wang xwang2439@wisc.edu <br>
          <br>
          Ruyan Zhou rzhou84@wisc.edu <br>
          <br>
         ')
    })

})


