#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(wordcloud)
library(wordcloud2)


dat <- read.csv("wordembedding.csv",header=TRUE)
avg.hotel.cloud <- sort(tapply(as.numeric(dat$stars),dat$name,mean))
hotel.names <- names(avg.hotel.cloud)

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
    service.part.len <- reactive({length(which(part.dat()[,"booked"]==1|part.dat()[,"money"]==1|part.dat()[,"staff"]==1|part.dat()[,"manager"]==1|part.dat()[,"breakfast"]==1|part.dat()[,"desk"]==1))})
    facility.part.len <- reactive({length(which(part.dat()[,"wall"]==1|part.dat()[,"parking"]==1))})
    location.part.len <- reactive({length(which(part.dat()[,"bar"]==1|part.dat()[,"downtown"]==1|part.dat()[,"restaurant"]==1|part.dat()[,"location"]==1))})
    atmoshere.part.len <- reactive({length(which(part.dat()[,"clean"]==1|part.dat()[,"quiet"]==1|part.dat()[,"comfortable"]==1|part.dat()[,"spacious"]==1|part.dat()[,"quiet"]==1|part.dat()[,"smell"]==1|part.dat()[,"modern"]==1|part.dat()[,"pretty"]==1|part.dat()[,"comfy"]==1|part.dat()[,"dirty"]==1))})
    
    #service
    book.part.star <- reactive({as.numeric(part.dat()$stars[which(part.dat()[,"booked"]==1)])})
    money.part.star <- reactive({as.numeric(part.dat()$stars[which(part.dat()[,"money"]==1)])})
    staff.part.star <- reactive({as.numeric(part.dat()$stars[which(part.dat()[,"staff"]==1)])})
    manager.part.star <- reactive({as.numeric(part.dat()$stars[which(part.dat()[,"manager"]==1)])})
    breakfast.part.star <- reactive({as.numeric(part.dat()$stars[which(part.dat()[,"breakfast"]==1)])})
    desk.part.star <- reactive({as.numeric(part.dat()$stars[which(part.dat()[,"desk"]==1)])})
    
    book.part.star.len <- reactive({length(which(part.dat()[,"booked"]==1))})
    money.part.star.len <- reactive({length(which(part.dat()[,"money"]==1))})
    staff.part.star.len <- reactive({length(which(part.dat()[,"staff"]==1))})
    manager.part.star.len <- reactive({length(which(part.dat()[,"manager"]==1))})
    breakfast.part.star.len <- reactive({length(which(part.dat()[,"breakfast"]==1))})
    desk.part.star.len <- reactive({length(which(part.dat()[,"desk"]==1))})
    
    book.dat.star <-as.numeric(dat$stars[which(dat[,"booked"]==1)])
    money.dat.star <- as.numeric(dat$stars[which(dat[,"money"]==1)])
    staff.dat.star <- as.numeric(dat$stars[which(dat[,"staff"]==1)])
    manager.dat.star <- as.numeric(dat$stars[which(dat[,"manager"]==1)])
    breakfast.dat.star <- as.numeric(dat$stars[which(dat[,"breakfast"]==1)])
    desk.dat.star <- as.numeric(dat$stars[which(dat[,"desk"]==1)])
    
    #facility
    wall.part.star <- reactive({as.numeric(part.dat()$stars[which(part.dat()[,"wall"]==1)])})
    parking.part.star <- reactive({as.numeric(part.dat()$stars[which(part.dat()[,"parking"]==1)])})
    
    wall.part.star.len <- reactive({length(which(part.dat()[,"wall"]==1))})
    parking.part.star.len <- reactive({length(which(part.dat()[,"parking"]==1))})
    
    wall.dat.star <- as.numeric(dat$stars[which(dat[,"wall"]==1)])
    parking.dat.star <- as.numeric(dat$stars[which(dat[,"parking"]==1)])
    
    #location
    bar.part.star <- reactive({as.numeric(part.dat()$stars[which(part.dat()[,"bar"]==1)])})
    downtown.part.star <- reactive({as.numeric(part.dat()$stars[which(part.dat()[,"downtown"]==1)])})
    restaurant.part.star <- reactive({as.numeric(part.dat()$stars[which(part.dat()[,"restaurant"]==1)])})
    location.part.star <- reactive({as.numeric(part.dat()$stars[which(part.dat()[,"location"]==1)])})
    
    bar.part.star.len <- reactive({length(which(part.dat()[,"bar"]==1))})
    downtown.part.star.len <- reactive({length(which(part.dat()[,"downtown"]==1))})
    restaurant.part.star.len <- reactive({length(which(part.dat()[,"restaurant"]==1))})
    location.part.star.len <- reactive({length(which(part.dat()[,"location"]==1))})
    
    bar.dat.star <- as.numeric(dat$stars[which(dat[,"bar"]==1)])
    downtown.dat.star <- as.numeric(dat$stars[which(dat[,"downtown"]==1)])
    restaurant.dat.star <- as.numeric(dat$stars[which(dat[,"restaurant"]==1)])
    location.dat.star <- as.numeric(dat$stars[which(dat[,"location"]==1)])
    
    #atmosphere
    clean.part.star <- reactive({as.numeric(part.dat()$stars[which(part.dat()[,"clean"]==1)])})
    comfortable.part.star <- reactive({as.numeric(part.dat()$stars[which(part.dat()[,"comfortable"]==1)])})
    spacious.part.star <- reactive({as.numeric(part.dat()$stars[which(part.dat()[,"spacious"]==1)])})
    quiet.part.star <- reactive({as.numeric(part.dat()$stars[which(part.dat()[,"quiet"]==1)])})
    smell.part.star <- reactive({as.numeric(part.dat()$stars[which(part.dat()[,"smell"]==1)])})
    modern.part.star <- reactive({as.numeric(part.dat()$stars[which(part.dat()[,"modern"]==1)])})
    pretty.part.star <- reactive({as.numeric(part.dat()$stars[which(part.dat()[,"pretty"]==1)])})
    comfy.part.star <- reactive({as.numeric(part.dat()$stars[which(part.dat()[,"comfy"]==1)])})
    dirty.part.star <- reactive({as.numeric(part.dat()$stars[which(part.dat()[,"dirty"]==1)])})
    
    clean.part.star.len <- reactive({length(which(part.dat()[,"clean"]==1))})
    comfortable.part.star.len <- reactive({length(which(part.dat()[,"comfortable"]==1))})
    spacious.part.star.len <- reactive({length(which(part.dat()[,"spacious"]==1))})
    quiet.part.star.len <- reactive({length(which(part.dat()[,"quiet"]==1))})
    smell.part.star.len <- reactive({length(which(part.dat()[,"smell"]==1))})
    modern.part.star.len <- reactive({length(which(part.dat()[,"modern"]==1))})
    pretty.part.star.len <- reactive({length(which(part.dat()[,"pretty"]==1))})
    comfy.part.star.len <- reactive({length(which(part.dat()[,"comfy"]==1))})
    dirty.part.star.len <- reactive({length(which(part.dat()[,"dirty"]==1))})

    clean.dat.star <- as.numeric(dat$stars[which(dat[,"clean"]==1)])
    comfortable.dat.star <- as.numeric(dat$stars[which(dat[,"comfortable"]==1)])
    spacious.dat.star <- as.numeric(dat$stars[which(dat[,"spacious"]==1)])
    quiet.dat.star <- as.numeric(dat$stars[which(dat[,"quiet"]==1)])
    smell.dat.star <- as.numeric(dat$stars[which(dat[,"smell"]==1)])
    modern.dat.star <- as.numeric(dat$stars[which(dat[,"modern"]==1)])
    pretty.dat.star <- as.numeric(dat$stars[which(dat[,"pretty"]==1)])
    comfy.dat.star <- as.numeric(dat$stars[which(dat[,"comfy"]==1)])
    dirty.dat.star <- as.numeric(dat$stars[which(dat[,"dirty"]==1)])
    
    output$wordcloud <- renderWordcloud2({
        if (input$Hotel_name == hotel.names[1]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[1]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[2]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[2]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[3]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[3]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[4]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[4]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[5]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[5]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[6]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[6]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[7]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[7]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[8]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[8]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[9]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[9]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[10]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[10]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[11]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[11]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[12]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[12]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[13]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[13]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[14]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[14]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[15]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[15]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[16]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[16]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[17]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[17]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[18]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[18]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[19]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[19]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[20]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[20]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[21]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[21]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[22]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[22]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[23]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[23]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[24]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[24]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[25]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[25]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[26]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[26]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[27]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[27]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[28]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[28]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[29]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[29]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[30]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[30]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[31]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[31]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[32]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[32]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[33]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[33]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[34]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[34]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[35]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[35]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[36]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[36]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[37]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[37]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[38]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[38]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[39]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[39]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[40]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[40]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[41]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[41]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[42]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[42]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[43]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[43]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[44]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[44]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        } else if (input$Hotel_name == hotel.names[45]) {
            part.dat.cloud <- dat[which(dat$name==hotel.names[45]),]
            part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
            part.freq.cloud <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
            worddataframe1 <- as.data.frame(part.words.cloud)
            worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
            wordcloud2(data <- worddataframe2)
        }

    })
    
    output$Overall1 <- renderText({
        paste(input$Hotel_name,"got",length(which(dat$name == input$Hotel_name)),"reviews from the customers.")
    })
    
    output$Overall2 <- renderText({   
        if (round(avg.hotel[input$Hotel_name],1)<round(mean(avg.hotel),1)){
            paste(input$Hotel_name,"obtained",round(avg.hotel[input$Hotel_name],1),"average rate which is below the average rate(",round(mean(avg.hotel),1),")of hotels in Madison Area.")
        }else if (round(avg.hotel[input$Hotel_name],1)==round(mean(avg.hotel),1)){
            paste(input$Hotel_name,"obtained",round(avg.hotel[input$Hotel_name],1),"average rate which is the same as average rate(",round(mean(avg.hotel),1),")of hotels in Madison Area.")
        }else{
            paste(input$Hotel_name,"obtained",round(avg.hotel[input$Hotel_name],1),"average rate which is above the average rate(",round(mean(avg.hotel),1),") of hotels in Madison Area.")
        }
     })

    output$barplot <- renderPlot({
        barplot(table(dat$stars[which(dat$name==input$Hotel_name)]), xlab="Stars",ylab="Number of Review", main=input$Hotel_name)
    })
    
    output$Service <- renderText({
        if(service.part.len()>=3){ 
          if(service.part.avg()>service.avg){
              paste(input$Hotel_name,"obtained",service.part.avg(),"star rate for service which is above the average rate(",service.avg,")of hotels in Madison Area.")
          }else if(service.part.avg()==service.avg){
              paste(input$Hotel_name,"obtained",service.part.avg(),"star rate for service which is the same as the average rate(",service.avg,")of hotels in Madison Area.")
          }else{
              paste(input$Hotel_name,"obtained",service.part.avg(),"star rate for service which is below the average rate(",service.avg,")of hotels in Madison Area.")
          }
        }
        else{
            print("Not enough reviews from customers")
        }
        })
    
    output$Facility <- renderText({
        if(facility.part.len()>=3){ 
            if(facility.part.avg()>facility.avg){
                 paste(input$Hotel_name,"obtained",facility.part.avg(),"star rate for facility which is above the average rate(",facility.avg,")of hotels in Madison Area.")
             }else if(facility.part.avg() == facility.avg){
                 paste(input$Hotel_name,"obtained",facility.part.avg(),"star rate for facility which is the same as the average rate(",facility.avg,")of hotels in Madison Area.")
             }else {
                 paste(input$Hotel_name,"obtained",facility.part.avg(),"star rate for facility which is below the average rate(",facility.avg,")of hotels in Madison Area.")
             }
        }else{
            print("Not enough reviews from customers")
        }
        })
    
    output$Location <- renderText({
        if(location.part.len()>=3){ 
         if(location.part.avg()>location.avg){
               paste(input$Hotel_name,"obtained",location.part.avg(),"star rate for location which is above the average rate(",location.avg,")of hotels in Madison Area.")
           }else if(location.part.avg() == location.avg){
               paste(input$Hotel_name,"obtained",location.part.avg(),"star rate for location which is the same as the average rate(",location.avg,")of hotels in Madison Area.")
           }else{
               paste(input$Hotel_name,"obtained",location.part.avg(),"star rate for location which is below the average rate(",location.avg,")of hotels in Madison Area.")
           }
        }else{
            print("Not enough reviews from customers")
        }})
    
    output$Atmosphere <- renderText({
        if(atmoshere.part.len()>=3){ 
            if(atmosphere.part.avg()>atmosphere.avg){
                  paste(input$Hotel_name,"obtained",atmosphere.part.avg(),"star rate for atmosphere which is above the average rate(",atmosphere.avg,")of hotels in Madison Area.")
              }else if(atmosphere.part.avg() == atmosphere.avg){
                  paste(input$Hotel_name,"obtained",atmosphere.part.avg(),"star rate for atmosphere which is the same as the average rate(",atmosphere.avg,")of hotels in Madison Area.")
              }else{
                  paste(input$Hotel_name,"obtained",atmosphere.part.avg(),"star rate for atmosphere which is below the average rate(",atmosphere.avg,")of hotels in Madison Area.")
              }
        }else{
            print("Not enough reviews from customers")
        }})
    
    output$Details <- renderText({
        paste(input$variable1,":")
    })


    output$boxplots <- renderPlot({
         par(mfrow=c(3,3))

         boxplot(cbind(book.part.star(),book.dat.star),main="Booked",ylab="Star")
         boxplot(cbind(money.part.star(),money.dat.star),main="Money",ylab="Star")
         boxplot(cbind(staff.part.star(),staff.dat.star),main="Staff",ylab="Star")
         boxplot(cbind(manager.part.star(),manager.dat.star),main="Manager",ylab="Star")
         boxplot(cbind(breakfast.part.star(),manager.dat.star),main="breakfast",ylab="Star")
         boxplot(cbind(desk.part.star(),desk.dat.star),main="Desk",ylab="Star")
         
         boxplot(cbind(wall.part.star(),wall.dat.star),main="wall",ylab="Star")
         boxplot(cbind(parking.part.star(),parking.dat.star),main="parking",ylab="Star")
         
         boxplot(cbind(bar.part.star(),bar.dat.star),main="bar",ylab="Star")
         boxplot(cbind(downtown.part.star(),downtown.dat.star),main="downtown",ylab="Star")
         boxplot(cbind(restaurant.part.star(),restaurant.dat.star),main="restaurant",ylab="Star")
         boxplot(cbind(location.part.star(),location.dat.star),main="location",ylab="Star")
         
         boxplot(cbind(clean.part.star(),clean.dat.star),main="clean",ylab="Star")
         boxplot(cbind(comfortable.part.star(),comfortable.dat.star),main="comfortable",ylab="Star")
         boxplot(cbind(spacious.part.star(),spacious.dat.star),main="spacious",ylab="Star")
         boxplot(cbind(quiet.part.star(),quiet.dat.star),main="quiet",ylab="Star")
         boxplot(cbind(smell.part.star(),smell.dat.star),main="smell",ylab="Star")
         boxplot(cbind(modern.part.star(),modern.dat.star),main="modern",ylab="Star")
         boxplot(cbind(pretty.part.star(),pretty.dat.star),main="pretty",ylab="Star")
         boxplot(cbind(comfy.part.star(),comfy.dat.star),main="comfy",ylab="Star")
         boxplot(cbind(dirty.part.star(),dirty.dat.star),main="dirty",ylab="Star")
    })
     
    output$book <- renderText({
        if(book.part.star.len()>=3){
            if(round(mean(book.part.star()),1)>=round(mean(book.dat.star),1)){
                paste(input$Hotel_name,"obtained",round(mean(book.part.star()),1),"star rate from the customers and it is above or the same as the average rate(",round(mean(book.dat.star),1),")of hotels in Madison Area.",input$Hotel_name,"is performing a good reservation system.")
            }else{
                paste(input$Hotel_name, "obtained",round(mean(book.part.star()),1),"star rate from the customers and it is below the average rate(",round(mean(book.dat.star),1),")of hotels in Madison Area.",input$Hotel_name,"needs to improve the reservation system.")
            }
        }else{
            print("Not enough reviews from customers")
        }
    }) 
    
    output$price <- renderText({
        if(money.part.star.len()>=3){
            if(round(mean(money.part.star()),1)>=round(mean(money.dat.star),1)){
                paste(input$Hotel_name,"obtained",round(mean(money.part.star()),1),"star rate from the customers and it is above or the same as the average rate(",round(mean(money.dat.star),1),")of hotels in Madison Area.",input$Hotel_name,"provides the service in proper price.")
            }else{
                paste(input$Hotel_name, "obtained",round(mean(money.part.star()),1),"star rate from the customers and it is below the average rate(",round(mean(money.dat.star),1),")of hotels in Madison Area.",input$Hotel_name,"needs to adjust the lodging expense.")
            }
        }else{
            print("Not enough reviews from customers")
        }
    })
    
    output$employee <- renderText({
        if((staff.part.star.len()+manager.part.star.len())>=3){
            if(round(mean(c(staff.part.star(),manager.part.star())),1)>=round(mean(c(staff.dat.star,manager.dat.star)),1)){
                paste(input$Hotel_name,"obtained",round(mean(c(staff.part.star(),manager.part.star())),1),"star rate from the customers and it is above or the same as the average rate(",round(mean(c(staff.dat.star,manager.dat.star)),1),")of hotels in Madison Area.",input$Hotel_name,"'s staffs provides the service in a good manner.")
            }else{
                paste(input$Hotel_name, "obtained",round(mean(c(staff.part.star(),manager.part.star())),1),"star rate from the customers and it is below the average rate(",round(mean(c(staff.dat.star,manager.dat.star)),1),")of hotels in Madison Area.",input$Hotel_name,"'s staffs needs to be trained for providing the service in a good manner.")
            }
        }else{
            print("Not enough reviews from customers")
        }
    }) 
    
    output$breakf <- renderText({
        if((breakfast.part.star.len())>=3){
            if(round(mean(breakfast.part.star()),1)>=round(mean(breakfast.dat.star),1)){
                paste(input$Hotel_name,"obtained",round(mean(breakfast.part.star()),1),"star rate from the customers and it is above or the same as the average rate(",round(mean(breakfast.dat.star),1),")of hotels in Madison Area.",input$Hotel_name," provides good quality of breakfast.")
            }else{
                paste(input$Hotel_name, "obtained",round(mean(breakfast.part.star()),1),"star rate from the customers and it is below the average rate(",round(mean(breakfast.dat.star),1),")of hotels in Madison Area.",input$Hotel_name,"needs to improve the quality of the breakfast.")
            }
        }else{
            print("Not enough reviews from customers")
        }
    })
    
    output$desk <- renderText({
        if((desk.part.star.len())>=3){
            if(round(mean(desk.part.star()),1)>=round(mean(desk.dat.star),1)){
                paste(input$Hotel_name,"obtained",round(mean(desk.part.star()),1),"star rate from the customers and it is above or the same as the average rate(",round(mean(desk.dat.star),1),")of hotels in Madison Area.",input$Hotel_name," provides good quality of desk in the room.")
            }else{
                paste(input$Hotel_name, "obtained",round(mean(desk.part.star()),1),"star rate from the customers and it is below the average rate(",round(mean(desk.dat.star),1),")of hotels in Madison Area.",input$Hotel_name,"needs to improve the quality of the desk in the room")
            }
        }else{
            print("Not enough reviews from customers")
        }
    })
    
    output$wall <- renderText({
        if((wall.part.star.len())>=3){
            if(round(mean(wall.part.star()),1)>=round(mean(wall.dat.star),1)){
                paste(input$Hotel_name,"obtained",round(mean(wall.part.star()),1),"star rate from the customers and it is above or the same as the average rate(",round(mean(wall.dat.star),1),")of hotels in Madison Area.",input$Hotel_name,"'s wall earns good evaluation from customer. Their wall may have good design and soundproofing.")
            }else{
                paste(input$Hotel_name, "obtained",round(mean(wall.part.star()),1),"star rate from the customers and it is below the average rate(",round(mean(wall.dat.star),1),")of hotels in Madison Area.",input$Hotel_name,"needs to improve the design of the wall or enhance the soundproofing function of the wall.")
            }
        }else{
            print("Not enough reviews from customers")
        }
    })
    
    output$parking <- renderText({
        if((parking.part.star.len())>=3){
            if(round(mean(parking.part.star()),1)>=round(mean(parking.dat.star),1)){
                paste(input$Hotel_name,"obtained",round(mean(parking.part.star()),1),"star rate from the customers and it is above or the same as the average rate(",round(mean(parking.dat.star),1),")of hotels in Madison Area.",input$Hotel_name,"has enough parking facility.")
            }else{
                paste(input$Hotel_name, "obtained",round(mean(parking.part.star()),1),"star rate from the customers and it is below the average rate(",round(mean(parking.dat.star),1),")of hotels in Madison Area.",input$Hotel_name,"needs to expand their parking facility.")
            }
        }else{
            print("Not enough reviews from customers")
        }
    }) 
    
    output$location <- renderText({
        if((location.part.star.len()+downtown.part.star.len())>=3){
            if(round(mean(c(location.part.star(),downtown.part.star())),1)>=round(mean(c(location.dat.star,downtown.dat.star)),1)){
                paste(input$Hotel_name,"obtained",round(mean(c(location.part.star(),downtown.part.star())),1),"star rate from the customers and it is above or the same as the average rate(",round(mean(c(location.dat.star,downtown.dat.star)),1),")of hotels in Madison Area.",input$Hotel_name,"is located in nice place and has a good approach to Downtown Madison.")
            }else{
                paste(input$Hotel_name, "obtained",round(mean(c(location.part.star(),downtown.part.star())),1),"star rate from the customers and it is below the average rate(",round(mean(c(location.dat.star,downtown.dat.star)),1),")of hotels in Madison Area.",input$Hotel_name,"does not have a good approach need to provide public transportaions to get over the disadvantage.")
            }
        }else{
            print("Not enough reviews from customers")
        }
    })
    
    output$restaurant <- renderText({
        if((restaurant.part.star.len())>=3){
            if(round(mean(restaurant.part.star()),1)>=round(mean(restaurant.dat.star),1)){
                paste(input$Hotel_name,"obtained",round(mean(restaurant.part.star()),1),"star rate from the customers and it is above or the same as the average rate(",round(mean(restaurant.dat.star),1),")of hotels in Madison Area.",input$Hotel_name,"has a nice restaurant inside of the hotel or in surrounding area.")
            }else{
                paste(input$Hotel_name, "obtained",round(mean(restaurant.part.star()),1),"star rate from the customers and it is below the average rate(",round(mean(restaurant.dat.star),1),")of hotels in Madison Area.",input$Hotel_name,"needs to take care about the restaurant inside of the hotel or in surrounding area.")
            }
        }else{
            print("Not enough reviews from customers")
        }
    })
    
    output$bar <- renderText({
        if((bar.part.star.len())>=3){
            if(round(mean(bar.part.star()),1)>=round(mean(bar.dat.star),1)){
                paste(input$Hotel_name,"obtained",round(mean(bar.part.star()),1),"star rate from the customers and it is above or the same as the average rate(",round(mean(bar.dat.star),1),")of hotels in Madison Area.",input$Hotel_name,"has a nice bar inside of the hotel or in surrounding area.")
            }else{
                paste(input$Hotel_name, "obtained",round(mean(bar.part.star()),1),"star rate from the customers and it is below the average rate(",round(mean(bar.dat.star),1),")of hotels in Madison Area.",input$Hotel_name,"needs to take care about the bar inside of the hotel or in surrounding area.")
            }
        }else{
            print("Not enough reviews from customers")
        }
    })
    
    output$clean <- renderText({
        if((clean.part.star.len())>=3){
            if(round(mean(clean.part.star()),1)>=round(mean(clean.dat.star),1)){
                paste(input$Hotel_name,"obtained",round(mean(clean.part.star()),1),"star rate from the customers and it is above or the same as the average rate(",round(mean(clean.dat.star),1),")of hotels in Madison Area.",input$Hotel_name,"provides clean environment for customers.")
            }else{
                paste(input$Hotel_name, "obtained",round(mean(clean.part.star()),1),"star rate from the customers and it is below the average rate(",round(mean(clean.dat.star),1),")of hotels in Madison Area.",input$Hotel_name,"needs to take care about their cleanliness.")
            }
        }else{
            print("Not enough reviews from customers")
        }
    })
    
    output$comfortable <- renderText({
        if((comfortable.part.star.len()+comfy.part.star.len())>=3){
            if(round(mean(c(comfortable.part.star(),comfy.part.star())),1)>=round(mean(c(comfortable.dat.star,comfy.dat.star)),1)){
                paste(input$Hotel_name,"obtained",round(mean(c(comfortable.part.star(),comfy.part.star())),1),"star rate from the customers and it is above or the same as the average rate(",round(mean(c(comfortable.dat.star,comfy.dat.star)),1),")of hotels in Madison Area.",input$Hotel_name,"provides comfortable environment for customers.")
            }else{
                paste(input$Hotel_name, "obtained",round(mean(c(comfortable.part.star(),comfy.part.star())),1),"star rate from the customers and it is below the average rate(",round(mean(c(comfortable.dat.star,comfy.dat.star)),1),")of hotels in Madison Area.",input$Hotel_name,"needs to improve their environment to make customers feel comfortable.")
            }
        }else{
            print("Not enough reviews from customers")
        }
    })
    
    output$spacious <- renderText({
        if((spacious.part.star.len())>=3){
            if(round(mean(spacious.part.star()),1)>=round(mean(spacious.dat.star),1)){
                paste(input$Hotel_name,"obtained",round(mean(spacious.part.star()),1),"star rate from the customers and it is above or the same as the average rate(",round(mean(spacious.dat.star),1),")of hotels in Madison Area.",input$Hotel_name,"provides spacious environment for customers.")
            }else{
                paste(input$Hotel_name, "obtained",round(mean(spacious.part.star()),1),"star rate from the customers and it is below the average rate(",round(mean(spacious.dat.star),1),")of hotels in Madison Area.",input$Hotel_name,"needs to improve their interior to be seen more spacious.")
            }
        }else{
            print("Not enough reviews from customers")
        }
    })
    
    output$smell <- renderText({
        if((smell.part.star.len())>=3){
            if(round(mean(smell.part.star()),1)>=round(mean(smell.dat.star),1)){
                paste(input$Hotel_name,"obtained",round(mean(smell.part.star()),1),"star rate from the customers and it is above or the same as the average rate(",round(mean(smell.dat.star),1),")of hotels in Madison Area.",input$Hotel_name,"has not bad smell.")
            }else{
                paste(input$Hotel_name, "obtained",round(mean(smell.part.star()),1),"star rate from the customers and it is below the average rate(",round(mean(smell.dat.star),1),")of hotels in Madison Area.",input$Hotel_name,"needs to find the reason that cause bad smell.")
            }
        }else{
            print("Not enough reviews from customers")
        }
    })
    
    output$modern <- renderText({
        if((modern.part.star.len())>=3){
            if(round(mean(modern.part.star()),1)>=round(mean(modern.dat.star),1)){
                paste(input$Hotel_name,"obtained",round(mean(modern.part.star()),1),"star rate from the customers and it is above or the same as the average rate(",round(mean(modern.dat.star),1),")of hotels in Madison Area.",input$Hotel_name,"provides modernic environment for customers.")
            }else{
                paste(input$Hotel_name, "obtained",round(mean(modern.part.star()),1),"star rate from the customers and it is below the average rate(",round(mean(modern.dat.star),1),")of hotels in Madison Area.",input$Hotel_name,"needs to improve their interior to be seen as more modernic.")
            }
        }else{
            print("Not enough reviews from customers")
        }
    })
    
    output$pretty <- renderText({
        if((pretty.part.star.len())>=3){
            if(round(mean(pretty.part.star()),1)>=round(mean(pretty.dat.star),1)){
                paste(input$Hotel_name,"obtained",round(mean(pretty.part.star()),1),"star rate from the customers and it is above or the same as the average rate(",round(mean(pretty.dat.star),1),")of hotels in Madison Area.",input$Hotel_name,"provides pretty view and interiors for customers.")
            }else{
                paste(input$Hotel_name, "obtained",round(mean(pretty.part.star()),1),"star rate from the customers and it is below the average rate(",round(mean(pretty.dat.star),1),")of hotels in Madison Area.",input$Hotel_name,"needs to improve their interior to be seen as more pretty.")
            }
        }else{
            print("Not enough reviews from customers")
        }
    })
    
    output$dirty <- renderText({
        if((dirty.part.star.len())>=3){
            if(round(mean(dirty.part.star()),1)>=round(mean(dirty.dat.star),1)){
                paste(input$Hotel_name,"obtained",round(mean(dirty.part.star()),1),"star rate from the customers and it is above or the same as the average rate(",round(mean(dirty.dat.star),1),")of hotels in Madison Area.",input$Hotel_name,"does not get complaints about the dirtiness from the customers.")
            }else{
                paste(input$Hotel_name, "obtained",round(mean(dirty.part.star()),1),"star rate from the customers and it is below the average rate(",round(mean(dirty.dat.star),1),")of hotels in Madison Area.",input$Hotel_name,"needs to take care about cleanliness of rooms.")
            }
        }else{
            print("Not enough reviews from customers")
        }
    })
    
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


