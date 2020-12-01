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
library(ggplot2)
if (!require(gridExtra)){
    install.packages("gridExtra")
}


dat <- read.csv("wordembedding.csv",header=TRUE)
sentiment <- read.csv("sentiments.csv")
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
    
    # output$wordcloud1 <- renderText({
    #     paste("Most freqently used word for",input$Hotel_name,":")
    # })
    output$wordcloud <- renderWordcloud2({
        indx = which(hotel.names == input$Hotel_name)
        part.dat.cloud <- dat[which(dat$name==hotel.names[indx]),]
        part.words.cloud <- apply(part.dat.cloud[,4:dim(part.dat.cloud)[2]],2,sum)
        part.freq <- names(sort(part.words.cloud, decreasing= TRUE)[1:30])
        worddataframe1 <- as.data.frame(part.words.cloud)
        worddataframe2 <- data.frame(w = rownames(worddataframe1),c = worddataframe1$part.words.cloud)
        wordcloud2(data <- worddataframe2)
    })

    
    output$Overall1 <- renderText({
        paste(input$Hotel_name,"got",length(which(dat$name == input$Hotel_name)),"reviews from the customers. There are"
              , sentiment$P[which(sentiment$name == input$Hotel_name)], "positive reviews and", sentiment$N[which(sentiment$name == input$Hotel_name)], "negative reviews.")
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
        bar = table(dat$stars[which(dat$name==input$Hotel_name)])
        bar = as.data.frame(bar)
        colnames(bar) = c("Stars", "Freq")
        ggplot(bar, aes(x = Stars, y = Freq, fill = Stars))+
            geom_bar(stat="identity")+
            scale_fill_brewer(palette="Blues")+
            theme_minimal()+
            theme(legend.position="bottom")+
            ylab("Number of Reviews")+
            labs(title = input$Hotel_name)
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
         # par(mfrow=c(3,3))
         variables =  c("Service","Facility","Location","Atmosphere")
         if (input$variable1 == variables[1]) {
             if (book.part.star.len() > 0){
                 df1 = data.frame(book = input$Hotel_name, Stars = book.part.star())
             } else {
                 df1 = data.frame()
             }
             df2 = data.frame(book = "Total", Stars = book.dat.star)
             plot.data = rbind(df1,df2)
             p1 = ggplot(plot.data, aes(x=book, y=Stars, fill=book)) + geom_boxplot() + 
                 theme(legend.position = "none")
             if (money.part.star.len() > 0){
                 df1 = data.frame(money = input$Hotel_name, Stars = money.part.star())
             } else {
                 df1 = data.frame()
             }
             df2 = data.frame(money = "Total", Stars = money.dat.star)
             plot.data = rbind(df1,df2)
             p2 = ggplot(plot.data, aes(x=money, y=Stars, fill=money)) + geom_boxplot() + 
                 theme(legend.position = "none")
             if (staff.part.star.len() > 0){
                 df1 = data.frame(staff = input$Hotel_name, Stars = staff.part.star())
             } else {
                 df1 = data.frame()
             }
             df2 = data.frame(staff = "Total", Stars = staff.dat.star)
             plot.data = rbind(df1,df2)
             p3 = ggplot(plot.data, aes(x=staff, y=Stars, fill=staff)) + geom_boxplot() + 
                 theme(legend.position = "none")
             if (manager.part.star.len() > 0){
                 df1 = data.frame(manager = input$Hotel_name, Stars = manager.part.star())
             } else {
                 df1 = data.frame()
             }
             df2 = data.frame(manager = "Total", Stars = manager.dat.star)
             plot.data = rbind(df1,df2)
             p4 = ggplot(plot.data, aes(x=manager, y=Stars, fill=manager)) + geom_boxplot() + 
                 theme(legend.position = "none")
             if (breakfast.part.star.len() > 0){
                 df1 = data.frame(breakfast = input$Hotel_name, Stars = breakfast.part.star())
             } else {
                 df1 = data.frame()
             }
             df2 = data.frame(breakfast = "Total", Stars = manager.dat.star)
             plot.data = rbind(df1,df2)
             p5 = ggplot(plot.data, aes(x=breakfast, y=Stars, fill=breakfast)) + geom_boxplot() + 
                 theme(legend.position = "none")
             if (desk.part.star.len() > 0){
                 df1 = data.frame(desk = input$Hotel_name, Stars = desk.part.star())
             } else {
                 df1 = data.frame()
             }
             df2 = data.frame(desk = "Total", Stars = desk.dat.star)
             plot.data = rbind(df1,df2)
             p6 = ggplot(plot.data, aes(x=desk, y=Stars, fill=desk)) + geom_boxplot() + 
                 theme(legend.position = "none")
             grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3)
         } else if (input$variable1 == variables[2]){
             if (wall.part.star.len() > 0){
                 df1 = data.frame(wall = input$Hotel_name, Stars = wall.part.star())
             } else {
                 df1 = data.frame()
             }
             df2 = data.frame(wall = "Total", Stars = wall.dat.star)
             plot.data = rbind(df1,df2)
             p1 = ggplot(plot.data, aes(x=wall, y=Stars, fill=wall)) + geom_boxplot() + 
                 theme(legend.position = "none")
             if (parking.part.star.len() > 0){
                 df1 = data.frame(parking = input$Hotel_name, Stars = parking.part.star())
             } else {
                 df1 = data.frame()
             }
             df2 = data.frame(parking = "Total", Stars = parking.dat.star)
             plot.data = rbind(df1,df2)
             p2 = ggplot(plot.data, aes(x=parking, y=Stars, fill=parking)) + geom_boxplot() + 
                 theme(legend.position = "none")
             grid.arrange(p1, p2, ncol=2)
         } else if (input$variable1 == variables[3]){
             if (bar.part.star.len() > 0){
                 df1 = data.frame(bar = input$Hotel_name, Stars = bar.part.star())
             } else {
                 df1 = data.frame()
             }
             df2 = data.frame(bar = "Total", Stars = bar.dat.star)
             plot.data = rbind(df1,df2)
             p1 = ggplot(plot.data, aes(x=bar, y=Stars, fill=bar)) + geom_boxplot() + 
                 theme(legend.position = "none")
             if (downtown.part.star.len() > 0){
                 df1 = data.frame(downtown = input$Hotel_name, Stars = downtown.part.star())
             } else {
                 df1 = data.frame()
             }
             df2 = data.frame(downtown = "Total", Stars = downtown.dat.star)
             plot.data = rbind(df1,df2)
             p2 = ggplot(plot.data, aes(x=downtown, y=Stars, fill=downtown)) + geom_boxplot() + 
                 theme(legend.position = "none")
             if (restaurant.part.star.len() > 0){
                 df1 = data.frame(restaurant = input$Hotel_name, Stars = restaurant.part.star())
             } else {
                 df1 = data.frame()
             }
             df2 = data.frame(restaurant = "Total", Stars = restaurant.dat.star)
             plot.data = rbind(df1,df2)
             p3 = ggplot(plot.data, aes(x=restaurant, y=Stars, fill=restaurant)) + geom_boxplot() + 
                 theme(legend.position = "none")
             if (location.part.star.len() > 0){
                 df1 = data.frame(location = input$Hotel_name, Stars = location.part.star())
             } else {
                 df1 = data.frame()
             }
             df2 = data.frame(location = "Total", Stars = location.dat.star)
             plot.data = rbind(df1,df2)
             p4 = ggplot(plot.data, aes(x=location, y=Stars, fill=location)) + geom_boxplot() + 
                 theme(legend.position = "none")
             grid.arrange(p1, p2, p3, p4, ncol=2)
         } else if (input$variable1 == variables[4]){
             if (clean.part.star.len() > 0){
                 df1 = data.frame(clean = input$Hotel_name, Stars = clean.part.star())
             } else {
                 df1 = data.frame()
             }
             df2 = data.frame(clean = "Total", Stars = clean.dat.star)
             plot.data = rbind(df1,df2)
             p1 = ggplot(plot.data, aes(x=clean, y=Stars, fill=clean)) + geom_boxplot() + 
                 theme(legend.position = "none")
             if (comfortable.part.star.len() > 0){
                 df1 = data.frame(comfortable = input$Hotel_name, Stars = comfortable.part.star())
             } else {
                 df1 = data.frame()
             }
             df2 = data.frame(comfortable = "Total", Stars = comfortable.dat.star)
             plot.data = rbind(df1,df2)
             p2 = ggplot(plot.data, aes(x=comfortable, y=Stars, fill=comfortable)) + geom_boxplot() + 
                 theme(legend.position = "none")
             if (spacious.part.star.len() > 0){
                 df1 = data.frame(spacious = input$Hotel_name, Stars = spacious.part.star())
             } else {
                 df1 = data.frame()
             }
             df2 = data.frame(spacious = "Total", Stars = spacious.dat.star)
             plot.data = rbind(df1,df2)
             p3 = ggplot(plot.data, aes(x=spacious, y=Stars, fill=spacious)) + geom_boxplot() + 
                 theme(legend.position = "none")
             if (quiet.part.star.len() > 0){
                 df1 = data.frame(quiet = input$Hotel_name, Stars = quiet.part.star())
             } else {
                 df1 = data.frame()
             }
             df2 = data.frame(quiet = "Total", Stars = quiet.dat.star)
             plot.data = rbind(df1,df2)
             p4 = ggplot(plot.data, aes(x=quiet, y=Stars, fill=quiet)) + geom_boxplot() + 
                 theme(legend.position = "none")
             if (smell.part.star.len() > 0){
                 df1 = data.frame(smell = input$Hotel_name, Stars = smell.part.star())
             } else {
                 df1 = data.frame()
             }
             df2 = data.frame(smell = "Total", Stars = smell.dat.star)
             plot.data = rbind(df1,df2)
             p5 = ggplot(plot.data, aes(x=smell, y=Stars, fill=smell)) + geom_boxplot() + 
                 theme(legend.position = "none")
             if (modern.part.star.len() > 0){
                 df1 = data.frame(modern = input$Hotel_name, Stars = modern.part.star())
             } else {
                 df1 = data.frame()
             }
             df2 = data.frame(modern = "Total", Stars = modern.dat.star)
             plot.data = rbind(df1,df2)
             p6 = ggplot(plot.data, aes(x=modern, y=Stars, fill=modern)) + geom_boxplot() + 
                 theme(legend.position = "none")
             if (pretty.part.star.len() > 0){
                 df1 = data.frame(pretty = input$Hotel_name, Stars = pretty.part.star())
             } else {
                 df1 = data.frame()
             }
             df2 = data.frame(pretty = "Total", Stars = pretty.dat.star)
             plot.data = rbind(df1,df2)
             p7 = ggplot(plot.data, aes(x=pretty, y=Stars, fill=pretty)) + geom_boxplot() + 
                 theme(legend.position = "none")
             if (comfy.part.star.len() > 0){
                 df1 = data.frame(comfy = input$Hotel_name, Stars = comfy.part.star())
             } else {
                 df1 = data.frame()
             }
             df2 = data.frame(comfy = "Total", Stars = comfy.dat.star)
             plot.data = rbind(df1,df2)
             p8 = ggplot(plot.data, aes(x=comfy, y=Stars, fill=comfy)) + geom_boxplot() + 
                 theme(legend.position = "none")
             if (dirty.part.star.len() > 0){
                 df1 = data.frame(dirty = input$Hotel_name, Stars = dirty.part.star())
             } else {
                 df1 = data.frame()
             }
             df2 = data.frame(dirty = "Total", Stars = dirty.dat.star)
             plot.data = rbind(df1,df2)
             p9 = ggplot(plot.data, aes(x=dirty, y=Stars, fill=dirty)) + geom_boxplot() + 
                 theme(legend.position = "none")
             grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, ncol=3)
         } 
         
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


