dat <- read.csv("wordembedding.csv",header=TRUE)

avg.hotel <- sort(tapply(as.numeric(dat$stars),dat$name,mean))
hotel.names <- names(avg.hotel)

if (round(avg.hotel[hotel.names[2]],1)<round(mean(avg.hotel),1)){
  print(paste(hotel.names[2],"obtained",round(avg.hotel[hotel.names[2]],1),"average rate which is below the average rate(",round(mean(avg.hotel),1),")of hotels in Madison Area"))
}else{
  print(paste(hotel.names[2],"obtained",round(avg.hotel[hotel.names[2]],1),"average rate which is above the average rate(",round(mean(avg.hotel),1),") of hotels in Madison Area"))
}

print(paste(hotel.names[2],"got",length(which(dat$name==hotel.names[2])),"reviews from the customers"))

barplot(table(dat$stars[which(dat$name==hotel.names[2])]), xlab="Stars",ylab="Number of Review", main=hotel.names[2])

freq.words <- as.data.frame(sort(part.words, decreasing= TRUE)[1:30])

if(length(which(dat$name==hotel.names[2]))>5){

  part.dat <- dat[which(dat$name==hotel.names[2]),]
  part.words <- apply(part.dat[,4:dim(part.dat)[2]],2,sum)
  part.freq <- names(sort(part.words, decreasing= TRUE)[1:30])
  
  print(paste("Most frequently appeared 30 words in reviews for",hotel.names[2],"are"))
  print(sort(part.words, decreasing= TRUE)[1:30])


service.avg <- round(mean(as.numeric(dat$stars[which(dat[,"booked"]==1|dat[,"money"]==1|dat[,"staff"]==1|dat[,"manager"]==1|dat[,"breakfast"]==1|dat[,"desk"]==1)])),1)
facility.avg <- round(mean(as.numeric(dat$stars[which(dat[,"wall"]==1|dat[,"parking"]==1)])),1)
location.avg <- round(mean(as.numeric(dat$stars[which(dat[,"bar"]==1|dat[,"downtown"]==1|dat[,"restaurant"]==1|dat[,"location"]==1)])),1)
atmosphere.avg <- round(mean(as.numeric(dat$stars[which(dat[,"clean"]==1|dat[,"quiet"]==1|dat[,"comfortable"]==1|dat[,"spacious"]==1|dat[,"quiet"]==1|dat[,"smell"]==1|dat[,"modern"]==1|dat[,"pretty"]==1|dat[,"comfy"]==1|dat[,"dirty"]==1)])),1)


service.part.avg <- round(mean(as.numeric(part.dat$stars[which(part.dat[,"booked"]==1|part.dat[,"money"]==1|part.dat[,"staff"]==1|part.dat[,"manager"]==1|part.dat[,"breakfast"]==1|part.dat[,"desk"]==1)])),1)
facility.part.avg <- round(mean(as.numeric(part.dat$stars[which(part.dat[,"wall"]==1|part.dat[,"parking"]==1)])),1)
location.part.avg <- round(mean(as.numeric(part.dat$stars[which(part.dat[,"bar"]==1|part.dat[,"downtown"]==1|part.dat[,"restaurant"]==1|part.dat[,"location"]==1)])),1)
atmosphere.part.avg <- round(mean(as.numeric(part.dat$stars[which(part.dat[,"clean"]==1|part.dat[,"quiet"]==1|part.dat[,"comfortable"]==1|part.dat[,"spacious"]==1|part.dat[,"quiet"]==1|part.dat[,"smell"]==1|part.dat[,"modern"]==1|part.dat[,"pretty"]==1|part.dat[,"comfy"]==1|part.dat[,"dirty"]==1)])),1)

if(service.part.avg>service.avg){
  print(paste(hotel.names[2],"obtained",service.part.avg,"star rate for service which is above the average rate(",service.avg,")of hotels in Madison Area"))
}else{
  print(paste(hotel.names[2],"obtained",service.part.avg,"star rate for service which is below the average rate(",service.avg,")of hotels in Madison Area"))
}

if(facility.part.avg>facility.avg){
  print(paste(hotel.names[2],"obtained",facility.part.avg,"star rate for facility which is above the average rate(",facility.avg,")of hotels in Madison Area"))
}else{
  print(paste(hotel.names[2],"obtained",facility.part.avg,"star rate for facility which is below the average rate(",facility.avg,")of hotels in Madison Area"))
}

if(location.part.avg>location.avg){
  print(paste(hotel.names[2],"obtained",location.part.avg,"star rate for location which is above the average rate(",location.avg,")of hotels in Madison Area"))
}else{
  print(paste(hotel.names[2],"obtained",location.part.avg,"star rate for location which is below the average rate(",location.avg,")of hotels in Madison Area"))
}

if(atmosphere.part.avg>atmosphere.avg){
  print(paste(hotel.names[2],"obtained",atmosphere.part.avg,"star rate for atmosphere which is above the average rate(",atmosphere.avg,")of hotels in Madison Area"))
}else{
  print(paste(hotel.names[2],"obtained",atmosphere.part.avg,"star rate for atmosphere which is below the average rate(",atmosphere.avg,")of hotels in Madison Area"))
}

service <- c("booked","money","staff","manager","breakfast","desk")
facility <- c("wall","parking")
location <- c("bar","downtown","restaurant","location")
atmosphere <- c("clean","comfortable","spacious","quiet","smell","modern","pretty","comfy","dirty")

#service
book.part.star <- as.numeric(part.dat$stars[which(part.dat[,"booked"]==1)])
money.part.star <- as.numeric(part.dat$stars[which(part.dat[,"money"]==1)])
staff.part.star <- as.numeric(part.dat$stars[which(part.dat[,"staff"]==1)])
manager.part.star <- as.numeric(part.dat$stars[which(part.dat[,"manager"]==1)])
breakfast.part.star <- as.numeric(part.dat$stars[which(part.dat[,"breakfast"]==1)])
desk.part.star <- as.numeric(part.dat$stars[which(part.dat[,"desk"]==1)])

book.dat.star <- as.numeric(dat$stars[which(dat[,"booked"]==1)])
money.dat.star <- as.numeric(dat$stars[which(dat[,"money"]==1)])
staff.dat.star <- as.numeric(dat$stars[which(dat[,"staff"]==1)])
manager.dat.star <- as.numeric(dat$stars[which(dat[,"manager"]==1)])
breakfast.dat.star <- as.numeric(dat$stars[which(dat[,"breakfast"]==1)])
desk.dat.star <- as.numeric(dat$stars[which(dat[,"desk"]==1)])

#facility
wall.part.star <- as.numeric(part.dat$stars[which(part.dat[,"wall"]==1)])
parking.part.star <- as.numeric(part.dat$stars[which(part.dat[,"parking"]==1)])

wall.dat.star <- as.numeric(dat$stars[which(dat[,"wall"]==1)])
parking.dat.star <- as.numeric(dat$stars[which(dat[,"parking"]==1)])

#location
bar.part.star <- as.numeric(part.dat$stars[which(part.dat[,"bar"]==1)])
downtown.part.star <- as.numeric(part.dat$stars[which(part.dat[,"downtown"]==1)])
restaurant.part.star <- as.numeric(part.dat$stars[which(part.dat[,"restaurant"]==1)])
location.part.star <- as.numeric(part.dat$stars[which(part.dat[,"location"]==1)])

bar.dat.star <- as.numeric(dat$stars[which(dat[,"bar"]==1)])
downtown.dat.star <- as.numeric(dat$stars[which(dat[,"downtown"]==1)])
restaurant.dat.star <- as.numeric(dat$stars[which(dat[,"restaurant"]==1)])
location.dat.star <- as.numeric(dat$stars[which(dat[,"location"]==1)])

#atmosphere
clean.part.star <- as.numeric(part.dat$stars[which(part.dat[,"clean"]==1)])
comfortable.part.star <- as.numeric(part.dat$stars[which(part.dat[,"comfortable"]==1)])
spacious.part.star <- as.numeric(part.dat$stars[which(part.dat[,"spacious"]==1)])
quiet.part.star <- as.numeric(part.dat$stars[which(part.dat[,"quiet"]==1)])
smell.part.star <- as.numeric(part.dat$stars[which(part.dat[,"smell"]==1)])
modern.part.star <- as.numeric(part.dat$stars[which(part.dat[,"modern"]==1)])
pretty.part.star <- as.numeric(part.dat$stars[which(part.dat[,"pretty"]==1)])
comfy.part.star <- as.numeric(part.dat$stars[which(part.dat[,"comfy"]==1)])
dirty.part.star <- as.numeric(part.dat$stars[which(part.dat[,"dirty"]==1)])

clean.dat.star <- as.numeric(dat$stars[which(dat[,"clean"]==1)])
comfortable.dat.star <- as.numeric(dat$stars[which(dat[,"comfortable"]==1)])
spacious.dat.star <- as.numeric(dat$stars[which(dat[,"spacious"]==1)])
quiet.dat.star <- as.numeric(dat$stars[which(dat[,"quiet"]==1)])
smell.dat.star <- as.numeric(dat$stars[which(dat[,"smell"]==1)])
modern.dat.star <- as.numeric(dat$stars[which(dat[,"modern"]==1)])
pretty.dat.star <- as.numeric(dat$stars[which(dat[,"pretty"]==1)])
comfy.dat.star <- as.numeric(dat$stars[which(dat[,"comfy"]==1)])
dirty.dat.star <- as.numeric(dat$stars[which(dat[,"dirty"]==1)])

boxplot(cbind(book.part.star,book.dat.star),main="Booked")
a <- t.test(book.part.star,book.dat.star)

boxplot(cbind(money.part.star,money.dat.star),main="money")


boxplot(cbind(staff.part.star,staff.dat.star),main="staff")



boxplot(cbind(manager.part.star,manager.dat.star),main="manager")

boxplot(cbind(breakfast.part.star,breakfast.dat.star),main="breakfast")

boxplot(cbind(desk.part.star,desk.dat.star),main="desk")

boxplot(cbind(wall.part.star,wall.dat.star),main="wall")

boxplot(cbind(parking.part.star,parking.dat.star),main="parking")

boxplot(cbind(bar.part.star,bar.dat.star),main="bar")

boxplot(cbind(downtown.part.star,downtown.dat.star),main="downtown")

boxplot(cbind(restaurant.part.star,restaurant.dat.star),main="restaurant")

boxplot(cbind(location.part.star,location.dat.star),main="location")

boxplot(cbind(clean.part.star,clean.dat.star),main="clean")

boxplot(cbind(comfortable.part.star,comfortable.dat.star),main="comfortable")

boxplot(cbind(spacious.star,spacious.dat.star),main="spacious")

boxplot(cbind(quiet.part.star,quiet.dat.star),main="quiet")

boxplot(cbind(modern.part.star,modern.dat.star),main="modern")

boxplot(cbind(pretty.part.star,pretty.dat.star),main="pretty")

boxplot(cbind(comfy.part.star,comfy.dat.star),main="comfy")

boxplot(cbind(smell.part.star,smell.dat.star),main="smell")

boxplot(cbind(dirty.part.star,dirty.dat.star),main="dirty")

}else{
  print("Not enough reviews from customers")
}

