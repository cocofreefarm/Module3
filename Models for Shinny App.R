dat <- read.csv("Data/df.csv",header=TRUE)
star.dat <- dat[,c(2,5,11:dim(dat)[2])]

wordembedding.dat <- star.dat[,-c(1,2)]
for (i in 1:dim(wordembedding.dat)[2]){
  wordembedding.dat[which(wordembedding.dat[,i] >=1),i]=1
}

words <- apply(wordembedding.dat[,1:dim(wordembedding.dat)[2]],2,sum)

stars <- star.dat[,1]
name <- star.dat[,2]
wordembedding.dat <- wordembedding.dat[names(sort(words[words>summary(words)[5]], decreasing= TRUE))]
wordembedding.dat <- cbind(stars,name,wordembedding.dat)
wordembedding.dat$stars <- as.factor(wordembedding.dat$stars)

sort(words[words>summary(words)[5]], decreasing= TRUE)


avg.hotel <- sort(tapply(as.numeric(wordembedding.dat$stars),wordembedding.dat$name,mean))
hotel.names <- names(avg.hotel)

if (avg.hotel[1]<mean(avg.hotel)){
  print(paste(hotel.names[30],"obtained",mean(avg.hotel),"average rate which is below the average rate of hotels in Madison Area"))
}else{
  print(paste(hotel.names[30],"obtained",mean(avg.hotel),"average rate which is above the average rate of hotels in Madison Area"))
}

print(paste(hotel.names[30],"got",length(which(wordembedding.dat$name==hotel.names[30])),"reviews from the customers"))

barplot(table(wordembedding.dat$stars[which(wordembedding.dat$name==hotel.names[30])]), xlab="Stars",ylab="Number of Review", main=hotel.names[30])

if(length(which(wordembedding.dat$name==hotel.names[30]))>5){

  part.dat <- wordembedding.dat[which(wordembedding.dat$name==hotel.names[30]),]
  
  part.words <- apply(part.dat[,3:dim(part.dat)[2]],2,sum)
  part.freq <- names(sort(part.words, decreasing= TRUE)[1:30])
  
  print(paste("Most frequently appeared 30 words in reviews for",hotel.names[30],"are"))
  print(sort(part.words, decreasing= TRUE)[1:30])
  
  box.evaluation <- function(argument1){
    ind <- argument1
    index <- which(part.dat[ind]==1)
    part.star <- as.numeric(part.dat[index,1])
    mean(part.star)
    index2 <- which(betterhotel.dat[ind]==1)
    word.star <- as.numeric(wordembedding.dat[index2,1])
    mean(word.star)
        
  }

service.avg <- mean(as.numeric(wordembedding.dat$stars[which(part.dat[,c("booked","money","staff","manager","breakfast","desk")]==1)]))
facility.avg <- mean(as.numeric(wordembedding.dat$stars[which(part.dat[,c("wall","parking")]==1)]))
location.avg <- mean(as.numeric(wordembedding.dat$stars[which(part.dat[,c("bar","downtown","restaurant","location")]==1)]))
atmosphere.avg <- mean(as.numeric(wordembedding.dat$stars[which(part.dat[,c("clean","comfortable","spacious","quiet","smell","modern","pretty","comfy","dirty")]==1)]))

print(paste(hotel.names[30],"obtained",round(service.avg,1),"star rate for service"))
print(paste(hotel.names[30],"obtained",round(facility.avg,1),"star rate for facility"))
print(paste(hotel.names[30],"obtained",round(location.avg,1),"star rate for location"))
print(paste(hotel.names[30],"obtained",round(atmosphere.avg,1),"star rate for atmosphere"))

service <- c("booked","money","staff","manager","breakfast","desk")
facility <- c("wall","parking")
location <- c("bar","downtown","reataurant","location")
atmosphere <- c("clean","comfortable","spacious","quiet","smell","modern","pretty","comfy","dirty")



boxplot(cbind(star,better.star),main=paste(ind,'.star comparison between high rated hotel and low rated hotel'))
t.test(ac.star,better.star)     



}else{
  print("Not enough reviews from customers")
}

