if(!require(tidyverse)){
  install.packages("tidyverse")
}
if(!require(tidytext)){
  install.packages("tidytext")
}
if(!require(reshape)){
  install.packages("reshape")
}

data = read_csv("Data/df.csv")
review = read_csv("Data/wordembedding.csv")

sentiments = get_sentiments('afinn')
words = colnames(review)[4:dim(review)[2]]
for (i in 1:300) {
  words[i] %in% sentiments$word
}

sentimentscore = c()
for (i in 1:length(review$X1)) {
  single_review = review[i,]
  single_review = as.data.frame(single_review[,4:dim(single_review)[2]])
  single_review = data.frame("word" = colnames(single_review),
                             "counts" = as.numeric(single_review[1,]))
  
  single_review = left_join(single_review, sentiments, by = "word")
  single_review = single_review[!is.na(single_review$value),]
  sentimentscore[i] = sum(single_review$counts*single_review$value)
  
}

review.sentiments = cbind(review[,3],sentimentscore)

for (i in 1:dim(review.sentiments)[1]) {
  if (review.sentiments$sentimentscore[i] > 0) {
    review.sentiments$sentiment[i] = "Positive"
  } else {
    review.sentiments$sentiment[i] = "Negative"
  }
}

sentiments.count = review.sentiments %>% group_by(name) %>% summarise(P = sum(sentiment=="Positive"),
                                                   N = sum(sentiment=="Negative"))
write.csv(sentiments.count, file = "sentiments.csv", row.names = F)
