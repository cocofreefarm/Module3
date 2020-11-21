#Loading data
data = read.csv(file = 'word embedding dataset_excluding repeats.csv',header = T)
data$stars = as.factor(data$stars)
data = data[1:10,1:5]

#Building model
require('mlogit')
mdata = mlogit.data(data = data,shape = 'long',alt.var = 'stars',id.var = 'X')
head(mdata,10)
