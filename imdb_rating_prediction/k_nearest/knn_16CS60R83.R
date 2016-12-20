library(class)
library(gmodels)
set.seed(100)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

data<-read.csv("movie_metadata.csv",stringsAsFactors=F,na.strings=c("","NA"))
data[is.na(data)]<-0

int_data<-data[,sapply(data,is.numeric)]
int_data <- as.data.frame(lapply(int_data, normalize))
train_size<-floor(0.75*nrow(data))
#train_ind<-sample(seq_len(nrow(int_data)), size = train_size)
train_data<-int_data[1:train_size,]
train_labels<-sapply(strsplit(data[1:train_size,"genres"],"\\|"),`[[`,1)
test_data<-int_data[(train_size+1):nrow(data),]
test_labels<-sapply(strsplit(data[(train_size+1):nrow(data),"genres"],"\\|"),`[[`,1)
km=floor(sqrt(nrow(int_data)))
prediction<-knn(train=train_data,test=test_data,cl=train_labels,k=km)
confuse<-CrossTable(x=test_labels,y=prediction,prop.chisq=F)
value_confuse<-confuse[1]
value_confuse<-value_confuse[[1]]
sumd<-0
total<-0
for (i in 1:nrow(value_confuse)){
  for(j in 1:ncol(value_confuse)){
    total=total+value_confuse[i,j]
    if(rownames(value_confuse)[i]==colnames(value_confuse)[j])
    {
      sumd=sumd+value_confuse[i,j]
    }
  }
}
accuracy<-(sumd/total)
print(accuracy*100)
