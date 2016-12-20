data<-read.csv("movie_metadata.csv",stringsAsFactors = F)
imdb_movie<-readline(prompt="Enter movie imdb url")
int_data<-data[,sapply(data,is.numeric)]
int_data[is.na(int_data)]<-0

row_no<-which(data$movie_imdb_link==imdb_movie)
search_data_feature<-int_data[row_no,]
x<- as.matrix(int_data)
y<- as.matrix(search_data_feature)
y1<-as.vector(y)
library('lsa')
library('data.table')
library('fields')
ans_cosine<-c()
ans_euclidean<-c()
print(data[1,"movie_title"])
for (i in 1:nrow(x)){
  
  cosine_dist<-lsa::cosine(as.vector(x[i,]),as.vector(y1))
  euclidean<-dist(rbind(as.vector(x[i,]),as.vector(y1)))
  yans<-c(i,cosine_dist)
 ans_cosine<-rbind(ans_cosine,yans)
 yans<-c(i,euclidean)
 ans_euclidean<-rbind(ans_euclidean,yans)
 
}

ans_sort<-ans_cosine[order(ans_cosine[,2],decreasing=T),]
ans_sort_euc<-ans_euclidean[order(ans_euclidean[,2],decreasing = T),]
list_ans<-c()
for (i in 2:6){
  print (data[ans_sort[[i,1]],"movie_title"])
}
for (i in 2:6){
  print (data[ans_sort_euc[[i,1]],"movie_title"])
}



