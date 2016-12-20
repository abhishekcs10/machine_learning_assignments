data<-read.csv("movie_metadata.csv",stringsAsFactors = F,na.strings=c("","NA"))
ac_di<-data[,c("actor_1_name","director_name","movie_imdb_link")]
ac_di2<-data[,c("actor_2_name","director_name","movie_imdb_link")]
colnames(ac_di2)[1]<-"actor_1_name"
ac_di3<-data[,c("actor_3_name","director_name","movie_imdb_link")]
colnames(ac_di3)[1]<-"actor_1_name"
ac_di<-rbind(ac_di,ac_di2)
ac_di<-rbind(ac_di,ac_di3)
ac_di["j_coefficient"]<-NA
j_values<-c()
for (i in 1:nrow(ac_di)){
A<-as.vector(subset(ac_di,director_name==ac_di[i,2],select=c(movie_imdb_link)))
A<-A[[1]]
A<-unique(A)
B<-as.vector(subset(ac_di,actor_1_name==ac_di[i,1],select=c(movie_imdb_link)))
B<-B[[1]]
B<-unique(B)
jacard<-length(intersect(A,B))/length(union(A,B))
ac_di[i,4]<-jacard
}
ac_di<-ac_di[order(ac_di[,4],decreasing = T),]
print(ac_di[1:5,1:2])
