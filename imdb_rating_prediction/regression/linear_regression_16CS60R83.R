data<-read.csv("movie_metadata.csv",stringsAsFactors=F, na.strings=c("","NA"))
data<-data[,sapply(data,is.numeric)]
data[is.na(data)]<-0
train_size<-floor(0.75*nrow(data))
set.seed(190)
library(lattice)
library(Metrics)

train_ind<-sample(seq_len(nrow(data)), size = train_size)
train_data<-data[train_ind,]
test_data<-data[-train_ind,]

formula<-imdb_score~num_critic_for_reviews + duration + director_facebook_likes+actor_3_facebook_likes+actor_1_facebook_likes+gross+num_voted_users+cast_total_facebook_likes+facenumber_in_poster+num_user_for_reviews+budget+actor_2_facebook_likes+movie_facebook_likes
model<-glm(formula=formula, data=train_data)
#plot(model)
to_be_predicted=test_data[,c(1:13,15,16)]
predictedValues <- predict(model, newdata=to_be_predicted)
plot(predictedValues, test_data$imdb_score,bg="yellow", cex=0.5, pch=21)
lines(predictedValues, col="blue", lwd=2)
#table(predictedValues, test_data$imdb_score)
rmse(test_data$imdb_score,predictedValues) #rmse value printed
