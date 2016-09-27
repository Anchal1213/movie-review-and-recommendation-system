library(data.table)
library(recommenderlab)
library(rvest)

setwd("C:/Users/user/Desktop/college/Movie Project/ml-latest-small")
links=read.csv("links.csv",header=T)
f_comments=read.csv("f_comments.csv",header=T)


####MOVIES DATASET###########
movies<-read.csv("movies.csv",header=T)
#split year from movie name into new column
movies$title<-as.character(movies$title)
movies$year=NA
movies$year=strsplit(movies$title,"[()]",fixed=FALSE)
for(i in 1:nrow(movies)){
  movies[i,4]=movies[i,4][[1]][2]
}
movies$year<-as.numeric(movies$year)

#split movie name into column
movies$title=strsplit(movies$title,"[()]",fixed=FALSE)
for(i in 1:nrow(movies)){
  movies[i,2]=movies[i,2][[1]][1]
}
movies$title<-as.character(movies$title)

#creating a vector of genres seperated by |
movies$genres<-as.character(movies$genres)
movies$genres=strsplit(movies$genres,"[|]")
for(i in 1:nrow(movies)){
  movies[i,2]=movies[i,2][[1]][1]
}

#create a dataframe for all genres and their corresponding movie Id
abc<-data.frame()
for (i in 1:nrow(movies)){
  genres<- as.data.frame(c(movies$movieId[i],movies$genres[i][1]))
  colnames(genres)<-c("movieId","X")
  abc<-rbind(abc,genres)
}
abc[,2]<-as.character(abc[,2])

#create dummy variables for all unique genres and cbind them with corresponding movies
y=as(abc,"realRatingMatrix")
z=as(y,"matrix")
z[is.na(z)]=0
movies=cbind(movies,z)
movies$genres=NULL
write.csv(movies,"Movie.csv",row.names = F)


####WEB EXTRACTION#####

#####Out url of all movies in a vector
a="http://www.imdb.com/title/tt"
b=as.vector(links$imdbId)
c<-"/reviews"
url=c()
for(i in 1:nrow(links)){
  x<-paste(a,links[i,2],c,sep="")
  url=c(url,x)
}
length(url)

####comments extraction from imdb and create data frame of movie id and corresponding comments#
comments=data.frame()
f_comments=data.frame()
x=links$movieId
for( i in 1:lenth(x)){
  movie=read_html(url[i])
  nodes=html_nodes(movie,"h2")
  text=html_text(nodes)
  for (j in 1:length(text)){
    comments[j,1]=x[i]
    comments[j,2]=text[j]
  }
  f_comments=rbind(final_comments,comments)
}

write.csv(f_comments,"f_comments.csv",row.names = F)

#giving user details to comments#
f_comments$userId=NA
for(i in 1:length(movie_id)){
  x=subset(ratings,subset=(movieId==movie_id[i]),select=userId)
  f_comments$userId[f_comments$Movie_ID==movie_id[i]]= x[1:10,]
}
movie_id[10328]
range(ratings$userId)
table(is.na(f_comments$userId))
y=669:1000
f_comments$userId[is.na(f_comments$userId)]=y


####SENTIMENT ANALYSIS#########

#create function for cleaning comments, creating bag of words and calculating score
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)

  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # remove punctuation, whitespace:
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # bag of words
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    #TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

#read the dictionaries of positve and negative words

hu.li.neg<- scan(file="negative-words.txt",what='character',comment.char = ';')
hu.li.pos<- scan(file="positive-words.txt",what='character',comment.char = ';')
scores = score.sentiment(f_comments$Review, pos.words = hu.li.pos, neg.words = hu.li.neg, .progress = 'text')
f_comments$score<- scores$score

#classify comments as positve, neg and neutral based on score
for(i in 1:nrow(f_comments)){
  if(f_comments[i,3]<0){
    f_comments[i,4]<- "negative"
  }
  else if(f_comments[i,3]>0){
    f_comments[i,4]<- "positive"
  }
  else{
    f_comments[i,4]<- "neutral"
  }
}
names(f_comments)[4]<- "sentiment"
write.csv(f_comments,"f_comments.csv",row.names = F)


####FINAL REVIEW############
#function for mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


y=unique(f_comments$Movie_ID)
#create dataset of mean and mode of each movie reviews
Final_review<-data.frame()
for( i in 1:length(y)){
  Final_review[i,'Movie_Id']=y[i]
  a<-subset(ratings, subset=(ratings$movieId==y[i]),select=rating)
  Final_review[i,'Star_rating']=mean(a[,1])
  b=subset(f_comments,f_comments$Movie_ID==y[i],select=sentiment)
  Final_review[i,'Comment']=getmode(b[,1])
}


#treating NAs of ratings column
table(is.na(Final_review))
summary(Final_review)
subset(Final_review,is.na(Final_review$Star_rating))
Final_review$Star_rating[is.na(Final_review$Star_rating) & Final_review$Comment=="positive"]<-4
Final_review$Star_rating[is.na(Final_review$Star_rating) & Final_review$Comment=="negative"]<-1


#creating final review by scaling
Final_review$final_review=NA
for(i in 1:nrow(Final_review)){
  if(Final_review[i,3]=="positive"){
    Final_review[i,4]=Final_review[i,2]+0.5
  }
  else if(Final_review[i,3]=="negative"){
    Final_review[i,4]=Final_review[i,2]-0.5
  }
  else{
    Final_review[i,4]=Final_review[i,2]
  }
}


write.csv(Final_review,"Final_Review.csv",row.names = F)


####RECOMMENDATION#########################
setwd("C:/Users/user/Desktop/college/Movie Project/ml-latest-small")
movies=read.csv("Movie.csv")
rating=read.csv("ratings.csv")

#merge the ratiing and movie file to display movie name
user_rate=merge(rating,movies, by="movieId",all.x=T)
#reorder data
user_rate=data.frame("userId"=user_rate$userId,"title"=user_rate$title,"rating"=user_rate$rating)

#convert to matrix
rating.matrix<- as(user_rate,"realRatingMatrix")

#appplying user based collaborative filtering for 668 users in data
Rec.model<-Recommender(rating.matrix[1:668], method = "UBCF")

#predict 5 recommendations for 1st user and store in data frame
recommended.items <- predict(Rec.model, rating.matrix[as.character(1),], n=5)
x=as(recommended.items, "list")
y=data.frame(x)

#loop for all other users
for(i in 2:668){
  recommended.items <- predict(Rec.model, rating.matrix[as.character(i),], n=5)
  x=as(recommended.items, "list")
  y=cbind(y,x)}


# to obtain the top 3(in this case for last user)
recommended.items.top3 <- bestN(recommended.items, n = 3)
# to display them
as(recommended.items.top3, "list")

#prettify the recommendation data and write it
z=transpose(y)
names(z)=c("Movie 1","Movie 2","Movie 3","Movie 4","Movie 5")
i=row.names(z)
z=cbind("user_Id"=i,z)
write.csv(z,"recommendations.csv",row.names=F)

#check accuracy#
e <- evaluationScheme(rating.matrix[1:668], method="split", train=0.9, given=15)
# creation of recommender model based on ubcf
Rec.ubcf <- Recommender(getData(e, "train"), "UBCF")
# making predictions on the test data set
p.ubcf <- predict(Rec.ubcf, getData(e, "known"), n=5)
error.ubcf<-calcPredictionAccuracy(p.ubcf, getData(e, "unknown"))
error.ubcf
