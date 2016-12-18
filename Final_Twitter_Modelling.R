### Model based on Naive Bayes and SVM Using WeightBin : 

libs<-c("tm","dplyr","SnowballC","caret")
lapply(libs,library,character.only=TRUE)


library(caret)
library(SnowballC)
set.seed(123)
Tweets_Training<-read.csv("Tweets_Training.csv",header=TRUE,sep = ",")
tweets1<-subset(Tweets_Training,Tweets_Training$airline_sentiment %in% c("positive","negative"))
tweets_train<- as.matrix(tweets1)
head(tweets_train)

indexes<-createDataPartition(tweets_train[,1],p=.7,list=FALSE)
train.data<-tweets_train[indexes,]
test.data<-tweets_train[-indexes,]
prop.table(table(tweets1[,1]))
prop.table(table(train.data[,1]))
prop.table(table(test.data[,1]))

nrow(tweets1)
nrow(train.data)
as.data.frame(train.data)
nrow(test.data)
head(train.data)

Corpus1<-Corpus(VectorSource(train.data[,2]))
tryTolower = function(x)
{
  # create missing value
  # this is where the returned value will be
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}


tweets_lower<-sapply(Corpus1, function(x) tryTolower(x))
Corpus1_clean<- VCorpus(VectorSource(tweets_lower))
## Corpus1_clean<- tm_map(Corpus1_clean,content_transformer(tolower))

tweets_rm_number<- tm_map(Corpus1_clean,removeNumbers)

tweets_stop_words<-tm_map(tweets_rm_number,removeWords,stopwords())

tweets_punc_rm<-tm_map(tweets_stop_words,content_transformer(removePunctuation))

tweets_stem<-tm_map(tweets_punc_rm, stemDocument,language="english")
#copy<-tweets_stem
#stemCompletion_mod <- function(x,dict=copy) {
#  PlainTextDocument(stripWhitespace(paste(stemCompletion(unlist(strsplit(as.character(x)," ")),dictionary=copy, type="prevalent"),sep="", collapse=" ")))
#}

#tweets_corpus_clean<- lapply(tweets_stem, stemCompletion_mod,copy)

tweets_final<-tm_map(tweets_stem,stripWhitespace)


----------------------
  
Corpus2<-Corpus(VectorSource(test.data[,2]))
tryTolower = function(x)
{
  # create missing value
  # this is where the returned value will be
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
  
}


tweets_lower<-sapply(Corpus2, function(x) tryTolower(x))
Corpus2_clean<- VCorpus(VectorSource(tweets_lower))
#tweets_corpus_clean<- tm_map(tweets_corpus_clean,content_transformer(tolower))

tweets_rm_number<- tm_map(Corpus2_clean,removeNumbers)

tweets_stop_words<-tm_map(tweets_rm_number,removeWords,stopwords())

tweets_punc_rm<-tm_map(tweets_stop_words,content_transformer(removePunctuation))

tweets_stem<-tm_map(tweets_punc_rm, stemDocument,language="english")

tweets_final2<-tm_map(tweets_stem,stripWhitespace)

---------------------------------
  
  
Train_dtm<-DocumentTermMatrix(tweets_final,control=list(weighting=weightBin))
train_dtm_sparse_99<-removeSparseTerms(Train_dtm,0.995)
dim(train_dtm_sparse_99)
## for using for later training purposes :
train_bag_99<-findFreqTerms(train_dtm_sparse_99)
train_bag_99

##Document term matrix to a normal matrix :

train_dtm_sparse_99<-as.matrix(train_dtm_sparse_99)

mean_train_99<-sort(colMeans(train_dtm_sparse_99),decreasing = T)

avg_train_99<-mean(mean_train_99[1:20])

## Plotting it :)
barplot(mean_train_99[1:20],border=NA,las=3,xlab="freq terms",ylab="average binary weights",
        ylim=c(0,1))

## COMBINE SENTIMENT AND DOCUEMNTS_COLUMN
alpha<-as.data.frame(train.data)
train_data_99<- data.frame(Sentiments=alpha$airline_sentiment,train_dtm_sparse_99 )
dim(train_data_99)


===================
  
  #Now convert this test data as well into the same format :)
  
Test_dtm<-DocumentTermMatrix(tweets_final2,control=list(weighting=weightBin,
                                                          dictionary=train_bag_99))
inspect(Test_dtm[1:10,2:20])

Test_Matrix_99<-as.matrix(Test_dtm)
beta<-as.data.frame(test.data)

Test_comb_data<-data.frame(Sentiments=beta$airline_sentiment, Test_Matrix_99)
dim(Test_comb_data)
dim(train_data_99)

=================================
library(e1071)
library(kernlab)

bow_svm<-ksvm(Sentiments~.,data=train_data_99)
test1_predict<-predict(bow_svm,newdata=Test_comb_data)

Conf_SVM<-confusionMatrix(test2_predict,Test_comb_data[,1],positive="positive",
                          dnn=c("Prediction","TRUE"))
Conf_SVM

-------------------------------------------------------------
  
## Naive Bayes Classifier :
  
bow_nb<-naiveBayes(Sentiments~.,data = train_data_99)
test2_predict<-predict(bow_nb,newdata=Test_comb_data)
Conf_NB<-confusionMatrix(test2_predict,Test_comb_data[,1],positive = "positive",
                         dnn=c("Prediction","True"))
Conf_NB

---------------------------
  
## Our Project_Data Prediction : 
  
project_earlier<-read.csv("Jetblue_Project_5000.csv",header = TRUE,sep=",")

project<-read.csv("cleaned_tweets.csv",header = TRUE,sep=",")

str(project)
colnames(project)
## KNN clustering algorigthm :

project1<-as.data.frame(project[,2])
colnames(project1)<-c("Tweet")
colnames(project1)
head(project1)


tryTolower = function(x)
{
  # create missing value
  # this is where the returned value will be
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}
teek<-function(x) {
  teekcorpus<-VCorpus(VectorSource(x))
  teekcorpus<-sapply(teekcorpus, function(x) tryTolower(x))
  teekcorpus<-VCorpus(VectorSource(teekcorpus))
  teekcorpus<-tm_map(teekcorpus,content_transformer(tolower))
  teekcorpus<-tm_map(teekcorpus,removeNumbers)
  teekcorpus<-tm_map(teekcorpus,removeWords,stopwords())
  teekcorpus<-tm_map(teekcorpus,removePunctuation)
  teekcorpus<-tm_map(teekcorpus, stemDocument,language="english")
  teekcorpus<-tm_map(teekcorpus,stripWhitespace)
  
}

library(tm)
live_corpus<-Corpus(VectorSource(project[,2]))
tweets_Final<-teek(live_corpus)
str(tweets_Final)

DTM_FINAL<-DocumentTermMatrix(tweets_Final,control=list(weighting=weightBin,
                                                        dictionary=train_bag_99))
inspect(DTM_FINAL[1:10,2:20])

DTM_FINAL<-as.matrix(DTM_FINAL)
DTM_FINAL<-as.data.frame(DTM_FINAL)


Final_Predict_NB<-predict(bow_nb,newdata=DTM_FINAL)
table(Final_Predict_NB)


bow_svm<-ksvm(Sentiments~.,data=train_data_99)
test1_predict<-predict(bow_svm,newdata=Test_comb_data)

Conf_SVM<-confusionMatrix(test2_predict,Test_comb_data[,1],positive="positive",
                          dnn=c("Prediction","TRUE"))
Conf_SVM

Final_Predict_SVM<-predict(bow_svm,newdata=DTM_FINAL)
table(Final_Predict_NB)

Conf_NB

---------------------
head(train_data_99)
train_Data_new<-train_data_99
train_Data_new$Sentiments<-ifelse(train_Data_new$Sentiments=="negative",0,1)

head(train_Data_new$Sentiments,10)

--------------------------------------------------
  
glm_model<-glm(Sentiments~.,family=binomial(link=logit),data=train_Data_new)

test2_predict<-predict(glm_model,newdata=Test_comb_data)

test3_predict<-predict(glm_model, newdata = Test_comb_data,
                       type ="response")
test3_predict<-ifelse(test3_predict>=.5,1,0)
table(Test_comb_data$Sentiments)

Final_Predict_GLM<-predict(glm_model,data=DTM_FINAL)


Final_Predict_GLM<-ifelse(Final_Predict_GLM>=.5,1,0)  
table(Final_Predict_GLM)

