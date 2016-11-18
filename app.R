library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tm)
library(tidytext)
library(MASS)

project<-read.csv("Jetblue_Project_5000.csv",header = TRUE,sep=",")
tweets.df<-project

ui<- fluidPage (
  tags$h3("TWITTER SENTITMENT ANALYSIS@LKT"),
  tags$a(href="https://www.twitter.com","TWITTER"),
  img(src = "my_image.png",
       width = 35,
       height = 35),
  
  textOutput("currentTime"),   #Here, I show a real time clock
  h4("Tweets:"),
#  sliderInput("freq",
 #             "Minimum Frequency:",
  #            min = 1,  max = 50, value = 15),
  #sliderInput("max",
   #           "Maximum Number of Words:",
    #          min = 1,  max = 300,  value = 100),
  
  tabPanel("Plot",
           fluidRow( column(12, plotOutput("Sentiment_analysis"))),
           fluidRow(
             column(2,sliderInput("freq",
                                  "Minimum Frequency:",
                                  min = 1,  max = 50, value = 15)),
             column(2, sliderInput("max",
                                   "Max Num of Words:",
                                   min = 1,  max = 300,  value = 100)),
             column(8, plotOutput("tweet_plot")))
             
           #  column(4, plotOutput("Sentiment_analysis"))),
            # column(4, plotOutput("tweets_plot"))
            #fluidRow( column(8, plotOutput("tweet_plot")))
))


server<-function(input,output)
{
  output$tweet_plot<-renderPlot(
    { str(tweets.df)
      library(tm)
      tweets_corpus<- VCorpus(VectorSource(tweets.df$text))
      
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
      
      tweets_corpus_clean<-sapply(tweets_corpus, function(x) tryTolower(x))
      
      tweets_corpus_clean<- Corpus(VectorSource(tweets_corpus_clean))
      tweets_corpus_clean<- tm_map(tweets_corpus_clean,content_transformer(tolower))
      str(tweets_corpus_clean)
      
      as.character(tweets_corpus[[12]])
      as.character(tweets_corpus_clean[[12]])
      
      tweets_corpus_clean<- tm_map(tweets_corpus_clean,removeNumbers)
      
      as.character(tweets_corpus[[11]])
      as.character(tweets_corpus_clean[[11]])
      
      tweets_corpus_clean<-tm_map(tweets_corpus_clean,removeWords,stopwords())
      
      as.character(tweets_corpus[[11]])
      as.character(tweets_corpus_clean[[11]])
      
      replacePunctuation<- function(x){
        gsub("[[:punct:]]+"," ",x)  
      }
      
      replacePunctuation("Give me a .. break")
      
      tweets_corpus_clean<-tm_map(tweets_corpus_clean,content_transformer(replacePunctuation))
      
      as.character(tweets_corpus[[11]])
      as.character(tweets_corpus_clean[[11]])
      
      library(SnowballC)
      tweets_corpus_clean<-tm_map(tweets_corpus_clean, stemDocument,language="english")
      
      as.character(tweets_corpus[[29]])
      as.character(tweets_corpus_clean[[29]])
      
      tweets_corpus_clean<-tm_map(tweets_corpus_clean,stripWhitespace)
      
      as.character(tweets_corpus[[6]])
      as.character(tweets_corpus_clean[[6]])
      
      
      tweets_dtm<-DocumentTermMatrix(tweets_corpus_clean)
      
      
      library(wordcloud)
      
      wordcloud(tweets_corpus_clean,min.freq =input$freq,max.words=input$max,random.order = FALSE,scale=c(10, .8),colors=brewer.pal(6, "Dark2"))
      
    })
  
  output$Sentiment_analysis<-renderPlot({
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
    teek<-function(x){
      teekcorpus<-VCorpus(VectorSource(x))
      teekcorpus<-sapply(teekcorpus, function(x) tryTolower(x))
      teekcorpus<-VCorpus(VectorSource(teekcorpus))
      teekcorpus<-tm_map(teekcorpus,content_transformer(tolower))
      teekcorpus<-tm_map(teekcorpus,removeNumbers)
      teekcorpus<-tm_map(teekcorpus,removeWords,stopwords())
      teekcorpus<-tm_map(teekcorpus,removePunctuation)
      teekcorpus<-tm_map(teekcorpus, stemDocument,language="english")
      teekcorpus<-tm_map(teekcorpus,stripWhitespace)
      teekcorpus<-DocumentTermMatrix(teekcorpus)
      teekcorpus<-tidy(teekcorpus)
      bing <- sentiments %>%
        filter(lexicon == "nrc") %>%
        dplyr::select(word, sentiment)
      teekcorpus <- teekcorpus %>%
        inner_join(bing, by = c(term = "word"))
      teekcorpus$sentiment <- as.character(teekcorpus$sentiment)
      teekcorpus$positive <- ifelse(teekcorpus$sentiment == "positive",as.numeric(1),0)
      teekcorpus$negative <- ifelse(teekcorpus$sentiment == "negative",as.numeric(1),0)
      teekcorpus$trust <- ifelse(teekcorpus$sentiment == "trust",as.numeric(1),0)
      teekcorpus$anger <- ifelse(teekcorpus$sentiment == "anger",as.numeric(1),0)
      teekcorpus$anticipation <- ifelse(teekcorpus$sentiment == "anticipation",as.numeric(1),0)
      teekcorpus$disgust <- ifelse(teekcorpus$sentiment == "disgust",as.numeric(1),0)
      teekcorpus$fear <- ifelse(teekcorpus$sentiment == "fear",as.numeric(1),0)
      teekcorpus$joy <- ifelse(teekcorpus$sentiment == "joy",as.numeric(1),0)
      teekcorpus$sadness <- ifelse(teekcorpus$sentiment == "sadness",as.numeric(1),0)
      teekcorpus$surprise <- ifelse(teekcorpus$sentiment == "surprise",as.numeric(1),0)  
      teekcorpus<-teekcorpus%>%
        dplyr::select(document,positive,negative,trust,anger,anticipation,disgust,fear,joy,sadness,surprise)%>%
        group_by(document)%>%
        summarise(positive=sum(positive),negative=sum(negative),trust=sum(trust),anger=sum(anger),
                  anticipation=sum(anticipation),disgust=sum(disgust),fear=sum(fear),joy=sum(joy),
                  sadness=sum(sadness),surprise=sum(surprise),positive_negative_total=positive+negative,emotions_total=sum(trust+anger+anticipation+disgust+fear+joy+sadness+surprise))
      teekcorpus<-teekcorpus%>%
        group_by(document)%>%
        summarise(positive=(positive/(positive_negative_total)*100),negative=(negative/(positive_negative_total)*100),
                  trust=((trust/emotions_total)*100),
                  anger=((anger/emotions_total)*100),
                  anticipation=((anticipation/emotions_total)*100),
                  disgust=((disgust/emotions_total)*100),
                  fear=((fear/emotions_total)*100),
                  joy=((joy/emotions_total)*100),
                  sadness=((sadness/emotions_total)*100),
                  surprise=((surprise/emotions_total)*100))
      teekcorpus[is.na(teekcorpus)]<-0  
      print(teekcorpus)
      #Gather the Data Columns, ie : Transform data wide to long  
      teekcorpus<-gather(teekcorpus,POSITIVE_NEGATIVE,POSITIVE_NEGATIVE_PERCENT,
                         c(-document,-trust,-anger,-anticipation,-disgust,-fear,-joy,-sadness,-surprise))  
      teekcorpus<-gather(teekcorpus,EMOTIONS,EMOTIONS_PERCENT,c(-document,-POSITIVE_NEGATIVE,-POSITIVE_NEGATIVE_PERCENT))
      #Save data into 2 plots  
      Plot1<-ggplot(teekcorpus, aes(EMOTIONS, EMOTIONS_PERCENT)) +   
        geom_bar(aes(fill = EMOTIONS_PERCENT), position = "dodge", stat="identity")
      
     Plot2<-ggplot(teekcorpus, aes(POSITIVE_NEGATIVE,POSITIVE_NEGATIVE_PERCENT)) +   
     geom_bar(aes(fill = POSITIVE_NEGATIVE_PERCENT), position = "dodge", stat="identity",width = .3)  
      
      #Create MultiPlot Function
      multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
        library(grid)
        
        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        # If layout is NULL, then use 'cols' to determine layout
        if (is.null(layout)) {
          # Make the panel
          # ncol: Number of columns of plots
          # nrow: Number of rows needed, calculated from # of cols
          layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                           ncol = cols, nrow = ceiling(numPlots/cols))
        }
        
        if (numPlots==1) {
          print(plots[[1]])
          
        } else {
          # Set up the page
          grid.newpage()
          pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
          
          # Make each plot, in the correct location
          for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
          }
        }
      }
      multiplot(Plot1,Plot2)
    }
    
    
    x<-tweets.df
    teek(x)
    
  
  })
}
   


shinyApp(ui,server=server)
