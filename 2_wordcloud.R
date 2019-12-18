#------------------------------------------------------------
# Importing the datasets

library(ggplot2)
library(readr)
library(plyr)
library(NLP)
library(tm)
library(splitstackshape)
library(hunspell)
library(wordcloud)
library(SnowballC)
library(RColorBrewer)
library(plotly)

#------------------------------------------------------------
# Working directory

getwd()
# setwd('../Desktop/flight/')

#----------------------------------------------------------------------------
# Read dataset

df<-read.csv('crashes_5600_1908_2009.csv')
print(head(df,4))

#----------------------------------------------------------------------------
# Viewing the data

print(colnames(df))
print(dim(df))
print(head(df,4))
str(df)

#-----------------------------------------------------------------------------
# Preprocessing

df$Date<-as.Date(df$Date,format="%m/%d/%Y")
df$Year<-format(as.Date(df$Date, format="%m-%d-%Y"),"%Y")
df$Month<-format(as.Date(df$Date, format="%m-%d-%Y"),"%m")
df$Day<-format(as.Date(df$Date, format="%m-%d-%Y"),"%d")
df$Weekday<-format(as.Date(df$Date, format="%m-%d-%Y"),"%A")

#------------------------------------------------------------
# Word cloud on crashes happened before 1920

before_1920 <- subset(df, Year < 1920)

text <- Corpus(VectorSource(before_1920$Summary))
toSpace <- content_transformer(function(x, pattern) gsub(pattern," ",x))
text <- tm_map(text, toSpace, "/")
text <- tm_map(text, toSpace, "\\|")
text <- tm_map(text, toSpace, "@")
text <- tm_map(text, content_transformer(tolower))
text <- tm_map(text, removeWords, stopwords("english"))
text <- tm_map(text, removeWords, c("airlines", "aircraft", "near", "plane", "time", "city", "made","making","mexico", "took", "take", "made", "taking", "crashed", "crash", "flight", "flew", "killed", "due","cause", "caused", "result","one", "two"))
text <- tm_map(text, removePunctuation)
text <- tm_map(text, removeNumbers)
text <- tm_map(text, stripWhitespace)
dtm <- TermDocumentMatrix(text)
m <- as.matrix(dtm)
s <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(s), freq = s)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 50, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(10,"Paired"))

#------------------------------------------------------------
# Word Cloud on crashes happened on ground

on_ground <- subset(df, Ground < 1)

text <- Corpus(VectorSource(on_ground$Summary))
toSpace <- content_transformer(function(x, pattern) gsub(pattern," ",x))
text <- tm_map(text, toSpace, "/")
text <- tm_map(text, toSpace, "\\|")
text <- tm_map(text, toSpace, "@")
text <- tm_map(text, content_transformer(tolower))
text <- tm_map(text, removeWords, stopwords("english"))
text <- tm_map(text, removeWords, c("airlines", "aircraft", "near", "plane", "time", "city", "made","making","mexico", "took", "take", "made", "taking", "crashed", "crash", "flight", "flew", "killed", "due","cause", "caused", "result","one", "two"))
text <- tm_map(text, removePunctuation)
text <- tm_map(text, removeNumbers)
text <- tm_map(text, stripWhitespace)
dtm <- TermDocumentMatrix(text)
m <- as.matrix(dtm)
s <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(s), freq = s)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 50, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(10,"Paired"))

#------------------------------------------------------------------
# Word Cloud on crashes happened during training

trn <- subset(df, Route == "Training")

text <- Corpus(VectorSource(trn$Summary))
toSpace <- content_transformer(function(x, pattern) gsub(pattern," ",x))
text <- tm_map(text, toSpace, "/")
text <- tm_map(text, toSpace, "\\|")
text <- tm_map(text, toSpace, "@")
text <- tm_map(text, content_transformer(tolower))
text <- tm_map(text, removeWords, stopwords("english"))
text <- tm_map(text, removeWords, c("airlines", "aircraft", "near", "plane", "time", "city", "made","making","mexico", "took", "take", "made", "taking", "crashed", "crash", "flight", "flew", "killed", "due","cause", "caused", "result","one", "two"))
text <- tm_map(text, removePunctuation)
text <- tm_map(text, removeNumbers)
text <- tm_map(text, stripWhitespace)
dtm <- TermDocumentMatrix(text)
m <- as.matrix(dtm)
s <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(s), freq = s)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 50, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8,"Dark2"))

#------------------------------------------------------------------

