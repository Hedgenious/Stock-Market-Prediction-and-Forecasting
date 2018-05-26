# Based on Twitter "Sentiment Classification using Distant Supervision" research 
# paper published on Dec 18,2017 cited by 1776 !

#Using sentiment140 project from Stanford University using 1.6 million tweets
#Sentiment140 started as a class project from Stanford University, exploring various aspects of sentiment analysis

#using emoticons as noisy labels for training data is an effective way to perform
#distant supervised learning. Machine learning algorithms (maximum entropy
#classification, and support vector machines) can achieve high accuracy for 
#classifying sentiment when using this method.
#Using Maximum Entropy classifier on 1.6 million tweets

#R package created by authors has sentiment function of sentiment140 model
require(devtools)
#install_github("sentiment140", "okugami79")

library(sentiment)
library(data.table)
library(xts)
sentiments <- sentiment(tweets_nodups_text)
table(sentiments$polarity)
sentiments$score <- 0
sentiments$score[sentiments$polarity == "positive"] <- 1
sentiments$score[sentiments$polarity == "negative"] <- -1
sentiments$date <- as.IDate(tweets.nodups.df$Date)
result <- aggregate(score ~ date, data = sentiments, mean)
plot(result, type = "b")
