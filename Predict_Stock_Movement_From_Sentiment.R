#STOCK CORRELATION PREDICTION USING TWITTER SENTIMENT ANALYSIS

#Train a few word embeddings on financial news, tech news, etc, to be used in 
#conjunction with the timeseries models  

library("twitteR")
library("httr")
library("stringr")
library(ROAuth)
library("plyr")
library("dplyr")
library("NLP")
library(wordcloud2)
library("tm")
library("wordcloud")
library(plotrix)
library(ggplot2)
library(lattice)

# we are using the setup_twitter_oauth function
setup_twitter_oauth
#key = "Nq6GOpwZQLnn5ejAwAccjfB9x", secret =  "NtmKpPAdrVSERXD8HitoJ1FsotYhHblVVH8KC2maUWnrKMqFxZ", mytoken = "864348250970529792-DS6osUNW9fcSATzr5A76Ia4QsPEtiCf", secrettoken =  "er0x8fwaktyQQaYEr50L5dwe7YoNd3fdEsVTpR0Y96Mrc"
key = 'uZzYLJklo7eFh1JAVl3wfMxCS'
secret = 'I0KKzVNhZGB7cTKgSVIPdHgLbAZbwyugs6GGNmEH4t5RaWHSQL'
mytoken = '3298766293-8G3JGTU3gzKMSnDh6BNwWkem5gzK9JnBwIzRrpj'
secrettoken = 'jQZju6981NvUr9DCquPh8ReSvzjlYPLm0QFMwKbff7zfa'
# keep this order of arguments
setup_twitter_oauth(key, secret, mytoken, secrettoken)

#Extracting tweets eg..
 
tweets = searchTwitter("$AAPL OR @Apple OR @AppStore OR @AppleMusic OR @AppleSupport OR @AppleEDU", n=9000, since="2017-12-31", resultType = 'mixed', lang="en")
#convert list to data frame
(n.tweet <- length(tweets))
tweets.df <- twListToDF(tweets)
head(tweets)

tweets.nodups.df <- distinct(tweets.df, text, .keep_all =
                               TRUE)
tweets.nodups.df$text <- gsub('…', '',
                              tweets.nodups.df$text)
tweets.nodups.df <- plyr::rename(tweets.nodups.df,
                                 c("created" = "Date")) #rename created to Date
tweets.nodups.df$Date <- as.Date(tweets.nodups.df$Date)
#convert from datetime to date format

tweets_text <- lapply(tweets, function(x) x$getText())
#fix Mac encoding issue with
tweets_text <- sapply(tweets_text,function(row)
  iconv(row, "latin1", "ASCII", sub='byte'))
 
#removing duplicate tweets (retweets) from list
tweets_nodups_text <- unique(tweets_text)

usableText=str_replace_all(tweets_text,"[^[:graph:]]", " ") 
#create text list with tweets for sentiment analysis
#Create tweet corpus
r_stats_text_corpus <-
  Corpus(VectorSource(tweets_nodups_text))
#Clean up corpus in prepartion for word cloud
#Encoding corrections for Mac
usableText=str_replace_all(r_stats_text_corpus,"[^[:graph:]]", " ") 

r_stats_text_corpus <- tm_map(r_stats_text_corpus,
                              content_transformer(function(x) iconv(x, to="latin1", "ASCII", sub="")))
                                                  # to='UTF-8-MAC', sub='byte'
r_stats_text_corpus <- tm_map(r_stats_text_corpus,
                              content_transformer(tolower)) #Transform all text to lower case
r_stats_text_corpus <- tm_map(r_stats_text_corpus,
                              removePunctuation) #remove all punctuation
r_stats_text_corpus <- tm_map(r_stats_text_corpus,
                              function(x)removeWords(x,stopwords())) #remove all stop words
r_stats_text_corpus <- tm_map(r_stats_text_corpus, removeWords, stopwords("english"))
r_stats_text_corpus <- tm_map(r_stats_text_corpus, removeNumbers)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, stripWhitespace)

tdm <- TermDocumentMatrix(r_stats_text_corpus,
                          control = list(wordLengths = c(1, Inf)))
tdm
(freq.terms <- findFreqTerms(tdm, lowfreq = 20))
m <- as.matrix(tdm)

# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
mdf <- data.frame(names(word.freq),word.freq)
colnames(mdf) <- c("word","freq")

#Create color word cloud
#wordcloud(r_stats_text_corpus, random.order= F, min.freq = 10, max.words =
#            150, colors=brewer.pal(8, "Dark2"))
wordcloud2(mdf, backgroundColor="black")

#score sentiment calculator
score.sentiment = function(sentences, pos.words,
                           neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  # we got a vector of sentences. plyr will handle a list  or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
    scores = laply(sentences, function(sentence, pos.words,
                                       neg.words) {
     
       # clean up sentences with R's regex-driven global substitute, gsub():
        sentence = gsub('[[:punct:]]', '', sentence)
        sentence = gsub('[[:cntrl:]]', '', sentence)
        sentence = gsub('\\d+', '', sentence)
        #sentence = gsub('\n','',sentence)
        
        # and convert to lower case:
        sentence = tolower(sentence)
        # split into words. str_split is in the stringr        package
        word.list = str_split(sentence, '\\s+')
        # sometimes a list() is one level of hierarchy too        much
        words = unlist(word.list)
        # compare our words to the dictionaries of positive & negative terms
        pos.matches = match(words, pos.words)
        neg.matches = match(words, neg.words)
        # match() returns the position of the matched term or NA
        # we just want a TRUE/FALSE:
        pos.matches = !is.na(pos.matches)
        neg.matches = !is.na(neg.matches)
        # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
          score = sum(pos.matches) - sum(neg.matches)
        return(score)
    }, pos.words, neg.words, .progress=.progress )
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)

}

#NLP Dataset
#The positive and negative words lexicons are stored in alocal director collected
#from a research paper for a dataset of positive and negative words
#Please see appendix/reference for more information on origin
hu.liu.pos = scan('C:/Users/Umang Gupta/Desktop/Big Data Project/Lexical Twitter Sentiment Analytics/positive-words.txt', what =
                    'character', comment.char = ';')
hu.liu.neg = scan('C:/Users/Umang Gupta/Desktop/Big Data Project/Lexical Twitter Sentiment Analytics/negative-words.txt', what =
                    'character', comment.char = ';')
posdictionary=scan('C:/Users/Umang Gupta/Desktop/Big Data Project/Lexical Twitter Sentiment Analytics/positivewordsresearch-phrases-dictionary.txt',what =
                     'character', sep = ',' , comment.char = ';')

#added more than 6000 latest positive phrases, idioms and inspirational quotes!!!

#Here we add some additional words that were discovered from initial review of tweets
pos.words <- c(hu.liu.pos, posdictionary)
pos.words <- c(pos.words, 'Congrats', 'prizes', 'prize', 'thanks', 'thnx', 'Grt', 'gr8',
               'plz', 'trending', 'recovering', 'brainstorm', 'leader')
neg.words <- c(hu.liu.neg, 'wait', 'waiting', 'hold','onhold' , 'on hold' ,
               'cancel','spam', 'spams', 'cancel', 'wth', 'Fight', 'fighting',
               'wtf', 'arrest', 'no', 'not')

#run the sentiment function on the text of the tweets
apple.scores <- score.sentiment(tweets_nodups_text,
                                 pos.words, neg.words, .progress='none')

#Its time to run Distant Supervised Learning Classifier

apple.scores[1] = apple.scores[1] + sentiments[4]
apple.scores[1]

#merge the results back with the original file
tweets.nodups.df$text=tweets_nodups_text
apple.score.merge <- merge(apple.scores, tweets.nodups.df, by='text')

#Histogram of sentiment for all tweets
hist(apple.scores$score,xlab=" ",main="Sentiment of tweets that mention Apple", border="black",col="skyblue")

#scatter plot of tweet date vs sentiment score
plot(tweets.nodups.df$Date, apple.scores$score,
     xlab = "Date", ylab = "Sentiment Score", main = "Sentiment of tweets
     that mention apple BCBS by Date")

#Creating Pie chart with percentages for degree of positive,neagtive,neutral

#Good

Sc = apple.scores$score

#Output of following is FALSE or TRUE
good <- sapply(Sc, function(Sc) Sc <= 3 && Sc >= 1)
#Converts to actual value
Sc[good]
list_good = Sc[good]
value_good = length(list_good)

#Very good

vgood <- sapply(Sc, function(Sc) Sc > 3 && Sc < 6)
#Converts to actual value
Sc[vgood]
list_vgood = Sc[vgood]
value_vgood = length(list_vgood)

#Outstanding

vvgood <- sapply(Sc, function(Sc) Sc >= 6)
#Converts to actual value
Sc[vvgood]
list_vvgood = Sc[vvgood]
value_vvgood = length(list_vvgood)

#Bad : Unsatisfactory

#Output of following is FALSE or TRUE
bad <- sapply(Sc, function(Sc) Sc >= -3 && Sc <= -1)
#Converts to actual value
Sc[bad]
list_bad = Sc[bad]
value_bad = length(list_bad)

#Very bad : Poor

#Output of following is FALSE or TRUE
vbad <- sapply(Sc, function(Sc) Sc < -3 && Sc > -6)
#Converts to actual value
Sc[vbad]
list_vbad = Sc[vbad]
value_vbad = length(list_vbad)

#Awful

vvbad <- sapply(Sc, function(Sc) Sc <= -6)
#Converts to actual value
Sc[vvbad]
list_vvbad = Sc[vvbad]
value_vvbad = length(list_vvbad)

#Neutral
neutral <- sapply(Sc, function(Sc) Sc > -1 && Sc < 1) 
list_neutral = Sc[neutral]
value_neutral = length(list_neutral)

slices1 <- c(value_good, value_vvbad, value_bad, value_vgood, value_vbad, value_neutral, value_vvgood )
lbls1 <- c("Good", "Awful", "Unsatisfactory", "Great", "Poor", "Neutral", "Outstanding")
pct <- round(slices1/sum(slices1)*100) #Percentage
lbls1 <- paste(lbls1, pct) # add percents to labels 
lbls1 <- paste(lbls1,"%",sep="") # ad % to labels 
pie(slices1,labels = lbls1, col=rainbow(length(lbls1)),
    main="Number of tweets with particular sentiment")

#total evaluation: positive / negative / neutral
stat <- apple.scores$score
stat <- mutate(apple.scores,
               tweet=ifelse(apple.scores$score > 0, 'positive',
                            ifelse(apple.scores$score < 0, 'negative', 'neutral')))
by.tweet <- group_by(stat, tweet, tweets.nodups.df$Date)
by.tweet <- dplyr::summarise(by.tweet, number=n())

#Sentiment (positive, negative and neutral) over time
ggplot(by.tweet, aes(by.tweet$`tweets.nodups.df$Date`, by.tweet$number))  + xlab("Date") + ylab("No. Of Tweets") + ggtitle("Tweet Sentiments per Date")+
  geom_line(aes(group=tweet, color=tweet), size=2) +
  geom_point(aes(group=tweet, color=tweet), size=4) +
  theme(text = element_text(size=18), axis.text.x =
          element_text(angle=90, vjust=1))

#Read stock price CSV in
stock_prices <- read.csv("HistoricalQuotes.csv")
#Format date so R knows this is a date field

stock_prices$Date <- as.Date(strptime(stock_prices$date, format= "%d-%m-%y"))
#Left join the sentiment analysis with the stock prices
tweet_stock <- left_join(apple.score.merge,
                         stock_prices, by='Date')

#weekday tweets
weekday_tweet_stock <- tweet_stock
weekday_tweet_stock <- subset(tweet_stock,
                              !is.na(Daily.Change))
#Create indicator fields to flag tweets as positive,negative or neutral based on sentiment score
weekday_tweet_stock$pos <-
  as.numeric(weekday_tweet_stock$score > 0)
weekday_tweet_stock$neg <-
  as.numeric(weekday_tweet_stock$score < 0)
weekday_tweet_stock$neu <-
  as.numeric(weekday_tweet_stock$score == 0)
#Transform file from one row per tweet to one row per day summarizing the total positive, negative and netural tweets per day
tweet_stock_df <- ddply(weekday_tweet_stock, c('Date',
                                               'high', 'low', 'Daily.Change'), plyr::summarise, pos.count = sum(pos),
                        neg.count = sum(neg), neu.count = sum(neu))
tweet_stock_df$all.count <- tweet_stock_df$pos.count +
  tweet_stock_df$neg.count + tweet_stock_df$neu.count
#calculate the percent of tweets that were negative on each day
tweet_stock_df$percent.neg <-
  round((tweet_stock_df$neg.count / tweet_stock_df$all.count) * 100)
#Simple correlation
cor(tweet_stock_df$percent.neg,
    tweet_stock_df$Daily.Change, use = "complete")
glm_model <- glm(tweet_stock_df$Daily.Change ~
                   tweet_stock_df$percent.neg)
summary(glm_model)
#plot of % negative tweets vs daily change in stock price with linear regression line overlaid

# fit a loess line
xyplot(tweet_stock_df$Daily.Change ~ tweet_stock_df$percent.neg,
       grid = TRUE,
       type = c("p", "r"), col.line = "darkorange", lwd = 3, ylab = "Daily Change in Stock Price",
       xlab = "Percent of Negative Tweets", main = "% Negative Tweets vs Daily
       Stock Price Change for AAPL")

#calculate the percent of tweets that were positive on each day
tweet_stock_df$percent.pos <-
  round((tweet_stock_df$pos.count / tweet_stock_df$all.count) * 100)
#Simple correlation
cor(tweet_stock_df$percent.pos,
    tweet_stock_df$Daily.Change, use = "complete")
glm_model <- glm(tweet_stock_df$Daily.Change ~
                   tweet_stock_df$percent.pos)
summary(glm_model)
#plot of % positive tweets vs daily change in stock price with linear regression line overlaid

xyplot(tweet_stock_df$Daily.Change ~ tweet_stock_df$percent.pos,
       grid = TRUE,
       type = c("p", "r"), col.line = "darkorange", lwd = 3,ylab = "Daily Change in Stock Price",
       xlab = "Percent of Positive Tweets", main = "% Positive Tweets vs Daily
       Stock Price Change for AAPL")
