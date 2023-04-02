#setwd("C://Users//HP//Desktop//Presidential_Election")
#setwd("C:/Users/HP/Desktop/Presidential_Election")
#getwd()
#list.files()
library(twitteR)
library(rtweet)
library(httr)
library(NLP)
library(tm) # text mining
library(stringr)
library(SnowballC) # text stemming
library(RColorBrewer) # Color Palettes
library(wordcloud)
library(RWeka)
library(syuzhet) # Sentiment
library(topicmodels)
library(tidytext)
library(slam)
library(rJava)
library(jsonlite)
library(httr)
library(httpuv)
library(tidyverse)

#getwd()
#rm(list=ls())
##############################################################
tweets_downloader <- function(tag, n, lang='en', retryonratelimit = TRUE){
  
  twitter_token <- create_token(
    app = 'AuraTransCorp - Extracting Tweets',
    consumer_key <- "f22IKpVCBM53CgsIsKAS1K6XD",
    consumer_secret <- "HfVQDoPfACj1ztxQ5p2VhSrpdyjVRvOvMWSANmGwGkLWqvtDfr",
    access_token <- "1388381999715475465-oqaQt6ajWzxMHZQRcZFjgGVoW41OUB",
    access_secret <- "dfinr1AxaGppt5P54DIut3z6Cbkml1QCjQKjgsZ1lsJgC",
    #bearer_Key <- "AAAAAAAAAAAAAAAAAAAAAF8AawEAAAAAntmWUEHFd8hlCEylKbtqi%2FVeXIM%3DzVaLl18EAfUNNwEiyhnRsYgHXEd64ea6N3Ua7RBWanj4DqBDbQ",
    set_renv = F
  )
  #setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret) 
  
  tweet.df <- search_tweets(tag, n = n, include_rts = FALSE, lang = lang, token = twitter_token, retryonratelimit = TRUE)
  print(paste0("Total Tweets downloaded for - ",tag,": ",length(tweet.df$text)))
  print(paste0("Total Unique Texts downloaded for - ",tag,": ",length(unique(tweet.df$text))))
  
  #tweetsStrangerThings = search_tweets(q = "#Canada", 
  #                                    n = 100, 
  #                                   include_rts = FALSE)
  
  # head(tweetsStrangerThings)
  
  tweet.df$hashtags <- list(tweet.df$hashtags)
  tweet.df$symbols <- list(tweet.df$symbols)
  tweet.df$urls_url <- list(tweet.df$urls_url)
  tweet.df$urls_t.co <- list(tweet.df$urls_t.co)
  tweet.df$urls_expanded_url <- list(tweet.df$urls_expanded_url)
  tweet.df$media_url <- list(tweet.df$media_url)
  tweet.df$media_t.co <- list(tweet.df$media_t.co)
  tweet.df$media_expanded_url <- list(tweet.df$media_expanded_url)
  tweet.df$media_type <- list(tweet.df$media_type)
  tweet.df$ext_media_url <- list(tweet.df$ext_media_url)
  tweet.df$ext_media_t.co <- list(tweet.df$ext_media_t.co)
  tweet.df$ext_media_expanded_url <- list(tweet.df$ext_media_expanded_url)
  tweet.df$mentions_user_id <- list(tweet.df$mentions_user_id)
  tweet.df$mentions_screen_name <- list(tweet.df$mentions_screen_name)
  tweet.df$geo_coords <- list(tweet.df$geo_coords)
  tweet.df$coords_coords <- list(tweet.df$coords_coords)
  tweet.df$bbox_coords <- list(tweet.df$bbox_coords)
  
  tweet.df
  
}



tweets_cleaner <- function(tweet.df){
  
  tweets_txt <- unique(tweet.df$text)
  clean_tweet = gsub("&amp", "", tweets_txt) # Remove Amp
  clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet) # Remove Retweet
  clean_tweet = gsub("@\\w+", "", clean_tweet) # Remove @
  clean_tweet = gsub("#", " ", clean_tweet) # Before removing punctuations, add a space before every hashtag
  clean_tweet = gsub("[[:punct:]]", "", clean_tweet) # Remove Punct
  clean_tweet = gsub("[[:digit:]]", "", clean_tweet) # Remove Digit/Numbers
  clean_tweet = gsub("http\\w+", "", clean_tweet) # Remove Links
  clean_tweet = gsub("[ \t]{2,}", " ", clean_tweet) # Remove tabs
  clean_tweet = gsub("^\\s+|\\s+$", " ", clean_tweet) # Remove extra white spaces
  clean_tweet = gsub("^ ", "", clean_tweet)  # remove blank spaces at the beginning
  clean_tweet = gsub(" $", "", clean_tweet) # remove blank spaces at the end
  clean_tweet = gsub("[^[:alnum:][:blank:]?&/\\-]", "", clean_tweet) # Remove Unicode Char
  
  clean_tweet <- str_replace_all(clean_tweet," "," ") #get rid of unnecessary spaces
  clean_tweet <- str_replace_all(clean_tweet, "https://t.co/[a-z,A-Z,0-9]*","") # Get rid of URLs
  clean_tweet <- str_replace_all(clean_tweet, "http://t.co/[a-z,A-Z,0-9]*","")
  clean_tweet <- str_replace(clean_tweet,"RT @[a-z,A-Z]*: ","") # Take out retweet header, there is only one
  clean_tweet <- str_replace_all(clean_tweet,"#[a-z,A-Z]*","") # Get rid of hashtags
  clean_tweet <- str_replace_all(clean_tweet,"@[a-z,A-Z]*","") # Get rid of references to other screennames
  
  clean_tweet
}

tweets_cleaner_tm <- function(clean_tweet, custom_stopwords = c("bla bla")){
  
  docs <- Corpus(VectorSource(clean_tweet))
  #inspect(docs)
  
  docs <- tm_map(docs, content_transformer(tolower)) # Convert the text to lower case
  docs <- tm_map(docs, removeNumbers) # Remove numbers
  docs <- tm_map(docs, removeWords, stopwords("english")) # Remove english common stopwords
  docs <- tm_map(docs, removeWords, custom_stopwords)  # Remove your own stop word
  docs <- tm_map(docs, removePunctuation) # Remove punctuations
  docs <- tm_map(docs, stripWhitespace) # Eliminate extra white spaces
  # docs <- tm_map(docs, stemDocument) # Text stemming
  docs
}

#============ Get The data for all hashtags ==================
#.............................................................
#.............................................................
#=== AurabyTransCorp==========================================

#Sys.sleep(15*60)
#tweet_df_PHED <- tweets_downloader(tag="#@aurabytranscorp OR #TheAuraExperience  OR #AuraExperience.", n=50000, lang='en', 
#                                  retryonratelimit = TRUE)
#saveRDS(tweet_df_PHED, file = "tweet_df_PHED.rds")


#=== BAT ==========================================
Sys.sleep(15*60)
tweet_df_TINUBU <- tweets_downloader(tag="#@officialABAT OR #Bola Tinubu OR #@OfficialAPCNg", n=5000, lang='en', 
                                     retryonratelimit = retryonratelimit)


saveRDS(tweet_df_TINUBU, file = "tweet_df_TINUBU.rds")

#=== POssible================
Sys.sleep(15*60)
tweet_df_OBI <- tweets_downloader(tag="#@PeterObi OR #PETER OBI OR #@NgLabour OR #POssible", n=5000, lang='en', 
                                  retryonratelimit = TRUE)
saveRDS(tweet_df_OBI, file = "tweet_df_OBI.rds")

#=== AA==================
Sys.sleep(15*60)
tweet_df_ATIKU <- tweets_downloader(tag="#@atiku OR #Atiku OR #@OfficialPDPNig OR #PDP", n=5000, lang='en', 
                                    retryonratelimit = TRUE)
saveRDS(tweet_df_ATIKU, file = "tweet_df_ATIKU.rds")

#=== INEC ===============
Sys.sleep(15*60)
tweet_df_INEC <- tweets_downloader(tag="#@inecnigeria OR #INEC", n=5000, lang='en', 
                                   retryonratelimit = TRUE)
saveRDS(tweet_df_INEC, file = "tweet_df_INEC.rds")

#=== BUHARI ===============
Sys.sleep(15*60)
tweet_df_BUHARI <- tweets_downloader(tag="#@MBuhari OR #Buhari", n=5000, lang='en', 
                                     retryonratelimit = TRUE)
saveRDS(tweet_df_BUHARI, file = "tweet_df_BUHARI.rds")

#================= Sentiment Analysis =======================================

#setwd("B:/R Track/ShinyApps/Twit_Dashboard/RDSFILE")
#setwd("C:/Users/HP/Documents/Aura_Sentiment/Projects_")
#tweet_df_PHED <- readRDS(file = "tweet_df_PHED.rds")
tweet_df_TINUBU <- readRDS(file = "tweet_df_TINUBU.rds")
tweet_df_OBI <- readRDS(file = "tweet_df_OBI.rds")
tweet_df_ATIKU <- readRDS(file = "tweet_df_ATIKU.rds")
tweet_df_INEC <- readRDS(file = "tweet_df_INEC.rds")
tweet_df_BUHARI <- readRDS(file = "tweet_df_BUHARI.rds")

#word.df <- as.vector(tweets_cleaner(tweet_df_PHED))
#emotion_df_PHED <- get_nrc_sentiment(word.df)
#saveRDS(emotion_df_PHED, file = "emotion_df_PHED.rds")


#word.df <- as.vector(tweets_cleaner(tweet_df_PHED))
#emotion_df_PHED <- get_nrc_sentiment(word.df)
##saveRDS(emotion_df_PHED, file = "emotions_df_PHED.rds")

word.df <- as.vector(tweets_cleaner(tweet_df_TINUBU))
emotion_df_TINUBU <- get_nrc_sentiment(word.df)
saveRDS(emotion_df_TINUBU, file = "emotion_df_TINUBU.rds")

word.df <- as.vector(tweets_cleaner(tweet_df_OBI))
emotion_df_OBI <- get_nrc_sentiment(word.df)
saveRDS(emotion_df_OBI, file = "emotion_df_OBI.rds")

word.df <- as.vector(tweets_cleaner(tweet_df_ATIKU))
emotion_df_ATIKU <- get_nrc_sentiment(word.df)
saveRDS(emotion_df_ATIKU, file = "emotion_df_ATIKU.rds")

word.df <- as.vector(tweets_cleaner(tweet_df_INEC))
emotion_df_INEC <- get_nrc_sentiment(word.df)
saveRDS(emotion_df_INEC, file = "emotion_df_INEC.rds")

word.df <- as.vector(tweets_cleaner(tweet_df_BUHARI))
emotion_df_BUHARI <- get_nrc_sentiment(word.df)
saveRDS(emotion_df_BUHARI, file = "emotion_df_BUHARI.rds")

#getwd()


