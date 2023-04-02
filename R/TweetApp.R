# ==================== Twitter Talks, Shiny Sparks! ================================= #
# Contact - Tams                                                                    #
# ============ Install and load the packages =======================

library(shiny)
library(shinydashboard)
library(highcharter)
library(dplyr)
library(twitteR)
library(rtweet)
library(NLP)
library(tm) # text mining
library(stringr)
library(SnowballC) # text stemming
library(RColorBrewer) # Color Palettes
library(wordcloud)
library(wordcloud2)
library(topicmodels)
library(tidytext)
library(slam)
library(tidyr)
library(igraph)
library(ggraph)
library(widyr)
library(textdata)
#install.packages("tidytext")
library("shinymanager")
#setwd("C://Users//HP/Desktop//Presidential_Election")
#rm(list=ls())
list.files()
imgName = "twitter.png"

if(!dir.exists("www")){
  dir.create("www")
}

addResourcePath(prefix = "test", directoryPath = "www")

png(file = paste0("www/", imgName), bg = "lightgreen")
par(mar = c(0,0,0,0))
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 0.5, y = 0.5, imgName, cex = 10, col = "black")
dev.off()

#=== UI Code Starts ===
header <- dashboardHeader(title= img(src = 'twitter.png',align = "left",
                                     title = "Presidential Election", height = "10px",align = "right"))
sidebar <- dashboardSidebar(
  sidebarMenu(id = 'menu',
              menuItem(strong("Token Frequency"),tabName = 'token', icon = icon("th-list", lib = "glyphicon")),
              menuItem(strong("Sentiment"),tabName = 'sentiment', icon = icon("thumbs-up", lib = "glyphicon")),
              #menuItem(strong("Network Analysis"),tabName = 'network', icon = icon("chart-line")),
              menuItem(strong("Popular Tweets"),tabName = 'popular', icon = icon("twitter"))
              #menuItem(strong("Complains"),tabName = 'popular', icon = icon("twitter")) # Complains MenuItem
  ),
  hr(),
  htmlOutput("topic_selector")
)

body <- dashboardBody(
  
  # Title 
  tags$head(tags$style(HTML(
    '.skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
    font-weight: bold;font-size: 16px;
    }
    '))),
  
  tags$head(tags$style(HTML(
    
    '
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color:##ADD8E6;
                                }
                                
                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color:#45818e;
                                }
                                
                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color:##ADD8E6;
                                }
                                
                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color:#000000;
                                }
                                
                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #b1b4b5;
                                }
                                
                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #000000;
                                color: #ffffff;
                                }
                                
                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #b1b4b5;
                                }
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #b1b4b5;
                                }

                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #eeeeee;
                                }
    
    '))),
  
  tags$head(tags$style(HTML(
    '.myClass { 
    font-size: 20px;
    line-height: 50px;
    text-align: left;
    font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
    padding: 0 15px;
    overflow: hidden;
    color: white;
    }
    '))),
  #tags$head(tags$style(HTML("div.main-header {text-align: center;}"))),
  #tags$script(HTML('
  #                $(document).ready(function() {
  #               $("header").find("nav").append(\'<span class="myClass" style="white-space:pre">                                                        Aura Listening </span>\');
  #              })
  #             ')),
  tags$head(tags$style(HTML('.modal-sm {width: 40px;}'))),
  
  # Set the boxes of all charts
  tags$head(tags$style(HTML(".col-sm-2,.col-sm-12,.col-sm-4,.col-sm-12,.col-sm-6,.col-sm-7,.col-sm-5 {
                            position: relative;
                            min-height: 1px;
                            padding-right: 5px;
                            padding-left: 5px;"))),
  
  tags$head(tags$style(HTML(".container-fluid {padding-left: 5px; padding-right: 5px;}"))),
  tags$head(tags$style(HTML(".form-group {margin-bottom: -15px;}"))),
  
  tags$head(tags$style(HTML(".box {margin-bottom: 10px;}"))),
  
  
  tags$head(tags$style(HTML("#col_word_cloud,#col_freq {padding-left:0px;padding-right:0px;} "))),
  #tags$head(tags$style(HTML("#col_emotion,#col_sentiment {padding-left:0px;padding-right:0px;} "))),
  tags$head(tags$style(HTML(".box-header {text-align: center;} "))),
  tags$head(tags$style(HTML("#network_panel {width:100%;} "))),
  
  tabItems(
    tabItem("token",
            fluidPage(
              fluidRow(
                column(7, id = "col_word_cloud",
                       box(width=12, height=550, solidHeader = F, title = strong("The Word Cloud"),
                           radioButtons("word_cloud_gram",NULL, c("Uni-gram","Bi-gram"), selected = "Uni-gram", inline = T),
                           #plotOutput("word_cloud_plot",height = "300px")
                           wordcloud2Output("word_cloud_plot",height = "470px"))
                ),
                column(5, id = "col_freq",
                       box(width=12, height=550, solidHeader = F, title = strong("Here are the frequent words.."),
                           highchartOutput("word_freq_plot", height=500)
                       )
                       
                )
              )
            )
            
    ),
    
    tabItem("sentiment",
            fluidPage(
              fluidRow(
                column(width = 6, id = "col_emotion",
                       box(width=NULL, height=550, solidHeader = F, title = strong("Emotions Radar"),
                           highchartOutput("emotion_polar_plot",height=500)
                       )
                ),
                column(width = 6, 
                       box(width=NULL, height=270, solidHeader = F, title = strong("Sentiment Polarity"),
                           highchartOutput("sentiment_plot",height = 210)
                       ),
                       box(width=NULL, height=270, solidHeader = F, title = strong("The extreme ones.."),
                           htmlOutput("pos_tweet"),
                           htmlOutput("neg_tweet")
                       )
                )
              )
            )
            
    ),
    
    #tabItem("network",
    #       fluidPage(
    #        fluidRow(
    #         box(width=12, height=500, solidHeader = F,
    #            tabsetPanel(type = 'pills',
    #                       id = 'network_panel',
    #                      tabPanel("Bi Directional Network", 
    #sliderInput("bi_freq", "Min Frequency of Bi-grams:",min = 10, max = 200,value = 100, width = '20%'),
    #                              plotOutput("network_plot1")),
    #                    tabPanel("Correlation Network", plotOutput("network_plot2"))
    #       )
    #  )
    #)
    #)
    
    #),
    
    tabItem("popular",
            fluidPage(
              fluidRow(
                box(width=12, height=370, title = strong("Let us check some popular tweets !!"),
                    radioButtons("fav_rt_button",NULL, c("Most Favorited","Most Retweeted"), selected = "Most Favorited", inline = T),hr(),
                    htmlOutput("fav_rt_tweets")),
                box(width=12, height=300, title = strong("Story - Screenplay - Direction ..."),
                    tags$ul(
                      tags$li(strong(tags$a("Pesidential Election 2023"))),
                      #strong(tags$a("Medium Article Link", href = "https://www.linkedin.com/in/muralimohanakrishnadandu/", target="_blank"))),
                    ),
                    #hr(),
                    #h5(strong("Twitter Data:"),style = 'color:rgb(0, 112, 192)'),strong("Real-time Tweets downloaded with following hashtags"),
                    # tags$ul(
                    # tags$li("Instances of @aurabytranscorp Twitter Handle"),
                    # tags$li("Instances of TranscorpHotels"),
                    # tags$li("Instances of Tony Elumelu Foundation"),
                    #tags$li("Instances of Trans National Corporation of Nigeria"),
                    #tags$li("Instances of Heirs Holdings"),
                    #tags$li("Instances of Tony Elumelu"))
                )
              )
            )
    )
  )
)

ui <- dashboardPage(title = 'Twitter Analytics App', header , sidebar, body)


server <- function(input, output, session){
  
  # Custom Functions for cleaning the twitter data
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
    
    return(docs)
  }
  
  #== Select the topic ===
  output$topic_selector <- renderUI({
    selectInput("topic_selector",label = "Select Twitter Topic:",
                choices = c("Bola Tinubu","Peter Obi", "Atiku Abubakar","INEC","Buhari"),
                selected = "Bola Tinubu", width = "100%")
  })
  
  #=== Read Data ===
  #tweet_df_PHED <- readRDS(file = "tweet_df_PHED.rds")
  tweet_df_TINUBU <- readRDS(file = "tweet_df_TINUBU.rds")
  tweet_df_OBI <- readRDS(file = "tweet_df_OBI.rds")
  tweet_df_ATIKU <- readRDS(file = "tweet_df_ATIKU.rds")
  tweet_df_INEC <- readRDS(file = "tweet_df_INEC.rds")
  tweet_df_BUHARI <- readRDS(file = "tweet_df_BUHARI.rds")
  
  #=== Concatenate Data with Selector ===
  tweet_df_final <- rbind(
    #cbind(tweet_df_PHED, topic = "Aura"),
    cbind(tweet_df_TINUBU, topic = "Bola Tinubu"),
    cbind(tweet_df_OBI, topic = "Peter Obi"),
    cbind(tweet_df_ATIKU, topic = "Atiku Abubakar"),
    cbind(tweet_df_INEC, topic = "INEC"),
    cbind(tweet_df_BUHARI, topic = "Buhari")
  )
  
  #=== Get cleaned tweets of selected topic ===
  cleaned_tweets <- reactive({
    progress <- shiny::Progress$new()
    progress$set(message = "Cleaning Tweets", value = 0)
    on.exit(progress$close())
    
    x <- tweet_df_final %>% filter(topic == input$topic_selector)
    progress$set(detail = "Ah! lot to clean...", value = 0.5)
    tweets_cleaner(x)
    
  })
  
  #=== Get docs for the selected topic
  docs <- reactive({custom_stopwords <- if(input$topic_selector == "Bola Tinubu"){
    c("#@officialABAT", "#Bola Tinubu", "#@OfficialAPCNg")
  }
  else if(input$topic_selector == "Peter Obi"){
    c("#@PeterObi","#PETER OBI","#@NgLabour OR #POssible")
  }
  else if(input$topic_selector == "Atiku Abubakar"){
    c("#@atiku","#Atiku","#@OfficialPDPNig OR #PDP")
  }
  else if(input$topic_selector == "INEC"){
    c("#@inecnigeria","#INEC")
  }else if(input$topic_selector == "Buhari"){
    c("#@MBuhari","#Buhari")
  }else{
    NULL
  }
  
  progress <- shiny::Progress$new()
  progress$set(message = "Preparing Docs", value = 0)
  on.exit(progress$close())
  
  progress$set(detail = "Docking...", value = 0.5)
  tweets_cleaner_tm(cleaned_tweets(), custom_stopwords = custom_stopwords)
  
  })
  
  
  tdm <- reactive({
    
    progress <- shiny::Progress$new()
    progress$set(message = "Creating TDM", value = 0)
    on.exit(progress$close())
    
    progress$set(detail = "Almost there...", value = 0.5)
    TermDocumentMatrix(docs())
    
  })
  
  d <- reactive({
    v <- sort(row_sums(tdm()),decreasing=TRUE)
    data.frame(word = names(v),freq=v)
  })
  
  
  #=== Word Freq plot ===
  output$word_freq_plot <- renderHighchart(
    
    d()[1:20,] %>% 
      
      hchart('bar', hcaes(x = 'word',y = 'freq')) %>%
      hc_exporting(enabled = TRUE) %>%
      hc_add_theme(hc_theme_bloom())
    #hchart(type = "bar", hcaes(x = word, y = freq,color = word))
    # hc <- highchart() %>%
    #   #hc_title(text = "Incremental Revenue and Total Cost by Offer Group") %>%
    #   hc_chart(type = "bar") %>%
    #   #hc_plotOptions(bar = list(getExtremesFromAll = T)) %>% 
    #   hc_tooltip(crosshairs = TRUE, shared = FALSE,useHTML=TRUE,
    #              formatter = JS(paste0("function() {
    #                                    //console.log(this);
    #                                    //console.log(this.point.y);
    #                                    var result='';
    #                                    result='<br/><span style=\\'color:'+this.series.color+'\\'>'+this.x.name+'</span>:<b> '
    #                                    +Math.round(this.point.y.toFixed(0)/100)/10 + 'K' + '</b>';
    #                  /                  return result;
    #   }"))) %>%
    #   hc_xAxis(categories = d()[1:100,]$word,
    #            #labels = list(rotation = 0, step=1), title =list(text="Brand")
    #            labels = list(style = list(fontSize= '11px')), max=20, scrollbar = list(enabled = T)
    #   )    %>%
    #   hc_add_series(name="Word", data = d()[1:100,]$freq, type ="column",
    #                 #max=max(d()$freq), tickInterval = max(d()$freq)/4, alignTicks = F,
    #                 color = "#4472c4", showInLegend= F)
    #hc_legend(layout = "vertical", align = "right", verticalAlign = "top", width=120, itemStyle = list(fontSize= '10px'))
  )
  
  #=== Wordcloud plot ===
  output$word_cloud_plot <- renderWordcloud2({
    
    if(input$word_cloud_gram == "Uni-gram"){
      
      set.seed(1234)
      # wordcloud(words = d()$word, freq = d()$freq, scale = c(3,0.5), min.freq = 3,
      #           max.words=100, random.order=FALSE, rot.per=0.35, 
      #           colors=brewer.pal(8, "Dark2"))
      d1 <- (d() %>% filter(freq>1) %>% arrange(desc(freq)))[1:10,]
      wordcloud2(data = d1, size=0.7, minSize = 0.0, fontWeight = 'bold', 
                 ellipticity = 0.65)
      
    } else if(input$word_cloud_gram == "Bi-gram"){
      
      progress <- shiny::Progress$new()
      progress$set(message = "Bi-gram", value = 0)
      on.exit(progress$close())
      
      progress$set(value = 0.3, detail = "Creating Bitokens...")
      bitoken <- data.frame(text = sapply(docs(), as.character), stringsAsFactors = FALSE) %>% 
        unnest_tokens(bigram, text, token = "ngrams", n = 2)
      two_word <- bitoken %>% count(bigram, sort = TRUE)
      progress$set(value = 0.9, detail = paste("Parsing a dataframe..Almost done"))
      sort_two <- two_word[order(two_word$n,decreasing=TRUE),]
      # wordcloud(sort_two$bitoken,sort_two$Freq,random.order=FALSE,scale = c(3,0.5),
      #           min.freq = 2,colors = brewer.pal(8,"Dark2"),max.words=50)
      names(sort_two) <- c("word", "freq")
      d1 <- (sort_two %>% filter(freq>2) %>% arrange(desc(freq)))[1:10,]
      wordcloud2(data = d1, size=0.7, minSize = 0.0, fontWeight = 'bold', 
                 ellipticity = 0.65)
    }
    
  })
  
  #===== Sentiment Analysis ======
  # emotion_df_ds <- readRDS(file = "data/emotion_df_ds.rds")
  # emotion_df_iphone <- readRDS(file = "data/emotion_df_iphone.rds")
  # emotion_df_capmarvel <- readRDS(file = "data/emotion_df_capmarvel.rds")
  # emotion_df_sec377 <- readRDS(file = "data/emotion_df_sec377.rds")
  # emotion_df_robo2 <- readRDS(file = "data/emotion_df_robo2.rds")
  # emotion_df_reliance <- readRDS(file = "data/emotion_df_reliance.rds")
  # 
  # emotion_df_final <- rbind(
  #   cbind(emotion_df_ds, topic = "Data Science"),
  #   cbind(emotion_df_iphone, topic = "IPhoneXS"),
  #   cbind(emotion_df_capmarvel, topic = "Captain Marvel Trailer"),
  #   cbind(emotion_df_sec377, topic = "Section 377"),
  #   cbind(emotion_df_robo2, topic = "Robo 2.0 Teaser"),
  #   cbind(emotion_df_reliance, topic = "Relaince - Dassault Rafale")
  #   
  # )
  
  emotion_score <- reactive({
    # emotion_df <- emotion_df_final %>% filter(topic == input$topic_selector)
    # x <- as.data.frame(colSums(emotion_df[,1:10]))
    # x <- data.frame(names = row.names(x), x)
    # names(x) <- c("emotion", "score")
    # x[1:8,]
    progress <- shiny::Progress$new()
    progress$set(message = "Getting Emotions", value = 0.2)
    on.exit(progress$close())
    
    x <- data.frame(tweet_nbr = 1:length(cleaned_tweets()), clean_tweet = cleaned_tweets())
    x$clean_tweet <- as.character(x$clean_tweet)
    
    df <- x  %>% unnest_tokens(output = word, input = clean_tweet, token = "words")
    df <- df %>% inner_join(get_sentiments("nrc"))
    progress$set(value = 0.8, detail = paste("Collating.."))
    df <- df %>% group_by(sentiment) %>% summarise(score = n())
    names(df) <- c("emotion", "score")
    df[c(1:5,8:10),]
    
  })
  
  
  
  # ..................getting emotions score................
  
  output$emotion_polar_plot <- renderHighchart(
    hc <- highchart() %>%
      #hc_title(text = "Incremental Revenue and Total Cost by Offer Group") %>% 
      hc_chart(polar = T,
               borderColor = "#FF6347",
               borderRadius = 10)
    #borderWidth = 2)
    #backgroundColor = "#e2e6df") %>% 
    # hc_tooltip(crosshairs = T, shared = T,useHTML=T,
    #            formatter = JS(paste0("function() {
    #                                      //console.log(this);
    #                                      //console.log(this.points[0].series.name);
    #                                      var result='';
    #                                      result='<br/><span style=\\'color:'+this.series.color+'\\'>'+this.series.name+'</span>:<b> '+'$'+Math.round(this.point.y.toFixed(0)/100000)/10 + 'M' + '</b>';
    #                                      return result;
    #                                      }"))
    %>%
      hc_xAxis(categories = emotion_score()$emotion, 
               labels = list(style = list(fontSize= '8px', fontweight ='bold')), title =NULL, tickmarkPlacement = "on", lineWidth = 0) %>% 
      hc_plotOptions(series = list(marker = list(enabled = F))) %>% 
      hc_yAxis(gridLineInterpolation = "polygon", lineWidth = 1, min = 0) %>% 
      hc_add_series(name = "Emotions Score", emotion_score()$score, type ="area", color = "#FF0000", pointPlacement = "on")
  )
  
  # --------- getting sentiment score-----------
  
  sentiment_score <- reactive({
    progress <- shiny::Progress$new()
    progress$set(message = "Working on Sentiment", value = 0)
    on.exit(progress$close())
    
    x <- data.frame(tweet_nbr = 1:length(cleaned_tweets()), clean_tweet = cleaned_tweets())
    x$clean_tweet <- as.character(x$clean_tweet)
    
    progress$set(detail = "Getting score...", value = 0.6)
    df <- x  %>% unnest_tokens(output = word, input = clean_tweet, token = "words")
    df <- df %>% inner_join(get_sentiments("afinn"))
    # df <- df %>% group_by(tweet_nbr) %>% summarize(score = n()) 
    df <- df %>% group_by(tweet_nbr) %>% summarize(score = sum(value))
    
    progress$set(detail = "Getting score...", value = 0.8)
    df$category_senti <- ifelse(df$score < 0, "Negative", ifelse(df$score > 0, "Positive", ifelse(df$score == 0, "Neutral", "No Class")))
    df1 <- df %>% left_join(x)
    
    x <- list()
    x[[1]] <- as.data.frame(df1)
    x[[2]] <- as.character(df1[df1$score == max(df1$score),"clean_tweet"][1,1])
    x[[3]] <- as.character(df1[df1$score == min(df1$score),"clean_tweet"][1,1])
    
    x
  })
  
  senti_df <- reactive({
    sentiment_score()[[1]] %>%as.data.frame() %>%  group_by(category_senti) %>% #summarise(score = n()) %>% 
      mutate(score_pct = score/sum(score)*100)#, coloract = c("#d35400", "#2980b9", "#2ecc71"))
  })
  
  output$sentiment_plot <- renderHighchart({
    
    senti_df() %>% 
      hchart('column', hcaes(x = 'category_senti', y = 'score_pct')) %>%
      hc_exporting(enabled = TRUE) %>%
      hc_add_theme(hc_theme_bloom())
    
    # hc <- highchart() %>%
    #   #hc_title(text = "Incremental Revenue and Total Cost by Offer Group") %>%
    #   hc_chart(type = "bar") %>%
    #   #hc_plotOptions(bar = list(getExtremesFromAll = T)) %>% 
    #   hc_tooltip(crosshairs = TRUE, shared = FALSE,useHTML=TRUE,
    #              formatter = JS(paste0("function() {
    #                                    console.log(this.point.y);
    #                                    var result='';
    #                                    result='<br/><span style=\\'color:'+this.series.color+'\\'>'+this.series.name+'</span>:<b> '+Math.round(this.point.y.toFixed(0))/1 + '%' + '</b>';
    #                                    return result;
    #                }")))%>%
    #   hc_xAxis(categories = senti_df()$category_senti,
    #            #labels = list(rotation = 0, step=1), title =list(text="Brand")
    #            labels = list(style = list(fontSize= '12px')) #max=20, scrollbar = list(enabled = T)
    #   )    %>%
    #   hc_colors(colors = senti_df()$coloract) %>% 
    #   hc_add_series(name="Sentiment", data = senti_df()$score_pct, colorByPoint = TRUE, 
    #                 type ="column",
    #                 #max=max(d()$freq), tickInterval = max(d()$freq)/4, alignTicks = F,
    #                 color = "#4472c4", showInLegend= F) %>% 
    #   hc_yAxis(labels=list(format = '{value}%'),min=0,
    #            max=100,showFirstLabel = TRUE,showLastLabel=TRUE)
    #hc_legend(layout = "vertical", align = "right", verticalAlign = "top", width=120, itemStyle = list(fontSize= '10px'))
  })
  
  output$network_plot1 <- renderPlot({
    progress <- shiny::Progress$new()
    progress$set(message = "Bi Directional Graph", value = 0.2)
    on.exit(progress$close())
    progress$set(detail = "Creating Bitokens..", value = 0.6)
    
    bitoken <- data.frame(text = sapply(docs(), as.character), stringsAsFactors = FALSE) %>% 
      unnest_tokens(bigram, text, token = "ngrams", n = 2)
    two_word <- bitoken %>% count(bigram, sort = TRUE)
    
    bigrams_separated <- two_word %>%
      separate(bigram, c("word1", "word2"), sep = " ")
    
    progress$set(detail = "Plotting..", value = 0.8)
    bigram_graph <- head(bigrams_separated %>% arrange(desc(n)),150) %>% graph_from_data_frame()
    
    #bigram_graph
    #library(ggraph)
    set.seed(2018)
    # ggraph(bigram_graph, layout = "fr") +
    #   geom_edge_link() +
    #   geom_node_point() +
    #   geom_node_text(aes(label = name), vjust = 1, hjust = 1)
    
    a <- grid::arrow(type = "closed", length = unit(.08, "inches"))
    ggraph(bigram_graph, layout = "fr") +
      geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                     arrow = a, end_cap = circle(.07, 'inches')) +
      geom_node_point(color = "lightblue", size = 3) +
      geom_node_text(aes(label = name), vjust = 1, hjust = 1, size=5) +
      theme_void()
  })
  
  output$network_plot2 <- renderPlot({
    progress <- shiny::Progress$new()
    progress$set(message = "Correlation Plot", value = 0.2)
    on.exit(progress$close())
    progress$set(message = "Creating Pairwise words", value = 0.4)
    
    doc <- tweets_cleaner_tm(tweets_cleaner(tweet_df_final), custom_stopwords = "bla")
    
    new_df <- data.frame(text=sapply(doc, identity), 
                         stringsAsFactors=F)
    new_df$tweet_nbr <- 1:nrow(new_df)
    tweet_word <- new_df %>%
      unnest_tokens(word, text) %>% 
      anti_join(stop_words)
    
    
    tweet_word_cors <- tweet_word %>% group_by(word) %>% filter(n() >= 50) %>% 
      pairwise_cor(word, tweet_nbr, sort = TRUE, upper = FALSE)
    
    progress$set(detail = "Plotting..", value = 0.8)
    set.seed(1234)
    tweet_word_cors %>%
      filter(correlation > mean(tweet_word_cors$correlation)) %>%
      graph_from_data_frame() %>%
      ggraph(layout = "fr") +
      geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "royalblue") +
      geom_node_point(size = 5) +
      geom_node_text(aes(label = name), repel = TRUE,
                     point.padding = unit(0.2, "lines")) +
      theme_void()
  })
  
  output$pos_tweet <- renderUI({
    HTML(paste("<b>","Most positive tweet: ","</b>"),"<br>","<i style = 'font-weight: bold'>","\"",sentiment_score()[[2]][1],"\"","</i>","<hr>")
  })
  output$neg_tweet <- renderText({
    HTML(paste("<b>","Most negative tweet: ","</b>"),"<br>","<i style = 'font-weight: bold'>","\"",sentiment_score()[[3]][1],"\"","</i>")
  })
  
  
  fav_rt_tweets <- reactive({
    
    tweets_df <- tweet_df_final %>% filter(topic == input$topic_selector)
    x <- list()
    x[[1]] <- head(tweets_df %>% select(text, favorite_count) %>% arrange(desc(favorite_count)))
    x[[2]] <- head(tweets_df %>% select(text, retweet_count) %>% arrange(desc(retweet_count)))
    x
  })
  
  output$fav_rt_tweets <- renderUI({
    if(input$fav_rt_button == "Most Favorited"){
      tags$ul(
        tags$li(tags$i(paste(fav_rt_tweets()[[1]][1,]$text,"-",paste(fav_rt_tweets()[[1]][1,]$favorite_count))), style = "margin-bottom: 5px; color: #1D65A6; font-weight: bold"), 
        tags$li(tags$i(paste(fav_rt_tweets()[[1]][2,]$text,"-",paste(fav_rt_tweets()[[1]][2,]$favorite_count))), style = "margin-bottom: 5px; color: #F1931B; font-weight: bold"), 
        tags$li(tags$i(paste(fav_rt_tweets()[[1]][3,]$text,"-",paste(fav_rt_tweets()[[1]][3,]$favorite_count))), style = "margin-bottom: 5px; color: #1D65A6; font-weight: bold"),
        tags$li(tags$i(paste(fav_rt_tweets()[[1]][4,]$text,"-",paste(fav_rt_tweets()[[1]][4,]$favorite_count))), style = "margin-bottom: 0px; color: #F1931B; font-weight: bold"),
        tags$li(tags$i(paste(fav_rt_tweets()[[1]][5,]$text,"-",paste(fav_rt_tweets()[[1]][5,]$favorite_count))), style = "margin-bottom: 0px; color: #1D65A6; font-weight: bold"),
        tags$li(tags$i(paste(fav_rt_tweets()[[1]][6,]$text,"-",paste(fav_rt_tweets()[[1]][6,]$favorite_count))), style = "margin-bottom: 0px; color: #F1931B; font-weight: bold")
      )
    } else if(input$fav_rt_button == "Most Retweeted"){
      tags$ul(
        tags$li(tags$i(paste(fav_rt_tweets()[[2]][1,]$text,"-",paste(fav_rt_tweets()[[2]][1,]$retweet_count))), style = "margin-bottom: 5px; color: #1D65A6; font-weight: bold"), 
        tags$li(tags$i(paste(fav_rt_tweets()[[2]][2,]$text,"-",paste(fav_rt_tweets()[[2]][2,]$retweet_count))), style = "margin-bottom: 5px; color: #F1931B; font-weight: bold"), 
        tags$li(tags$i(paste(fav_rt_tweets()[[2]][3,]$text,"-",paste(fav_rt_tweets()[[2]][3,]$retweet_count))), style = "margin-bottom: 5px; color: #1D65A6; font-weight: bold"),
        tags$li(tags$i(paste(fav_rt_tweets()[[2]][4,]$text,"-",paste(fav_rt_tweets()[[2]][4,]$retweet_count))), style = "margin-bottom: 0px; color: #F1931B; font-weight: bold"),
        tags$li(tags$i(paste(fav_rt_tweets()[[2]][5,]$text,"-",paste(fav_rt_tweets()[[2]][5,]$retweet_count))), style = "margin-bottom: 0px; color: #1D65A6; font-weight: bold"),
        tags$li(tags$i(paste(fav_rt_tweets()[[2]][6,]$text,"-",paste(fav_rt_tweets()[[2]][6,]$retweet_count))), style = "margin-bottom: 0px; color: #F1931B; font-weight: bold")
        
      )
    }
    
  })
  
}


shinyApp(ui = ui, server = server)

#getwd()

