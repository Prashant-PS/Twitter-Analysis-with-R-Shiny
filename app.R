### Initialize All Libarary, Install if not available

 ##### ****** Please note : Please use your token at line number 130   ********

ValidateLibrary <-function(x)
{x <- as.character(x)
if (!require(x,character.only=TRUE)) 
{  install.packages(pkgs=x,repos="http://cran.r-project.org")
  require(x,character.only=TRUE)
}
}

LoadLibrary<-function()
{
 # ValidateLibrary("rJava")
  ValidateLibrary("dplyr")
  ValidateLibrary("DT")
  ValidateLibrary("shiny")
  ValidateLibrary("shinydashboard")
  ValidateLibrary("ggplot2")
  ValidateLibrary("leaflet")
  ValidateLibrary("RColorBrewer")
  ValidateLibrary("tm")
  ValidateLibrary("highcharter")
  ValidateLibrary("SnowballC")
  ValidateLibrary("tidytext")
  ValidateLibrary("wordcloud2")
  ValidateLibrary("RWeka")
  ValidateLibrary("fmsb")
  ValidateLibrary("kernlab")
  ValidateLibrary("rtweet")
  ValidateLibrary("randomForest")
  ValidateLibrary("slam")
  ValidateLibrary("caret")
  ValidateLibrary("rpart.plot")
  ValidateLibrary("rpart")
  ValidateLibrary("rattle")
  ValidateLibrary("quanteda")
  ValidateLibrary("textstem")
  ValidateLibrary("reshape")
  ValidateLibrary("twitteR")
  #ValidateLibrary("plyr")
  ValidateLibrary("stringr")
  ValidateLibrary("ROAuth")
  ValidateLibrary("tibble")
  ValidateLibrary("purrr")
  ValidateLibrary("lazyeval")
  ValidateLibrary("shinyalert")
}

LoadLibrary()

header <- dashboardHeader( title = "Twitter", disable =FALSE)
positive_words = scan ('positive-words.txt', what = 'character', comment.char =';')
negative_words = scan ('negative-words.txt', what = 'character', comment.char =';')
#reviews = read.csv("combined.csv",encoding="UTF-8")

##########UI Code Block #############

shinyui <- shinyUI(pageWithSidebar(
  
  headerPanel(strong("TWITTER SENTIMENT ANALYSIS FRAMEWORK FOR AUTOMOTIVE & MOBILE BRANDS")),
  
  sidebarPanel(
    
    radioButtons("selectSource","", 
                 c("From-File",
                   "From-Twitter" ), selected = "From-File", inline = T),
    # Take Input from File	  
    fileInput("file1", "Choose CSV File",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv") ),
    
    
    tags$hr(),
    #checkboxInput("header", "Header", TRUE),
    # Take Input from Twitter
    textInput("searchTerm", "Enter data to be searched"  , "#"),
    #sliderInput("maxTweets","Number of recent tweets to use for analysis:",min=500,max=5000,value=500), 
    submitButton(text="Analyse")
    
  ),
  
  mainPanel(
    
    
    tabsetPanel(
      
      tabPanel('Data ', pre(DT::dataTableOutput('contents'))),
      tabPanel("Clean Data",DT::dataTableOutput('cleanData')),
      
      
      tabPanel("WordCloud", box(width=12, height=550, solidHeader = F,  
                                radioButtons("selectWCgraph","The Word Cloud", 
                                             c("UNI-GRAM", "BI-GRAM" ), selected = "UNI-GRAM", inline = T), 
                                wordcloud2Output("word2",height = "420px", width = "800px"))), 
      
      
      
      tabPanel("Words Frequency Analysis",HTML
               ("<div><h3> Words Frequency Analysis Histograms Graph </h3></div>"), plotOutput("hist100"), plotOutput("hist200"), plotOutput("hist300"), plotOutput("hist400")),
      
      tabPanel("Random Forest Model",HTML("<div><h3>Model</h3></div>"), verbatimTextOutput("Model1")), 
      tabPanel("Histogram",HTML
               ("<div><h3> Histograms graphically depict the positivity or negativity of peoples' opinion about of the hashtag
                 </h3></div>"), plotOutput("histPos"), plotOutput("histNeg"), plotOutput("histScore")
      ),
      tabPanel("Emotions Radar", box(width=NULL, height=550, solidHeader = F, title = strong("Emotions Radar"),
                                     highchartOutput("emotion_polar_plot",height=500) )),
      tabPanel("Sentiment Analysis", box(width=NULL, height=350, solidHeader = F, title = strong("Emotions Radar"),
                                         highchartOutput("sentiment_plot",height=500) )),
      
      
      tabPanel("Table",HTML( "<div><h3> Storing the Tweets associated with the Hashtag in Tabular Format </h3></div>"), tableOutput("tabledata"),
               HTML ("<div><h4> This table showing the score for each type of sentiment. </h4></div>"))
      
      
    )#end of tabset panel
  )#end of main panel
  
))#end of shinyUI

server <- shinyServer(function(input, output, session) {
  
  tweets_downloader <- function(tag, n, lang='en', retryonratelimit = TRUE){
    
    twitter_token <- create_token(
      app = 'GLIM - Extracting Tweets',
      consumer_key <- "iY3I8OW5R6FIOjO0isjqC" ,   #Please use your Token
      consumer_secret <- "nWCxxYnFidBHnNcMhqYPsjbuYcHGu8lqE6" ,    #Please use your Token
      access_token <- "768729800408053Q0gBCgaU4bT0p7pKUHI6" ,    #Please use your Token
      access_secret <- "obkaOL80YccjiN9b4Y1peWD68qpf" ,    #Please use your Token
      set_renv = F
    )
    progress <- shiny::Progress$new()
    progress$set(message = "Downloading Tweets", value = 0.2)
    on.exit(progress$close())
    tweet.df <- search_tweets(tag, n = n, include_rts = FALSE, lang = lang, token = twitter_token, retryonratelimit = retryonratelimit)
    progress$set(message = "Downloading Tweets", value = 0.8)
    print(paste0("Total Tweets downloaded for - ",tag,": ",length(tweet.df$text)))
    print(paste0("Total Unique Texts downloaded for - ",tag,": ",length(unique(tweet.df$text))))
    
    tweet.df$hashtags <- as.character(tweet.df$hashtags)
    tweet.df$symbols <- as.character(tweet.df$symbols)
    tweet.df$urls_url <- as.character(tweet.df$urls_url)
    tweet.df$urls_t.co <- as.character(tweet.df$urls_t.co)
    tweet.df$urls_expanded_url <- as.character(tweet.df$urls_expanded_url)
    tweet.df$media_url <- as.character(tweet.df$media_url)
    tweet.df$media_t.co <- as.character(tweet.df$media_t.co)
    tweet.df$media_expanded_url <- as.character(tweet.df$media_expanded_url)
    tweet.df$media_type <- as.character(tweet.df$media_type)
    tweet.df$ext_media_url <- as.character(tweet.df$ext_media_url)
    tweet.df$ext_media_t.co <- as.character(tweet.df$ext_media_t.co)
    tweet.df$ext_media_expanded_url <- as.character(tweet.df$ext_media_expanded_url)
    tweet.df$mentions_user_id <- as.character(tweet.df$mentions_user_id)
    tweet.df$mentions_screen_name <- as.character(tweet.df$mentions_screen_name)
    tweet.df$geo_coords <- as.character(tweet.df$geo_coords)
    tweet.df$coords_coords <- as.character(tweet.df$coords_coords)
    tweet.df$bbox_coords <- as.character(tweet.df$bbox_coords)
    
    tweet.df
  }
  
  TweetFrame<-function(twtList)
  {
    
    df<- twtList
    df$text <- sapply(df$text,function(x) iconv(x, "latin1", "ASCII", sub=""))  
   ### df$text <- sapply(df$text,function(x) iconv(x, "latin1", "UTF-8", sub=""))  
    return (df$text)
  }
  ReadFromFile <- function()
  {
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    twtList<-read.csv(inFile$datapath)# ,encoding="UTF-8//IGNORE")
    tweets<-twtList$text
    tweets<-TweetFrame(twtList)
    return( tweets)
  }
  
  GetFinalTweets <- function()
  {
    if (input$selectSource == "From-File") {
      tweets <- ReadFromFile()
    }  else if (input$selectSource == "From-Twitter") {
      
      
      twtList <- tweets_downloader(tag=input$searchTerm, n=500, lang='en', 
                                   retryonratelimit = TRUE) 
      tweets<-TweetFrame(twtList) }
    
    
    return(tweets)	
  }
  ## Output Data to First TAB
  tweets <- reactive({tweets <- GetFinalTweets()})
  output$contents <-DT::renderDataTable({
    
    DT::datatable(data.frame(tweets()), options = list(pageLength = 10))
  })
  
  GetSentimentWords <-function()
  {
    positive_words <<-c(positive_words )
    negative_words <<-c(negative_words)
  }
  GetSentimentWords()
  
  ProcessCleaner <- function(text){
    progress <- shiny::Progress$new()
    progress$set(message = "Cleaning Tweets", value = 0.2)
    on.exit(progress$close())
    screentext <<- text
    
    corpus <- VCorpus(VectorSource(text))
    myCorpus <- tm_map(corpus, content_transformer(function(x) iconv(enc2utf8(x), sub = "")))
    # #clean text
    
    myCorpus <- tm_map(myCorpus, content_transformer(tolower))
    
    # myCorpus <- tm_map(myCorpus,Textprocessing)
    
    hashtagRemover <- function(x)  gsub('#\\S+', '', x)
    myCorpus <- tm_map(myCorpus, content_transformer(hashtagRemover))
    progress$set(message = "Cleaning Tweets", value = 0.3)
    removeURL =  function(x)  gsub('http.*\\s*', '', x)
    myCorpus = tm_map(myCorpus, content_transformer(removeURL))
    progress$set(message = "Cleaning Tweets", value = 0.4)
    removeUsername = function(x) gsub("@[^[:space:]]*", "", x)  
    myCorpus = tm_map(myCorpus, content_transformer(removeUsername))
    
    progress$set(message = "Cleaning Tweets", value = 0.6)
    remove_specialChar = function(x)  gsub("<[^>]+>", "",x)
    myCorpus <- tm_map(myCorpus, content_transformer(remove_specialChar))
    #remove_specialChar3 = function(x)  gsub("\\'\x'[0-9]", "",x)
    #myCorpus <- tm_map(myCorpus, content_transformer(remove_specialChar3))
    remove_specialChar2 = function(x)  gsub('[[:cntrl:]]', '', x)
    myCorpus <- tm_map(myCorpus, content_transformer(remove_specialChar2))
    
    #stem words
    # myCorpus <- tm_map(myCorpus,stemDocument)
    
    ## myCorpus <- VCorpus(VectorSource(lemmatize_strings(myCorpus)))
    progress$set(message = "Cleaning Tweets - Number , Punctuation", value = 0.7)
    removeNumPunct = function(x) gsub('[[:punct:]]', '', x)
    myCorpus<- tm_map(myCorpus, content_transformer(stripWhitespace))
    myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
    myCorpus <- tm_map(myCorpus, content_transformer(removeNumbers))
    
   # removeSingle <- function(x) gsub("", " ", x)   
  #  myCorpus <- tm_map(myCorpus, content_transformer(removeSingle))
    
    #stopwords
    progress$set(message = "Cleaning Tweets - Stopwords", value = 0.9)
    myStopWords = c((stopwords('english')),c("rt","brt","uber","corola","mahindraxuv","model","europe","tesla","blue","ok","know","uf","im","honda","civic","toyota","corolla","bmw","series","audi","cars","car","vehicle","mahindra",
                                             "btcc","r","b","fk","list","datetimestamp","meta","content","wday","min","sec","hour","description","author","year","language","isdst","mon","yday","mday","nme","origin","okay","head","hour",
                                             "ii","character","camry","xuv","prius","class","mercedes","volkswagen","uaaufef","golf","w","fo","gt","amp","th"))
    myCorpus<- tm_map(myCorpus,removeWords , myStopWords)
    shortWordRemover <- function(x) gsub('\\b\\w{1,2}\\b',' ',x)
    myCorpus <- tm_map(myCorpus, content_transformer(shortWordRemover))
    
    return(myCorpus)
    progress$set(message = "Cleaning Tweets", value = 1)
    
  }
  # end of ProccessCleaner Function
  
  WordFreq <- function(myCorpus){
    progress <- shiny::Progress$new()
    progress$set(message = "Cleaning Tweets", value = 0.2)
    on.exit(progress$close())
    myCorpus <- VCorpus(VectorSource(myCorpus))
    tdm<- TermDocumentMatrix(myCorpus, control= list(wordLengths= c(1, Inf)))
    #tdm
    #frequency
    (freq.terms <- findFreqTerms(tdm, lowfreq = 100))
    term.freq <- rowSums(as.matrix(tdm))
    term.freq <- subset(term.freq, term.freq > 100)
    df1 <- data.frame(term = names(term.freq), freq= term.freq)
    p1 <- ggplot(df1, aes(reorder(term, freq), y=freq, fill=freq))+
      geom_bar(stat="identity")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      ylab("More then 100 Times Word Appears in Tweets")+
      xlab("")+
      guides(fill=FALSE)
    
    output$hist100 <- renderPlot({p1})
    
    (freq.terms <- findFreqTerms(tdm, lowfreq = 200))
    term.freq <- rowSums(as.matrix(tdm))
    term.freq <- subset(term.freq, term.freq > 200)
    df2 <- data.frame(term = names(term.freq), freq= term.freq)
    p2 <- ggplot(df2, aes(reorder(term, freq), y=freq, fill=freq))+
      geom_bar(stat="identity")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      ylab("More then 200 Times Word Appears in Tweets")+
      xlab("")+
      guides(fill=FALSE)
    output$hist200 <- renderPlot({p2})
    progress$set(message = "Preparing Graph", value = 0.8)
    
    (freq.terms <- findFreqTerms(tdm, lowfreq = 300))
    term.freq <- rowSums(as.matrix(tdm))
    term.freq <- subset(term.freq, term.freq > 300)
    df3 <- data.frame(term = names(term.freq), freq= term.freq)
    p3 <- ggplot(df3, aes(reorder(term, freq), y=freq, fill=freq))+
      geom_bar(stat="identity")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      ylab("More then 300 Times Word Appears in Tweets")+
      xlab("")+
      guides(fill=FALSE)
    
    output$hist300 <- renderPlot({p3})
    (freq.terms <- findFreqTerms(tdm, lowfreq = 400))
    term.freq <- rowSums(as.matrix(tdm))
    term.freq <- subset(term.freq, term.freq > 400)
    df4 <- data.frame(term = names(term.freq), freq= term.freq)
    progress$set(message = "Preparing Wordcloud", value = 0.9)
    p4 <- ggplot(df4, aes(reorder(term, freq), y=freq, fill=freq))+
      geom_bar(stat="identity")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      ylab("More then 400 Times Word Appears in Tweets")+
      xlab("")+
      guides(fill=FALSE)
    output$hist400 <- renderPlot({p4})
    #word cloud
    word.freq <-sort(rowSums(as.matrix(tdm)), decreasing= T)
    pal<- brewer.pal(8, "Dark2")
    
    return ( word.freq )
  }
  
  myCorpus <- reactive({ProcessCleaner(tweets())})
  cleanData  <- reactive({data.frame(text=unlist(sapply(myCorpus(), `[`, "content")), 
                                     stringsAsFactors=F) })
  
  ##Output for Clean Data TAB	
  output$cleanData  <- DT::renderDataTable({DT::datatable(cleanData(), options = list(pageLength = 10))})
  ##Output for Word frequency and WordCloud
  text_word<-reactive({text_word<-WordFreq( cleanData() )})
  #output$word <- renderPlot({ wordcloud(words = names(text_word()),freq = text_word(),min.freq = 2,random.order=F,max.words=input$max, col=rainbow(200), main="WordCloud", scale=c(4,0.5)) })
  
  
  output$word2<- renderWordcloud2({
    if(input$selectWCgraph == "UNI-GRAM"){
      set.seed(1234)
      worddata= data.frame(word = names(text_word()),freq=text_word())
      worddata1 <- (worddata %>% filter(freq>1) %>% arrange(desc(freq)))[1:200,]
      wordcloud2(data = worddata1, size=0.8, minSize = 0.0, fontWeight = 'bold',
                 ellipticity = 0.65)
    } else if(input$selectWCgraph == "BI-GRAM") {
      progress <- shiny::Progress$new()
      progress$set(message = "Bi-gram", value = 0)
      on.exit(progress$close())
      BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
      tdm.bigram = TermDocumentMatrix(myCorpus(),control = list(tokenize = BigramTokenizer))
      freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
      worddata = data.frame(word=names(freq), freq=freq)
      
      worddata1 <- (worddata %>% filter(freq>2) %>% arrange(desc(freq)))[1:150,]
      wordcloud2(data = worddata1, size=0.8, minSize = 0.0, fontWeight = 'bold', 
                 ellipticity = 0.65)  
    }}) ###Wordcloud 
  
  ##### Sentiment Scoring
  
  
  score.sentiment <- function(sentences, positive_words, negative_words, .progress='none')
  {
    list=lapply(sentences, function(sentence, positive_words, negative_words)
    {
      sentence = gsub('[[:punct:]]',' ',sentence)
      sentence = gsub('[[:cntrl:]]','',sentence)
      sentence = gsub('\\d+','',sentence)
      sentence = gsub('\n','',sentence)
      
      sentence = tolower(sentence)
      word.list = str_split(sentence, '\\s+')
      words = unlist(word.list)
      pos.matches = match(words, positive_words)
      neg.matches = match(words, negative_words)
      pos.matches = !is.na(pos.matches)
      neg.matches = !is.na(neg.matches)
      pp=sum(pos.matches)
      nn = sum(neg.matches)
      score = sum(pos.matches) - sum(neg.matches)
      list1=c(score, pp, nn)
      return (list1)
    }, positive_words, negative_words)
    score_new=lapply(list, `[[`, 1)
    pp1=score=lapply(list, `[[`, 2)
    nn1=score=lapply(list, `[[`, 3)
    
    scores.df = data.frame(score=score_new, text=sentences)
    positive.df = data.frame(Positive=pp1, text=sentences)
    negative.df = data.frame(Negative=nn1, text=sentences)
    
    list_df <-list(scores.df, positive.df, negative.df)
    
    return(list_df)
  }
  sentimentAnalyser<-function(result)
  {
    #Creating a copy of result data frame
    test1 <- result[[1]]
    test2 <- result[[2]]
    test3 <- result[[3]]
    
    #Creating three different data frames for Score, Positive and Negative
    #Removing text column from data frame
    test1$text=NULL
    test2$text=NULL
    test3$text=NULL
    #Storing the first row(Containing the sentiment scores) in variable q
    q1=test1[1,]
    q2=test2[1,]
    q3=test3[1,]
    qq1=melt(q1, var='Score')
    qq2=melt(q2, var='Positive')
    qq3=melt(q3, var='Negative') 
    qq1['Score'] = NULL
    qq2['Positive'] = NULL
    qq3['Negative'] = NULL
    #Creating data frame
    table1 <- data.frame(Text=result[[1]]$text, Score=qq1)
    table2 <- data.frame(Text=result[[2]]$text, Score=qq2)
    table3 <- data.frame(Text=result[[3]]$text, Score=qq3)
    
    #Merging three data frames into one
    table_final=data.frame(Text=table1$Text, Positive=table2$value, Negative=table3$value, Score=table1$value)
    #Storing Analysis in CSV file (Optional)
    #write.csv(table_final,file="SentiAnalysis.csv",append = TRUE) #To store Analysis in CSV, uncomment this line
    
    return(table_final)
  }
  
  percentage<-function(table_final)
  {
    #Positive Percentage
    
    #Renaming
    posSc=table_final$Positive
    negSc=table_final$Negative
    
    #Adding column
    table_final$PosPercent = posSc/ (posSc+negSc)
    
    #Replacing Nan with zero
    pp = table_final$PosPercent
    pp[is.nan(pp)] <- 0
    table_final$PosPercent = pp*100
    
    #Negative Percentage
    
    #Adding column
    table_final$NegPercent = negSc/ (posSc+negSc)
    
    #Replacing Nan with zero
    nn = table_final$NegPercent
    nn[is.nan(nn)] <- 0
    table_final$NegPercent = nn*100
    write.csv(table_final,file="SentiAnalysis.csv",append = FALSE)
    
    return(table_final)
    
  }
  
  
  #HISTOGRAM  
  
  result<-reactive({result<-score.sentiment(tweets(), positive_words, negative_words, .progress='none')})
  
  table_final<-reactive({table_final<-sentimentAnalyser(result())})
  output$histPos<- renderPlot({ hist(table_final()$Positive, col=rainbow(10), main="Histogram of Positive Sentiment", xlab = "Positive Score") })
  output$histNeg<- renderPlot({ hist(table_final()$Negative, col=rainbow(10), main="Histogram of Negative Sentiment", xlab = "Negative Score") })
  output$histScore<- renderPlot({ hist(table_final()$Score, col=rainbow(10), main="Histogram of Score Sentiment", xlab = "Overall Score") })	
  table_final_percentage<-reactive({table_final_percentage<-percentage(  table_final() )})
  
  output$tabledata<-renderTable(table_final_percentage())	
  
  emotion_score <- reactive({
    
    progress <- shiny::Progress$new()
    progress$set(message = "Getting Emotions", value = 0.2)
    on.exit(progress$close())
    
    # x <- data.frame(tweet_nbr = 1:length(cleaned_tweets()), clean_tweet = cleaned_tweets())
    x <-  cleanData ()
    x$clean_tweet <- as.character(x$text)
    
    df <- x  %>% unnest_tokens(output = word, input = clean_tweet, token = "words")
    df <- df %>% inner_join(get_sentiments("nrc"))
    progress$set(value = 0.8, detail = paste("COllating.."))
    df <- df %>% group_by(sentiment) %>% summarise(score = n())
    names(df) <- c("emotion", "score")
    df[c(1:5,8:10),]
    #df
  })
  output$emotion_polar_plot <- renderHighchart(
    hc <- highchart() %>%
      hc_chart(polar = T) %>% 
      
      hc_xAxis(categories = emotion_score()$emotion, 
               labels = list(style = list(fontSize= '14px')), title =NULL, tickmarkPlacement = "on", lineWidth = 0) %>% 
      hc_plotOptions(series = list(marker = list(enabled = F))) %>% 
      hc_yAxis(gridLineInterpolation = "polygon", lineWidth = 0, min = 0) %>% 
      hc_add_series(name = "Emotions Score", emotion_score()$score, type ="area", color = "#4472c4", pointPlacement = "on")
  )
  
  sentiment_score <- reactive({
    progress <- shiny::Progress$new()
    progress$set(message = "Working on Sentiment", value = 0)
    on.exit(progress$close())
    
    #x <- data.frame(tweet_nbr = 1:length(cleanData()), clean_tweet =  cleanData ())
    x <- data.frame(tweet_nbr = 1:length(unlist(sapply(myCorpus(), `[`, "content"))), clean_tweet=unlist(sapply(myCorpus(), `[`, "content")), 
                    stringsAsFactors=F)
    x$clean_tweet <- as.character(x$clean_tweet)
    
    progress$set(detail = "Getting score...", value = 0.6)
    df <- x  %>% unnest_tokens(output = word, input = clean_tweet, token = "words")
    df <- df %>% inner_join(get_sentiments("afinn"))
    df <- df %>% group_by(tweet_nbr) %>% summarize(score = sum(score))
    
    progress$set(detail = "Getting score...", value = 0.8)
    df$category_senti <- ifelse(df$score < 0, "Negative", ifelse(df$score > 0, "Positive", "Neutral"))
    df1 <- df %>% left_join(x)
    
    x <- list()
    x[[1]] <- as.data.frame(df1)
    x[[2]] <- as.character(df1[df1$score == max(df1$score),"clean_tweet"][1,1])
    x[[3]] <- as.character(df1[df1$score == min(df1$score),"clean_tweet"][1,1])
    
    x
  })
  
  senti_df <- reactive({
    sentiment_score()[[1]] %>% group_by(category_senti) %>% summarise(score = n()) %>% 
      mutate(score_pct = score/sum(score)*100, coloract = c("#d35400", "#2980b9", "#2ecc71"))
  })
  
  output$sentiment_plot <- renderHighchart(
    
    hc <- highchart() %>%
      hc_chart(type = "bar") %>%
      #hc_plotOptions(bar = list(getExtremesFromAll = T)) %>% 
      hc_tooltip(crosshairs = TRUE, shared = FALSE,useHTML=TRUE,
                 formatter = JS(paste0("function() {
                                       console.log(this.point.y);
                                       var result='';
                                       result='<br/><span style=\\'color:'+this.series.color+'\\'>'+this.series.name+'</span>:<b> '+Math.round(this.point.y.toFixed(0))/1 + '%' + '</b>';
                                       return result;
}")))%>%
      hc_xAxis(categories = senti_df()$category_senti,
               #labels = list(rotation = 0, step=1), title =list(text="Brand")
               labels = list(style = list(fontSize= '12px')) #max=20, scrollbar = list(enabled = T)
      )    %>%
      hc_colors(colors = senti_df()$coloract) %>% 
      hc_add_series(name="Sentiment", data = senti_df()$score_pct, colorByPoint = TRUE, 
                    type ="column",
                    #max=max(d()$freq), tickInterval = max(d()$freq)/4, alignTicks = F,
                    color = "#4472c4", showInLegend= F) %>% 
      hc_yAxis(labels=list(format = '{value}%'),min=0,
               max=100,showFirstLabel = TRUE,showLastLabel=TRUE)
    #hc_legend(layout = "vertical", align = "right", verticalAlign = "top", width=120, itemStyle = list(fontSize= '10px'))
  )
  
  
  rf_model <- function(text){
    reviews_rf = reviews
    id_train = sample(nrow(reviews),nrow(reviews)*0.80)
    reviews.train = reviews[id_train,]
    reviews.test = reviews[-id_train,]
    reviews_train_polarity = reviews.train[,2]
    reviews_test_polarity = reviews.test[,2]
    
    reviews.train <-na.omit(reviews.train)
    reviews.test <-na.omit(reviews.test)
    
    reviews.train <-reviews.train[,-2]
    reviews.test <-reviews.test[,-2]
    
    # Training with Random forest model
    comb.rf = randomForest( reviews_train_polarity~. , data=reviews.train)
    return(comb.rf)
  }
  
  #  verbatimTextOutput
  ref_model<-reactive({rf_testresult<- rf_model( reviews)})
  output$Model1 <- renderPrint({ str(ref_model())})
  
}) #end of Server function


app = shinyApp(
  #shinyApp(
  ui = shinyui,
  server <- server
) 

#runApp(app, host = '0.0.0.0', port = 3168)