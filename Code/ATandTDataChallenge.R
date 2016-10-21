install.packages("twitteR")
install.packages("RCurl")
install.packages("zipcode")
install.packages("qdap", dependencies = TRUE)
install.packages("rJava", dependencies = TRUE)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_101') # for 64-bit version
library(rJava)
library(twitteR)
library(RCurl)
library(zipcode)
library(ggmap)
library(data.table)
library(Rfacebook)
library(qdap)
library(tm)

###########TWITTER CRAWLER#######################
consumerKey <- 'fYy3MQTnItwuhK4jelH0RbPVo'
consumerSecret <- 'LCe15rMBdBgXulTX1erxt6nfgdnvlbTrgHLYwZeXhbGVNgFxAr'
accessToken <-	'780605227741216770-XnDRaW2wO5UJASQRPhRZI0D79QurUPh'
accessTokenSecret <-	'H6CX1B0mTQfQbeJsUHjIGWxYI4sFT7bA4U8OfCXLPrjpX'

setup_twitter_oauth(consumer_key = consumerKey, consumer_secret = consumerSecret, access_token = accessToken, access_secret = accessTokenSecret)
data(zipcode)
zipcodes <- data.table(zipcode)
dallasZips <- zipcodes[city == 'Dallas' & state == 'TX']


#############################################GET LAST nDay TWEETS
getNdaysTweets <- function(nDays = 7, presentGeoCode = '32.99813,-96.79088,40mi', searchString = 'ATTCares')
{
  presentDate <- Sys.Date()
  NonZeroCounter <- 0
  while(presentDate > Sys.Date() - nDays)
  {
    
    sinceDate <- as.character(presentDate,'%Y-%m-%d')
    untilDate <- as.character(presentDate + 1,'%Y-%m-%d')
    print(paste0(sinceDate,' TO ',untilDate))
    tweetList <- searchTwitter(searchString,lang = 'en',geocode = presentGeoCode,since = sinceDate,until = untilDate)
    listLength <- length(tweetList)
    if(listLength > 0)
    {
      NonZeroCounter <- NonZeroCounter +1
      if(NonZeroCounter == 1)
      {
        ATTCaresTweets <- twListToDF(tweetList)
      }
      else
      {
        ATTCaresTweetsPart <- twListToDF(tweetList)
        ATTCaresTweets <- rbind(ATTCaresTweets,ATTCaresTweetsPart)
      }
    }
    
    presentDate <- presentDate -1
  }
  return(ATTCaresTweets)
}

#ATTCareTweets <- getNdaysTweets(nDays = 12)
#ATTCaresTweets <- searchTwitter('ATTCares',lang = 'en',geocode = presentGeoCode,since = sinceDate, until = untilDate)
#presentGeoCode <- '32.99813,-96.79088,40mi'
###############RATE TWEETS USING POLARITY ALGORITHM
RateTweets <- function(data)
{
  mycorpus<-Corpus(VectorSource(data))
  
  #inspect(mycorpus[1:4])
  
  #Sentiment Analysis
  #Text Segmentation
  # Create patterns to elimina special code and other patterns
  # URLs
  urlPat<-function(x) gsub("(ftp|http)(s?)://.*\\b", "", x)
  mycorpus<-tm_map(mycorpus, urlPat)
  
  # Emails
  emlPat<-function(x) gsub("\\b[A-Z a-z 0-9._ - ]*[@](.*?)[.]{1,3} \\b", "", x)
  mycorpus<- tm_map(mycorpus, emlPat)
  
  # Twitter tags
  tt<-function(x) gsub("RT |via", "", x)
  mycorpus<- tm_map(mycorpus, tt)
  
  # Twitter Usernames
  tun<-function(x) gsub("[@][a - zA - Z0 - 9_]{1,15}", "", x)
  mycorpus<- tm_map(mycorpus, tun)
  
  #White Space
  mycorpus<- tm_map(mycorpus, stripWhitespace)
  
  mycorpus<-tm_map(mycorpus,tolower)
  mycorpus<-tm_map(mycorpus,removePunctuation)
  mycorpus<-tm_map(mycorpus,removeNumbers)
  mycorpus<-tm_map(mycorpus,removeWords,stopwords(kind="English"))
  stopwords(kind="English")
  mycorpus<-tm_map(mycorpus,stripWhitespace)
  mycorpus<-tm_map(mycorpus,PlainTextDocument)
  
  
  #convert corpus to data frame
  mydf<-data.frame(text=unlist(sapply(mycorpus,'[',"content")),stringAsFactors=FALSE)
  mydf$text<-as.character(mydf$text)
  
  
  a <- unlist(apply(mydf,1,function(x) polarity(x[1])[[2]]$ave.polarity))
  attributes(a) <- NULL
  a  <- as.vector(a)
  score <- data.frame("Sentence_Num" = 1:nrow(mydf),"Sentiment Score" = a)
  
  #str(score)
  #final<-cbind(data, score[,2])
  #final <- cbind(subset(score, select = 2),data)
  return(score)
}


###############################################
radius <- '10mi'
numDallasZips <- nrow(dallasZips)
#numDallasZips <- 5
zipNum <- 1
nonZeroCounter <- 0
result <- data.frame(matrix(ncol = 5, nrow = 1))
colnames(result) <- c('zipcode','TotalTweets','PositiveTweets','NegativeTweets','NeutralTweets')
while(zipNum <= numDallasZips)
{
  
  #print(dallasZips[zipNum,latitude])
  presentZipCode <- dallasZips[zipNum,zip]
  presentGeoCode <- paste0(dallasZips[zipNum,latitude],',',dallasZips[zipNum,longitude],',',radius)
  print(paste0(presentZipCode,' - ',presentGeoCode))
  ATTCareTweets <- getNdaysTweets(nDays = 12,presentGeoCode = presentGeoCode,searchString = 'ATTCares')
  nTweets <- nrow(ATTCareTweets)
  if(nTweets > 0 )
  {
    nonZeroCounter <- nonZeroCounter +1
    RatedTweets <- RateTweets(ATTCareTweets$text)
    nNegTweets <- nrow(subset(RatedTweets,Sentiment.Score <0))
    nPosTweets <- nrow(subset(RatedTweets,Sentiment.Score >0))
    nNeutralTweets <- nrow(subset(RatedTweets,Sentiment.Score == 0))
    
    resultMatrix  <- matrix(nrow = 1,ncol = 5)
    resultMatrix[1,1] <- presentZipCode
    resultMatrix[1,2] <- nTweets
    resultMatrix[1,3] <- ifelse(is.null(nPosTweets), 0, nPosTweets)
    resultMatrix[1,4] <- ifelse(is.null(nNegTweets), 0, nNegTweets)
    resultMatrix[1,5] <- ifelse(is.null(nNeutralTweets), 0, nNeutralTweets)
    resultPart <- data.frame(resultMatrix)
    colnames(resultPart) <- c('zipcode','TotalTweets','PositiveTweets','NegativeTweets','NeutralTweets')
    result <- rbind(result, resultPart)
    #result <- subset(result, !is.na(zipcode))
    
  }
  zipNum <- zipNum +1
  
  
}
write.csv(result,'result.csv')

##################################################

