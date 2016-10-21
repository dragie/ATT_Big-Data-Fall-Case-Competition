library(qdap)
library(tm)

data<- tweetsATTCaresDallasDF$text
mycorpus<-Corpus(VectorSource(data))

inspect(mycorpus[1:4])

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

str(score)
final<-cbind(data, score[,2])

write.csv(final,"final.csv")

