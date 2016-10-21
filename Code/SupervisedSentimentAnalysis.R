#Reference : https://rpubs.com/sgeletta/95577

dtsrc  <- "http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
if (!file.exists("coursera-swiftkey.zip")){
  download.file(dtsrc, destfile="coursera-swiftkey.zip")
  unzip("coursera-swiftkey.zip")
}
## list of bad/profane words download from github
bwsrc1<-"https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
if (!file.exists("en_bws.txt")){download.file(bwsrc1, destfile="en_bws.txt")}

twtsDat<- tweetsATTCaresDallasDF$text
twtWordCnt<- sum((nchar(twtsDat) - nchar(gsub(' ','',twtsDat))) + 1)
twtLinesCnt<-NROW(twtsDat)

docProperties<-matrix(c(twtWordCnt, twtLinesCnt), nrow=1, ncol=2)
rownames(docProperties)<- c("twitter")
colnames(docProperties)<- c("# of words","# of lines")

barplot(docProperties[,1], main="Number of Words",
        xlab="data", col=c("lightblue","lightgreen", "yellow"),
        log="y", beside=TRUE)

barplot(docProperties[,2], main="Number of Lines",
        xlab="data", col=c("lightblue","lightgreen", "yellow"),
        log="y", beside=TRUE)

twtsSamp<-sample(twtsDat,twtLinesCnt)
combSamp<- c(twtsSamp)
spSamp<- unlist(strsplit(combSamp, split=", "))

# find indices of words with non-ASCII characters
nonAscIDX<- grep("spSamp", iconv(spSamp, "latin1", "ASCII", sub="spSamp"))

# subset original vector of words to exclude words with non-ACCII characters
ascVec<- spSamp[ - nonAscIDX]

# convert vector back to string
ascSamp<- paste(ascVec, collapse = ", ")

clnSamp<- gsub('[[:digit:]]+', '', ascSamp)
clnSamp<- gsub('[[:punct:]]+', '', clnSamp)
clnSamp<- gsub("http[[:alnum:]]*", "", clnSamp)
clnSamp<- gsub("([[:alpha:]])\1+", "", clnSamp)

## Make into corpus
library(tm)
SampCrps<- Corpus(VectorSource(clnSamp))

##Convert characters to lower case
SampCrps<- tm_map(SampCrps, tolower)

##Remove punctuation
SampCrps<- tm_map(SampCrps, removePunctuation)

##Remove numbers
SampCrps<- tm_map(SampCrps, removeNumbers)


# Create patterns to elimina special code and other patterns
# URLs
urlPat<-function(x) gsub("(ftp|http)(s?)://.*\\b", "", x)
SampCrps<-tm_map(SampCrps, urlPat)

# Emails
emlPat<-function(x) gsub("\\b[A-Z a-z 0-9._ - ]*[@](.*?)[.]{1,3} \\b", "", x)
SampCrps<- tm_map(SampCrps, emlPat)

# Twitter tags
tt<-function(x) gsub("RT |via", "", x)
SampCrps<- tm_map(SampCrps, tt)

# Twitter Usernames
tun<-function(x) gsub("[@][a - zA - Z0 - 9_]{1,15}", "", x)
SampCrps<- tm_map(SampCrps, tun)

#White Space
SampCrps<- tm_map(SampCrps, stripWhitespace)


#Remove profane words
#First get the list of bad words
bwdat<-read.table("en_bws.txt", header=FALSE, sep="\n", strip.white=TRUE)
names(bwdat)<-"Bad Words"

SampCrps<- tm_map(SampCrps, removeWords, bwdat[,1])

SampCrps<-tm_map(SampCrps, removeWords, stopwords("english"))

SampCrps <- tm_map(SampCrps, PlainTextDocument)

# Create nGrams
library(stylo)

myCrps<- txt.to.words(SampCrps)

#create data frames of one, two and three ngrams, 

tblUniGrm<-data.frame(table(make.ngrams(myCrps, ngram.size = 1)))
tbldiGrm<-data.frame(table(make.ngrams(myCrps, ngram.size = 2)))
tbltriGrm<-data.frame(table(make.ngrams(myCrps, ngram.size = 3)))

#Create a sorted table "stbl*" by decending frequency count

stblUnigrm<-tblUniGrm[order(tblUniGrm$Freq, decreasing = TRUE),]
stblDigrm<-tbldiGrm[order(tbldiGrm$Freq, decreasing = TRUE),]
stbltrigrm<-tbltriGrm[order(tbltriGrm$Freq, decreasing = TRUE),]


top20unig<-stblUnigrm[1:40,]
colnames(top20unig)<-c("UniGram","Frequency")

top20dig<-stblDigrm[1:20,]
colnames(top20dig)<-c("DiGram","Frequency")

top20trig<-stbltrigrm[1:20,]
colnames(top20trig) <- c("TriGram","Frequency")

library(ggplot2)


ggplot (top20unig, aes(x = reorder(UniGram, - Frequency), y= Frequency )) + 
  geom_bar( stat = "Identity" , fill = "magenta" ) +  
  geom_text( aes (label = Frequency ) , vjust = - 0.20, size = 3 ) +
  xlab( "UniGram List" ) +
  ylab( "Frequency" ) +
  theme ( axis.text.x = element_text ( angle = 45 , hjust = 1 ) )
