#Set the working directory
setwd("C:\\Users\\Keerthi\\Desktop\\Data Set")

#Read the twitter data

obama1<-read.csv("obama1.csv",h=FALSE)
obama2<-read.csv("obama2.csv",h=FALSE)
obama3<-read.csv("obama3.csv",h=FALSE)
obama4<-read.csv("obama4.csv",h=FALSE)

obama<-rbind(obama1,obama2,obama3,obama4)
str(obama)
names(obama)

#Converting to a dataframe ; only second column
tweets<-data.frame(obama$V2)

#Renaming the column
names(tweets)<-"Tweet_Text"


#Data Pre-processing using tm package
library(tm)

#Building a Text Corpus
#Source for the corpus - Vector

tweets.corpus<-Corpus(VectorSource(tweets$Tweet_Text))
summary(tweets.corpus)
inspect(tweets.corpus[1:5]) #Inspecting elements in Corpus

#Cleaning the data
#Removing stop words
#trying to remove the http which refer to the url's
#You can also try and remove frequent words like "Obama","Barack" that occur in the document


inspect(tweets.corpus[1:5])
#Data Transformations
tweets.corpus<-tm_map(tweets.corpus,tolower) #Converting to lower case
tweets.corpus<-tm_map(tweets.corpus,stripWhitespace) #Removing extra white space
tweets.corpus<-tm_map(tweets.corpus,removePunctuation) #Removing punctuations
tweets.corpus<-tm_map(tweets.corpus,removeNumbers) #Removing numbers
my_stopwords<-c(stopwords('english'),'http*') #Can add more words apart from standard list
tweets.corpus<-tm_map(tweets.corpus,removeWords,my_stopwords)


#Building term document matrix
tweets.tdm<-TermDocumentMatrix(tweets.corpus)
tweets.tdm
dim(tweets.tdm) #Dimensions of term document matrix
inspect(tweets.tdm[1:10,1:10]) #Inspecting the term document matrix

#Removing sparse terms
#Words that occur infrequenctly 
#This function call removes those terms which have 
#at least a 97 percentage of sparse (i.e., terms occurring 0 times in a document) elements
tweets.imp<-removeSparseTerms(tweets.tdm,0.97)
tweets.imp
inspect(tweets.imp[1:10,1:10])


#Finding word and frequencies
temp<-inspect(tweets.imp)
wordFreq<-data.frame(apply(temp, 1, sum))
wordFreq<-data.frame(ST = row.names(wordFreq), Freq = wordFreq[, 1])
head(wordFreq)

wordFreq<-wordFreq[order(wordFreq$Freq, decreasing = T), ]
row.names(wordFreq) <- NULL
View(wordFreq)

##Basic Analyses
#Finding the most frequent terms/words
findFreqTerms(tweets.tdm,10) #Occuring minimum of 10 times
findFreqTerms(tweets.tdm,30) #Occuring minimum of 30 times
findFreqTerms(tweets.tdm,50) #Occuring minimum of 50 times
findFreqTerms(tweets.tdm,70) #Occuring minimum of 70 times

#Finding association between terms/words
findAssocs(tweets.tdm,"republicans",0.2)
findAssocs(tweets.tdm,"climate",0.3)
findAssocs(tweets.tdm,"myanmar",0.3)
findAssocs(tweets.tdm,"leader",0.3) #Correlation value of >= 0.5
findAssocs(tweets.tdm,"congress",0.3)


--------------------------------------------------------------
#Building a word cloud
#Visualization using WordCloud
library("wordcloud")
library("RColorBrewer")

#customizing wordcloud
#Need to use text corpus and not term document matrix
#How to choose colors?
?brewer.pal

display.brewer.all() #Gives you a chart
brewer.pal #Helps you identify the groups of pallete colors
display.brewer.pal(8,"Dark2")

display.brewer.pal(8,"Purples")
display.brewer.pal(3,"Oranges")

pal2<-brewer.pal(8,"Dark2")
#plot your word cloud
wordcloud(tweets.corpus,min.freq=10,max.words=100,random.order=T,colors=pal2)

wordcloud(tweets.corpus,min.freq=50,max.words=100, 
          random.order=T, 
          colors=pal2,vfont=c("script","plain"))
