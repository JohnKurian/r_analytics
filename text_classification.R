require(textcat)
require(textfeatures)
library(syuzhet)
library(ggplot2)
library(tm)
library(tidyverse)
library(tokenizers)
require(tm)
library(wordcloud)


text <- readLines("data_3/text.txt")

#Tokenize the text
text.tokens <- unique(get_tokens(text, pattern = "\\W")) 

#Find out the language
textcat(text)
table(textcat(text))

# find out the letter count of each line
nchar(text)

#Which line has the maximum letters
which.max(nchar(text))


#Find out the sentiment of each sentence
s <- data.frame(Word=text.token, sentiment=get_sentiment(text.token,
                                                         method="syuzhet"))

## delete words with score=0
sw0 <- s[!s$sentiment==0,]


# Plotting the sentiment

## plot
op <- par(bg="wheat")
plot(sw0$sentiment, axes=F, ylab="Sentiment Score",
     xlab="", type="b", col="red", lwd=2, ylim=c(-1,1))
abline(h=0, col="darkblue")
abline(v=1:nrow(sw0), col="darkblue", lty=2)
axis(2)
text(1:nrow(sw0), rep(-1, nrow(sw0)), lab=sw0$Word, srt=35, xpd=TRUE,# pos=1,
     adj=c(1.2,1.3))








mg <- VCorpus(DirSource("data_3/text", encoding = "UTF-8"),
              readerControl = list(language = "en") )
mg


mg.clean <- tm_map(mg, content_transformer(tolower))
mg.clean <- tm_map(mg.clean, stripWhitespace)
mg.clean <- tm_map(mg.clean,removePunctuation)
mg.clean <- tm_map(mg.clean,removeNumbers)
mg.clean <- tm_map(mg.clean, removeWords, stopwords("english"))
mg.clean <- tm_map(mg.clean, stemDocument)

dtm <- DocumentTermMatrix(mg.clean)

inspect(dtm)

findFreqTerms(dtm, 20)

freq <- sort(colSums(as.matrix(dtm)),decreasing = T)

wordcloud(names(freq),freq,scale = c(5,1), max.words = 50,
          random.order = F, colors = brewer.pal(8, "Dark2"),
          rot.per = 0.35,use.r.layout = F)
