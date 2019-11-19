setwd("C:/Users/VBM/Downloads/data project")
rm(list = ls())

library(tidytext)
library(tidyverse)
library(readxl)
library(qdap)
library(tm) #all the three text analysis packages are used, as they all have an exlusive function
library(SnowballC)
library(textmineR)

dic <- read_excel("inquirerbasic.xls")
dic$Positiv[dic$Positiv == "Positiv"] <- 1 #create a dummy variable
dic$Positiv[dic$Negativ == "Negativ"] <- 0

words <- na.omit(dic[1:3])
words <- words[,-2] #we only will use these two columns

words$Entry <- gsub("#1","",words$Entry)

words <- words[- grep("#", words$Entry),]

words$Entry <- tolower(words$Entry) #this is the final dictionary, only containing a dummy variable for a specific word being positive

pos <- filter(words, words$Positiv == 1) #list of only the positive words, used to create a document-text matrix with only these 

DJI <- read_csv("DJIA.csv")
DJI <- DJI %>%
  arrange(Date) %>%
  select(`Adj Close`) %>%
  mutate(diff = 100 * (`Adj Close` / lag(`Adj Close`) - 1))
DJI <- DJI[,2]
DJI[1,1] = 0 #normalize the adjusted close and take everything else out of the matrix

news <- read_csv("CombinedNews.csv")
dates <- news[, 1]
news <- news[,-2]
news <- apply(news, 1, paste, collapse=" ") #we want all the news of a given day to be in one cell of the matrix
news <- tolower(news) #the following commands make the news data easier to use
news <- removeNumbers(news)
news <- wordStem(news)
news <- replace_abbreviation(news)
news <- replace_contraction(news)
news <- replace_symbol(news)
news <- removePunctuation(news)

keep <- paste0(words$Entry, collapse = "|") #we create a single cell value that contains all the words we would like to keep (only the positive words in the next row)
keeppos <- paste0(pos$Entry, collapse = "|")
Text <- str_extract_all(string = news,pattern = keep) #this code check all the words in a day if they are included in the pattern
TextPos <- str_extract_all(string = news,pattern = keeppos)

dtm <- CreateDtm(Text) #we create the document-term matrix (documents=days are rows, while words are columns)
dtmpos <- CreateDtm(TextPos)

all <- rowSums(as.matrix(dtm)) #number of words from the dictionary per day (every row is one day, every column is one word)
posonly <- rowSums(as.matrix(dtmpos)) #number of positives only

score <- posonly/all #finally we assign a score to every day (row), stating the ratio between the positivity and all the words from the dictionary

scatter.smooth(x = score, y=DJI$diff, main="SCORE ~ DJI", ylab = "DJI") #visibly no correlation

linearmodel <- lm(DJI$diff ~ score)
summary(linearmodel)
