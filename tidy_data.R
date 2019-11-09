rm(list=ls())

setwd("Master/Pirate programming/DAta")

library(gtrendsR)

library(tidyverse)

########## importing the stock data

FB <- read_csv("FB.csv")

AAPL <- read_csv("AAPL.csv")

DJI <- read_csv("DJI.csv")

GOOGL <- read_csv("GOOGL.csv")

GSPC <- read_csv("GSPC.csv")

MSFT <- read_csv("MSFT.csv")

NHY <- read_csv("NHY.csv")

SALM <- read_csv("SALM.csv")

TEL <- read_csv("TEL.csv")

TSLA <- read_csv("TSLA.csv")

AMZN <- read_csv("AMZN.csv")

####### creating a new vector in each 
AAPL <- AAPL%>%
  mutate(Normalized = `Adj Close`/max(`Adj Close`, na.rm = FALSE)*100)

DJI <- DJI%>%
  mutate(hits = NA)%>%
  mutate(Normalized = `Adj Close`/max(`Adj Close`, na.rm = FALSE)*100)

FB <- FB%>%
  mutate(Normalized = `Adj Close`/max(`Adj Close`, na.rm = FALSE)*100)

GOOGL <- GOOGL%>%
  mutate(Normalized = `Adj Close`/max(`Adj Close`, na.rm = FALSE)*100)

GSPC <- GSPC%>%
  mutate(hits = NA)%>%
  mutate(Normalized = `Adj Close`/max(`Adj Close`, na.rm = FALSE)*100)

MSFT <- MSFT%>%
  mutate(Normalized = `Adj Close`/max(`Adj Close`, na.rm = FALSE)*100)

NHY <- NHY%>%
  mutate(Normalized = `Adj Close`/max(`Adj Close`, na.rm = FALSE)*100)

SALM <- SALM%>%
  mutate(Normalized = `Adj Close`/max(`Adj Close`, na.rm = FALSE)*100)

TEL <- TEL%>%
  mutate(Normalized = `Adj Close`/max(`Adj Close`, na.rm = FALSE)*100)

TSLA <- TSLA%>%
  mutate(Normalized = `Adj Close`/max(`Adj Close`, na.rm = FALSE)*100)

AMZN <- AMZN%>%
  mutate(Normalized = `Adj Close`/max(`Adj Close`, na.rm = FALSE)*100)

#getting weekly data for companies

library(tidyverse)
library(gtrendsR) # package that allows for extraction of google search data

#salm,nhy,msft,fb,btc-usd,aapl,tsla,telenor

## NOTE due to low(er) avargage volume of searches for norwgian companies and bitcoin, we will use all kinds of google searches for the norwegian companies (more spessiffic: gprop = "web") 

salm_gt <- gtrends(keyword = 'salmar', time = "2013-12-29 2018-12-31", onlyInterest = T) 

nhy_gt <- gtrends(keyword = 'norsk hydro', time = "2013-12-29 2018-12-31", onlyInterest = T) 

tel_gt <- gtrends(keyword = 'telenor', time = "2013-12-29 2018-12-31", onlyInterest = T)



## Note for the American companies we will use only searches for news about the companies(i call them american even though they have HQ in other places, for totally not tax-related reasons)

msft_gt <- gtrends(keyword = 'microsoft', time = "2013-12-29 2018-12-31", gprop = "news", onlyInterest = T)

fb_gt <- gtrends(keyword = 'facebook', time = "2013-12-29 2018-12-31", gprop = "news", onlyInterest = T)

aapl_gt <- gtrends(keyword = 'apple inc', time = "2013-12-29 2018-12-31", gprop = "news", onlyInterest = T)

tsla_gt <- gtrends(keyword = 'tesla', time = "2013-12-29 2018-12-31", gprop = "news", onlyInterest = T)

amzn_gt <- gtrends(keyword = 'amazon', time = "2013-12-29 2018-12-31", gprop = "news", onlyInterest = T)

googl_gt <- gtrends(keyword = 'google', time = "2013-12-29 2018-12-31", gprop = "news", onlyInterest = T)

#now to gather all the data into a nice dataset:




## Set google hits and stock info toghether. and add ticker names as a variable
AAPL <- AAPL %>%
  mutate(hits = aapl_gt$interest_over_time$hits)%>%
  mutate(ticker = "AAPL")
DJI <- DJI %>%
  mutate(ticker = "DJI")%>%
  mutate(hits = NA)
FB <- FB %>%
  mutate(ticker = "FB")%>%
  mutate(hits = fb_gt$interest_over_time$hits)

GOOGL <- GOOGL %>%
  mutate(ticker = "GOOGL")%>%
  mutate(hits = googl_gt$interest_over_time$hits)

GSPC <- GSPC %>%
  mutate(ticker = "GSPC")%>%
  mutate(hits = NA)
 

MSFT <- MSFT %>%
  mutate(ticker = "MSFT")%>%
  mutate(hits = msft_gt$interest_over_time$hits)

NHY <- NHY %>%
  mutate(ticker = "NHY")%>%
  mutate(hits = nhy_gt$interest_over_time$hits)

SALM <- SALM %>%
  mutate(ticker = "SALM")%>%
  mutate(hits = salm_gt$interest_over_time$hits)

TEL <- TEL %>%
  mutate(ticker = "TEL")%>%
  mutate(hits = tel_gt$interest_over_time$hits)

TSLA <- TSLA %>%
  mutate(ticker = "TSLA")%>%
  mutate(hits = tsla_gt$interest_over_time$hits)

AMZN <- AMZN %>%
  mutate(ticker = "AMZN")%>%
  mutate(hits = amzn_gt$interest_over_time$hits)


#combine all the different datasets into a nice Tidy tibble

combinedd <- rbind(AMZN,TSLA,DJI,FB,GOOGL,GSPC,MSFT,NHY,SALM,TEL,AAPL)



