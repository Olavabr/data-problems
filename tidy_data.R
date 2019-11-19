rm(list=ls())

setwd("C:/Users/Olav/Documents/Master/Pirate programming/DAta")

library(gtrendsR)

library(tidyverse)



########## importing the stock data. Found through yahoo finance, form 30.12.2013(DMY), to 31.12.2018 (DMY), 262 weeks (exept for bitcoin)

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

BTC <- read_csv("BTC.csv")
 

#getting weekly data for companies
## NOTE due to low(er) avargage volume of searches for norwgian companies, we will use all kinds of google searches for the norwegian companies (more spessiffic: gprop = "web") 

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

# and bitcoin. Its a shorter trend due to it having a shorter available price trend compared to the others.

btc_gt <- gtrends(keyword = 'bitcoin', time = "2014-09-13 2018-12-31", onlyInterest = T)




#now to gather all the data into a nice dataset, and make some new variables:
### 1) creating a new vector in each that are normalized in the same way as the google trends are normalized
### 2) Set google hits and stock info toghether. and add ticker names as a variable
### 3) making a collumn with the ticker name
AAPL <- AAPL %>%
  mutate(Normalized = `Adj Close`/max(`Adj Close`, na.rm = FALSE)*100)%>%
  mutate(hits = aapl_gt$interest_over_time$hits)%>%
  mutate(ticker = "AAPL")
DJI <- DJI %>%
  mutate(hits = NA)%>%
  mutate(Normalized = `Adj Close`/max(`Adj Close`, na.rm = FALSE)*100)%>%
  mutate(ticker = "DJI")

FB <- FB %>%
  mutate(Normalized = `Adj Close`/max(`Adj Close`, na.rm = FALSE)*100)%>%
  mutate(ticker = "FB")%>%
  mutate(hits = fb_gt$interest_over_time$hits)

GOOGL <- GOOGL %>%
  mutate(Normalized = `Adj Close`/max(`Adj Close`, na.rm = FALSE)*100)%>%
  mutate(ticker = "GOOGL")%>%
  mutate(hits = googl_gt$interest_over_time$hits)

GSPC <- GSPC %>%
  mutate(hits = NA)%>%
  mutate(Normalized = `Adj Close`/max(`Adj Close`, na.rm = FALSE)*100)%>%
  mutate(ticker = "GSPC")



MSFT <- MSFT %>%
  mutate(Normalized = `Adj Close`/max(`Adj Close`, na.rm = FALSE)*100)%>%
  mutate(ticker = "MSFT")%>%
  mutate(hits = msft_gt$interest_over_time$hits)

NHY <- NHY %>%
  mutate(Normalized = `Adj Close`/max(`Adj Close`, na.rm = FALSE)*100)%>%
  mutate(ticker = "NHY")%>%
  mutate(hits = nhy_gt$interest_over_time$hits)

SALM <- SALM %>%
  mutate(Normalized = `Adj Close`/max(`Adj Close`, na.rm = FALSE)*100)%>%
  mutate(ticker = "SALM")%>%
  mutate(hits = salm_gt$interest_over_time$hits)

TEL <- TEL %>%
  mutate(Normalized = `Adj Close`/max(`Adj Close`, na.rm = FALSE)*100)%>%
  mutate(ticker = "TEL")%>%
  mutate(hits = tel_gt$interest_over_time$hits)

TSLA <- TSLA %>%
  mutate(Normalized = `Adj Close`/max(`Adj Close`, na.rm = FALSE)*100)%>%
  mutate(ticker = "TSLA")%>%
  mutate(hits = tsla_gt$interest_over_time$hits)

AMZN <- AMZN %>%
  mutate(Normalized = `Adj Close`/max(`Adj Close`, na.rm = FALSE)*100)%>%
  mutate(ticker = "AMZN")%>%
  mutate(hits = amzn_gt$interest_over_time$hits)

BTC <- BTC %>%
  mutate(Normalized = `Adj Close`/max(`Adj Close`, na.rm = FALSE)*100)%>%
  mutate(ticker = "BTC")%>%
  mutate(hits = btc_gt$interest_over_time$hits)


#combine all the different datasets into a nice Tidy tibble

combinedd <- rbind(AMZN,TSLA,DJI,FB,GOOGL,GSPC,MSFT,NHY,SALM,TEL,AAPL,BTC)


#remove all the unused vaectors and datasets.
lsss <- ls()
lsss <- lsss[lsss != "combinedd"]
rm(list = lsss)


