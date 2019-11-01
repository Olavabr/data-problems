rm(list=ls())

library(gtrendsR)

library(tidyverse)

########## importing the stock data

FB <- read_csv("MASTER/Pirate programming/DAta/FB.csv")

AAPL <- read_csv("MASTER/Pirate programming/DAta/AAPL.csv")

DJI <- read_csv("MASTER/Pirate programming/DAta/DJI.csv")

GOOGL <- read_csv("MASTER/Pirate programming/DAta/GOOGL.csv")

GSPC <- read_csv("MASTER/Pirate programming/DAta/GSPC.csv")

MSFT <- read_csv("MASTER/Pirate programming/DAta/MSFT.csv")

NHY <- read_csv("MASTER/Pirate programming/DAta/NHY.csv")

SALM <- read_csv("MASTER/Pirate programming/DAta/SALM.csv")

TEL <- read_csv("MASTER/Pirate programming/DAta/TEL.csv")

TSLA <- read_csv("MASTER/Pirate programming/DAta/TSLA.csv")

AMZN <- read_csv("MASTER/Pirate programming/DAta/AMZN.csv")

####### creating a new vector in each 
AAPL <- AAPL%>%
  mutate(Normalized = `Adj Close`/max(`Adj Close`, na.rm = FALSE)*100)

DJI <- DJI%>%
  mutate(Normalized = `Adj Close`/max(`Adj Close`, na.rm = FALSE)*100)

FB <- FB%>%
  mutate(Normalized = `Adj Close`/max(`Adj Close`, na.rm = FALSE)*100)

GOOGL <- GOOGL%>%
  mutate(Normalized = `Adj Close`/max(`Adj Close`, na.rm = FALSE)*100)

GSPC <- GSPC%>%
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
  mutate(normalized = `Adj Close`/max(`Adj Close`, na.rm = FALSE)*100)

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

#listing the companies, and search results
comps <- c(salm_gt,nhy_gt,tel_gt,msft_gt,fb_gt,aapl_gt,tsla_gt,amzn_gt, googl_gt)
comps_names <- c('salm','nhy','tel','msft','fb','aapl','tsla','amzn', 'googl')

hit_set <- tibble(weeks = 1:length(amzn_gt$interest_over_time$hits),  #initiating a tibble, (could be done in a different way but yolo)
                  ends_of_week_date = amzn_gt$interest_over_time$date)

for (i in 1:length(comps)) {
  zzz = as.vector(comps[i]$interest_over_time$hits)
  hit_set <- cbind(hit_set,zzz)
  colnames(hit_set)[i+2]<-paste(comps_names[i],"hits",sep = "_")
  
}

search_dtset <- function(from,to){
  salm_gt <- gtrends(keyword = 'salmar', time = paste(from,to,sep = " "), onlyInterest = T) 
  
  nhy_gt <- gtrends(keyword = 'norsk hydro', time = paste(from,to,sep = " "), onlyInterest = T) 
  
  tel_gt <- gtrends(keyword = 'telenor', time = paste(from,to,sep = " "), onlyInterest = T)
  
  
  ## Note for the American companies we will use only searches for news about the companies(i call them american even though they have HQ in other places, for totally not tax-related reasons)
  
  msft_gt <- gtrends(keyword = 'microsoft', time = paste(from,to,sep = " "), gprop = "news", onlyInterest = T)
  
  fb_gt <- gtrends(keyword = 'facebook', time = paste(from,to,sep = " "), gprop = "news", onlyInterest = T)
  
  aapl_gt <- gtrends(keyword = 'apple inc', time = paste(from,to,sep = " "), gprop = "news", onlyInterest = T)
  
  tsla_gt <- gtrends(keyword = 'tesla', time = paste(from,to,sep = " "), gprop = "news", onlyInterest = T)
  
  amzn_gt <- gtrends(keyword = 'amazon', time = paste(from,to,sep = " "), gprop = "news", onlyInterest = T)
  
  googl_gt <- gtrends(keyword = 'google', time = paste(from,to,sep = " "), gprop = "news", onlyInterest = T)
  
  comps <- c(salm_gt,nhy_gt,tel_gt,msft_gt,fb_gt,aapl_gt,tsla_gt,amzn_gt, googl_gt)
  comps_names <- c('salm','nhy_hdr','tel','msft','fb','aapl','tsla','amzn','googl')
  
  hit_set_1 <- tibble(number_after = 1:length(amzn_gt$interest_over_time$hits),  #initiating a tibble, (could be done in a different way but yolo)
                      date = amzn_gt$interest_over_time$date)
  for (i in 1:length(comps)) {
    zzz = as.vector(comps[i]$interest_over_time$hits)
    hit_set_1 <- cbind(hit_set_1,zzz)
    colnames(hit_set_1)[i+2]<-paste(comps_names[i],"hits",sep = "_")
    
  }
  return(hit_set_1)
}


GOOGL <- GOOGL%>%
  mutate(hits = hit_set$googl_hits)

plot <- GOOGL%>%
  ggplot()+
  geom_line(aes(x = Date,y=hits, color = "red"))+
  geom_line(aes(x = Date, y = Normalized))







