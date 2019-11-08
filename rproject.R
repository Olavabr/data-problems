rm(list=ls())

setwd("~/Universitetet i Oslo/3.semester/ECON4170 Data Science for Economists/Term paper")

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

## Change variable names in the individual sets in order to be able to separate columns from each other when merging them
AAPL <- AAPL %>%
  setNames(paste('AAPL_',names(.)))
names(AAPL) <- str_replace_all(names(AAPL), c(" " = ""))

DJI <- DJI %>%
  setNames(paste('DJI_', names(.)))
names(DJI) <- str_replace_all(names(DJI), c(" " = ""))
  
FB <- FB %>%
  setNames(paste('FB_', names(.)))
names(FB) <- str_replace_all(names(FB), c(" " = ""))

GOOGL <- GOOGL %>%
  setNames(paste('GOOGL_', names(.)))
names(GOOGL) <- str_replace_all(names(GOOGL), c(" " = ""))

GSPC <- GSPC %>%
  setNames(paste('GSPC_', names(.)))
names(GSPC) <- str_replace_all(names(GSPC), c(" " = ""))

MSFT <- MSFT %>%
  setNames(paste('MSFT_', names(.)))
names(MSFT) <- str_replace_all(names(MSFT), c(" " = ""))

NHY <- NHY %>%
  setNames(paste('NHY_', names(.)))
names(NHY) <- str_replace_all(names(NHY), c(" " = ""))

SALM <- SALM %>%
  setNames(paste('SALM_', names(.)))
names(SALM) <- str_replace_all(names(SALM), c(" " = ""))
  
TEL <- TEL %>%
  setNames(paste('TEL_', names(.)))
names(TEL) <- str_replace_all(names(TEL), c(" " = ""))

TSLA <- TSLA %>%
  setNames(paste('TSLA_', names(.)))
names(TSLA) <- str_replace_all(names(TSLA), c(" " = ""))

AMZN <- AMZN %>%
  setNames(paste('AMZN_', names(.)))
names(AMZN) <- str_replace_all(names(AMZN), c(" " = ""))


## Create a tibble with all the companies
stock_prices <- as_tibble(c(AAPL, AMZN, DJI,FB, GOOGL, GSPC, MSFT, NHY, SALM, TEL, TSLA))


stock_prices <- stock_prices %>%
  select(ends_with("Open"), ends_with("Close"), #"Close" includes both "Close" and "Adj Close"
         ends_with("Volume"), ends_with("Normalized"), "AAPL_Date") %>%
  mutate(weeks = 1:nrow(stock_prices)) %>%
  rename(stock_dates = "AAPL_Date")

## Combine hit_set and stock_prices
combineddata <- inner_join(hit_set, stock_prices, by="weeks")

## Re-order variables in combineddata
combineddata <- combineddata %>%
  select("weeks", "ends_of_week_date", "stock_dates", starts_with("aapl"), starts_with("amzn"), starts_with("dji"), 
         starts_with("fb"), starts_with("googl"), starts_with("gspc"), starts_with("msft"), starts_with("nhy"), 
         starts_with("salm"), starts_with("tel"), starts_with("tsla")) %>% #starts_with has ignore.case=TRUE as default
  rename(AAPL_hits = "aapl_hits", AMZN_hits = "amzn_hits", FB_hits = "fb_hits", GOOGL_hits = "googl_hits", 
         MSFT_hits = "msft_hits", NHY_hits = "nhy_hits", SALM_hits = "salm_hits", TEL_hits = "tel_hits", TSLA_hits = "tsla_hits")
