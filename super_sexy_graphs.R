#need to run the tidy_data.R (initializing the data set) before running this code
library(tidyverse)

# adding some varialbes to the dataset: 
combidata <- combinedd%>%
  filter(! ticker %in% c("DJI","GSPC"))%>%
  group_by(ticker)%>%
  mutate(delta =(Normalized - lag(Normalized)))%>% #makes a variable that shows the change in normalized price
  mutate(abs_delta = abs(delta))%>%
  mutate(lhits = lag(hits))%>%
  mutate(norm_volume = (Volume/max(Volume))*100)%>%
  drop_na()


#LETS MAKE SOME GRAPHS

btc_trends <- combidata%>% #bitcoin trend
  filter(ticker == "BTC")%>%
  ggplot()+
  geom_line(aes(x = Date, y = hits, color = "Normalized hits"))+
  geom_line(aes(x = Date, y = Normalized, color = "Normalized price"))+
  ylab("value")


facetin_trends <- combidata%>% #trends for the six companies
  group_by(ticker)%>%
  filter(ticker != "BTC")%>%
  ggplot(aes(x =  Date),theme())+
  geom_line(aes(y = hits, color = "Normalized hits"))+
  geom_line(aes(y = Normalized, color = "Normalized stock price"))+
  facet_wrap(~ticker)+
  ylab("Value")


facetin_normalized <- combidata%>% #Displaying normalized stock price and hits in a scatter plot. and a linear regression line
  group_by(ticker)%>%
  filter(ticker != "BTC")%>%
  ggplot(aes(x = hits, y= Normalized))+
  geom_point()+
  geom_smooth(method = 'lm')+
  facet_wrap(~ticker)+
  ylab("Normalized price")

total_normalized <- combidata%>% #displaying all obeservations of normalized and hits in the same graph with a regression line
  filter(ticker != "BTC")%>%
  ggplot(aes(x = hits, y= Normalized))+
  geom_point()+
  geom_smooth(method = 'lm')+
  ylab("normalized price")
  

facetin_volume_hits <- combidata%>% #volume and hits in the same scatter plot. with a regression line, for each company
  group_by(ticker)%>%
  filter(ticker != "BTC")%>%
  ggplot(aes(x = hits, y= norm_volume))+
  geom_point()+
  facet_wrap(~ticker)+
  geom_smooth(method = 'lm')+
  ylab("normalized volume")
  

total_volume <- combidata%>% # all observations of volume and hits in the same graph with a regression line
  filter(ticker != "BTC")%>%
  ggplot(aes(x = hits, y= norm_volume))+
  geom_point()+
  geom_smooth(method = 'lm')+
  ylab("Normalized volume")









