#need to run the tidy_data.R (initializing the data set) before running this code
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



btc_delta <- combidata%>% #point graf showing absolute change in normalized stock value on the y-axis, and normalized hits in the x-axis
  filter(ticker == "BTC")%>%
  ggplot(aes(x = hits, y = abs_delta))+
  geom_point()+
  geom_smooth(method = 'lm')+
  ylab("absolute change in value")

facetin_normalized <- combidata%>% #
  group_by(ticker)%>%
  filter(ticker != "BTC")%>%
  ggplot(aes(x = hits, y= Normalized))+
  geom_point()+
  geom_smooth(method = 'lm')+
  facet_wrap(~ticker)+
  ylab("Normalized price")

total_delta <- combidata%>%
  filter(ticker != "BTC")%>%
  ggplot(aes(x = hits, y= Normalized))+
  geom_point()+
  geom_smooth(method = 'lm')+
  ylab("normalized price")
  

facetin_volume_hits <- combidata%>%
  group_by(ticker)%>%
  filter(ticker != "BTC")%>%
  ggplot(aes(x = hits, y= norm_volume))+
  geom_point()+
  facet_wrap(~ticker)+
  geom_smooth(method = 'lm')+
  ylab("normalized volume")
  

total_volume <- combidata%>%
  filter(ticker != "BTC")%>%
  ggplot(aes(x = hits, y= norm_volume))+
  geom_point()+
  geom_smooth(method = 'lm')+
  ylab("Normalized volume")


hits_bidielta <- combidata%>%
  filter(! ticker %in% c("DJI","GSPC"))%>%
  ggplot()+
  geom_point(aes(x = hits, y= norm_volume,color = big_delta))









