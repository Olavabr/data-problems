setwd("C:/Users/Olav/Documents/Master/Pirate programming/opgg")

#Making summary statistics for the searches and normalized prices for the 9 companies

hits_sum <- combinedd %>% #summary statistics for the "hits" variable
  filter(! ticker %in% c("BTC","GSPC","DJI"))%>%
  select(Date, hits, ticker)%>%
  group_by(ticker) %>% #want to group by the companies in the summaries
  summarize(mean_hits = mean(hits),sd_hits = sd(hits),min_hits = min(hits),max_hits = max(hits))%>% #selecting what should be summarized
  as.matrix() #need to be converted to a matrix...

write.table(hits_sum, file = "hits_sum.txt", sep = ",", quote = FALSE, row.names = F) #... in order to write it to a text file
#this .txt file can be copied into a word/latex file and converted to a table quite easily

norm_sum <- combinedd %>% #summary statistics for the "Normalized", but done the same way as for hits
  filter(! ticker %in% c("BTC","GSPC","DJI"))%>%
  select(Date, Normalized,ticker)%>%
  group_by(ticker) %>% 
  summarize(mean_Normalized = mean(Normalized),sd_Normalized = sd(Normalized),
            min_Normalized = min(Normalized),max_Normalized = max(Normalized))%>%
  as.matrix()

write.table(norm_sum, file = "norms_sum.txt", sep = ",", quote = FALSE, row.names = F)