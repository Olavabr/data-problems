#need to run the tidy_data.R (initializing the data set) before running this code

library(rpart)
library(rpart.plot)
library(randomForest)


set.seed(69745354)
companies <- c("AAPL","AMZN","FB", "GOOGL", "MSFT", "NHY", "SALM", "TEL", "TSLA")

combinedd_tree <- combinedd %>% #Making some new varaibles and storin the it as a new set
  filter(ticker %in% companies)%>%
  group_by(ticker)%>%
  mutate(delta =(Normalized - lag(Normalized)))%>%
  mutate(lhits = lag(hits))%>%
  mutate(abs_delta = abs(delta))%>%
  mutate(norm_volume = (Volume/max(Volume))*100)%>%
  drop_na()


#### making an outcom variable, it will be a boolean variable 
zeros <- rep(0,235)
ones <- rep(1,26)
tot <- c(zeros,ones)


combinedd_tree <- combinedd_tree%>%
  group_by(ticker)%>%
  arrange(ticker,abs(delta))%>%
  drop_na()%>%
  mutate(big_delta = as.factor(tot))


### adding the outcome variable to each company as the 26 most volatile days



## CREATE TRAINING AND TEST SAMPLE
train <- sample(1:261, 208) #all companies have the same number of rows = 262 (260 after removing missing values) 
trainset <- combinedd_tree %>%
  group_by(ticker) %>%
  slice(train)

testset <- combinedd_tree %>%
  group_by(ticker) %>%
  slice(-train)

#in total
rocky <- rpart(big_delta ~ hits, data = trainset)
predz <- predict(rocky, testset, type = "class")
testset$total_single <- predz

rocky_random <- randomForest(big_delta ~ hits, data = trainset)
predz_random <- predict(rocky_random, testset)
testset$total_single_random <- predz_random

table(testset$big_delta,testset$total_single)
table(testset$big_delta,testset$total_single_random)

  
##for each company
guesses <- c()
for (i in companies) {
  training <- trainset%>%
    filter(ticker == i)
  testing <- testset%>%
    filter(ticker == i)%>%
    arrange(big_delta)
  
  balboa <- rpart(big_delta~ hits, training)
  rpart.plot(balboa)
  pred <- predict(balboa,testing, type = "class")
  guesses <- append(guesses,pred)
}
guesses <- guesses -1
testset$guess <- guesses


#########random forest stuff:
guesses_random <- c()
for (i in companies) {
  training <- trainset%>%
    filter(ticker == i)
  
  testing <- testset%>%
    filter(ticker == i)%>%
    arrange(big_delta)
    
  
  balboa_random <- randomForest(big_delta ~ hits, training)
  pred_random <- predict(balboa_random,testing, type = "class")
  guesses_random <- append(guesses_random,pred_random)
}
guesses_random <- guesses_random -1
testset$guess_random <- guesses_random


#### making tables of the results
table(testset$big_delta,testset$guess)

table(testset$big_delta,testset$guess_random)




##########for bitcoin################################
zeros_b <- rep(0,202)
ones_b <- rep(1,22)
tot_b <- c(zeros_b,ones_b,NA)

bit_coin <- combinedd%>%
  filter(ticker == "BTC")%>%
  mutate(delta = Normalized - lag(Normalized))%>%
  arrange(ticker, abs(delta))
bit_coin$big_delta <- as.factor(tot_b)

train_b <- sample(1:226, 198)
train_set_b <- bit_coin%>%
  slice(train_b)%>%
  drop_na()
test_set_b <- bit_coin%>%
  slice(-train_b)%>%
  drop_na()

#single tree
single_tull <- rpart(big_delta ~ hits, data = train_set_b)

rpart.plot(single_tull)

rocky <- predict(single_tull,test_set_b, type = "class")

test_set_b$guess <- rocky

#random forest 

tull <- randomForest(big_delta ~ hits, data = train_set_b)

rocky_random <- predict(tull,test_set_b, type = "class")

test_set_b$guess_random <- rocky_random

table(test_set_b$big_delta,test_set_b$guess)

table(test_set_b$big_delta,test_set_b$guess_random)


######################################################

