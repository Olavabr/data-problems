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
  mutate(big_delta = as.factor(tot)) ##assigning the outcome variable to the most volotile weeks, for each company






## CREATE TRAINING AND TEST SAMPLE
train <- sample(1:261, 208) #all companies have the same number of rows = 261
trainset <- combinedd_tree %>%
  group_by(ticker) %>%
  slice(train)

testset <- combinedd_tree %>%
  group_by(ticker) %>%
  slice(-train)

######## The analisys ########

##### first by looking at all the companies at once. ####

rocky <- rpart(big_delta ~ hits, data = trainset) #training the classefiation tree
predz <- predict(rocky, testset, type = "class") # predicting the outcome variables in the test dataset
testset$total_single <- predz  #adding the results to the test data set.

rocky_random <- randomForest(big_delta ~ hits, data = trainset) #training the random forest
predz_random <- predict(rocky_random, testset) # predicting the outcome variables in the test dataset
testset$total_single_random <- predz_random  #adding the results to the test data set.

table(testset$big_delta,testset$total_single) #creating a confusion matrix with the results from the classefication tree
table(testset$big_delta,testset$total_single_random) #creating a confusion matrix with the results from the random forest

  
####### now the analisys useing the variation of google searches for each company ########

companies <- c("AAPL","AMZN","FB", "GOOGL", "MSFT", "NHY", "SALM", "TEL", "TSLA") #making a list of all the company names

###classefication tree#######
guesses <- c() #creating an empty vector to store the results
for (i in companies) {
  training <- trainset%>% #finding the data of company i, that is in the training dataset
    filter(ticker == i)
  testing <- testset%>%
    filter(ticker == i)%>% #finding the data of company i, that is in the test dataset
    arrange(big_delta)
  
  balboa <- rpart(big_delta~ hits, training) #training the classefiation tree model for company i
  pred <- predict(balboa,testing, type = "class")  # predicting the outcome variables in the test dataset for company i
  guesses <- append(guesses,pred) #adding the results for company i to a list of all the results
}
guesses <- guesses -1 #for some reason, the predict function resulted in 1's and 2's.. not 0's and 1's...
testset$guess <- guesses # adding the results to the test dataset


#########random forest ########
guesses_random <- c() #creating an empty vector to store the results
for (i in companies) {
  training <- trainset%>% #finding the data of company i, that is in the training dataset
    filter(ticker == i)
  
  testing <- testset%>% #finding the data of company i, that is in the test dataset
    filter(ticker == i)%>%
    arrange(big_delta)
    
  
  balboa_random <- randomForest(big_delta ~ hits, training) #training the random forest model for company i
  pred_random <- predict(balboa_random,testing, type = "class") # predicting the outcome variables in the test dataset for company i
  guesses_random <- append(guesses_random,pred_random) #adding the results for company i to a list of all the results
}
guesses_random <- guesses_random -1 #for some reason, the predict function resulted in 1's and 2's.. not 0's and 1's...
testset$guess_random <- guesses_random # adding the results to the test dataset


#### making confusion matrecis for both the classefication tree, and the random forrest
table(testset$big_delta,testset$guess)

table(testset$big_delta,testset$guess_random)




##########for bitcoin################################ it follows the same prosedure as the analisys above
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
