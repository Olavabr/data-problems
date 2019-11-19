library(glmnet)
library(broom)
library(data.table)

set.seed(911222)
companies <- c("AAPL","AMZN","FB", "GOOGL", "MSFT", "NHY", "SALM", "TEL", "TSLA")

combinedd <- combinedd %>%
  filter(! ticker %in% c("DJI","GSPC"))%>%
  group_by(ticker)%>%
  mutate(delta =(Normalized - lag(Normalized)))%>%
  mutate(abs_delta = abs(delta))%>%
  mutate(norm_volume = (Volume/max(Volume))*100)


## CREATE LAGGED VARIABLES
combinedd <- combinedd %>%
  group_by(ticker) %>%
  mutate(lNormalized = lag(Normalized), llNormalized = lag(Normalized, 2), lVolume = lag(norm_volume), 
         llVolume = lag(norm_volume, 2), lhits = lag(hits), llhits = lag(hits, 2)) %>%
  filter(llhits!="NA") # removes rows with missing values from lagging. 

## CREATE TRAINING AND TEST SAMPLE
train <- sample(1:260, 208) #all companies have the same number of rows = 262 (260 after removing missing values) 
trainset <- combinedd %>%
  group_by(ticker) %>%
  slice(train)

testset <- combinedd %>%
  group_by(ticker) %>%
  slice(-train)
  
## CREATE OLS REGRESSIONS ON PRICE FOR ALL COMPANIES
ols_price <- tibble() #initialise a reg summary tibble
ols_price_pred <- tibble()#initialise a pred summary tibble
mse_table <- tibble(ticker = companies, mse_price_ols = 0, mse_price_lasso = 0, mse_vol_ols = 0,
                    mse_vol_lasso = 0) #initialise a summary tibble of mse to compare.

#LOOP OVER ALL THE COMPANIES
for (i in companies) {
  regset <- trainset %>% #create set filtered on company for doing regressions
    filter(ticker == i)
  reg_price <- lm(Normalized ~ lNormalized + llNormalized + norm_volume + hits + lhits + llhits, data=regset) #regression
  tidyp <- tidy(reg_price) #tidies regression output into a tibble
  tidyp <- tidyp %>%
    mutate(ticker = i)
  ols_price <- rbind(ols_price, tidyp) #add results to the aggregate table
  predset <- testset %>% #filter testset on company to do predictions
    filter(ticker == i)
  pred_price <- predict(reg_price, newdata=predset) #predicted price
  actual_price <- testset %>% #actual price from the testing sample
    filter(ticker == i) %>%
    select("Normalized") %>%
    as.matrix()
  actual_price <- actual_price[,2] 
  class(actual_price) <- "numeric" #make sure matrix can be used for mathematical operations
  mse_price <- mean((pred_price-actual_price)^2) #calculates mean squared error
  confusionp <- tibble(actual_price, pred_price, mse_price, Date = predset$Date) #combine
  confusionp <- confusionp %>%
    mutate(ticker = i)
  ols_price_pred <- rbind(ols_price_pred, confusionp) #add predictions to aggregate table
  mse_table <- mse_table %>% #add mse to aggregate table
    mutate(mse_price_ols = replace(mse_price_ols, ticker==i, mse_price))
}

ols_price_pred[,1:3] <- round(ols_price_pred[,1:3], 3)
ols_price[,2:5] <- round(ols_price[,2:5],3) #round decimals
ols_price <- ols_price %>%
  mutate(sign = ifelse(abs(statistic) > 1.96, 1, 0)) #create dummy to display if variable is significant at 5% level.

##CREATE REGRESSIONS FOR ALL COMPANIES ON VOLUME
ols_vol <- tibble() #initialise a reg summary tibble
ols_vol_pred <- tibble()#initialise a pred summary tibble

#LOOP OVER ALL THE COMPANIES
for (i in companies) {
  regset <- trainset %>% #create set filtered on company for doing regressions
    filter(ticker == i)
  reg_vol <- lm(norm_volume ~ lVolume + Normalized + hits + lhits + llhits, data=regset) #regression
  tidyv <- tidy(reg_vol) #tidies regression output into a tibble
  tidyv <- tidyv %>%
    mutate(ticker = i)
  ols_vol <- rbind(ols_vol, tidyv) #add results to the aggregate table
  predset <- testset %>% #filter testset on company to do predictions
    filter(ticker == i)
  pred_vol <- predict(reg_vol, newdata=predset) #predicted price
  actual_vol <- testset %>% #actual price from the testing sample
    filter(ticker == i) %>%
    select("norm_volume") %>%
    as.matrix()
  actual_vol <- actual_vol[,2]
  class(actual_vol) <- "numeric" #make sure matrix can be used for mathematical operations
  mse_vol <- mean((pred_vol-actual_vol)^2) #calculates mean squared error
  confusionv <- tibble(actual_vol, pred_vol, mse_vol, Date = predset$Date) #combine
  confusionv <- confusionv %>%
    mutate(ticker = i)
  ols_vol_pred <- rbind(ols_vol_pred, confusionv) #add predictions to aggregate table
  mse_table <- mse_table %>% #add mse to aggregate table
    mutate(mse_vol_ols = replace(mse_vol_ols, ticker==i, mse_vol))
}

ols_vol_pred[,1:3] <- round(ols_vol_pred[,1:3], 3)
ols_vol[,2:5] <- round(ols_vol[,2:5],3) #round decimals
ols_vol <- ols_vol %>%
  mutate(sign = ifelse(abs(statistic) > 1.96, 1, 0)) #create dummy to display if variable is significant at 5% level.

##LASSO PRICE
lasso_price <- tibble() #initialise tibbles similar to the ones in OLS
lasso_price_coef <- tibble() #initialise tibbles similar to the ones in OLS


##LOOP THROUGH COMPANIES
for (i in companies) {
  #Create matrices to be used in glmnet
  yprice <- trainset %>%
    filter(ticker == i) %>%
    select("Normalized") %>%
    as.matrix()
  yprice <- yprice[,-1]
  class(yprice) <- "numeric"
  yprice <- as.matrix(yprice)
  xprice <- trainset %>%
    filter(ticker == i) %>%
    select("lNormalized", "llNormalized","norm_volume", "lVolume", "hits", "lhits", "llhits") %>%
    as.matrix()
  xprice <- xprice[,-1]
  class(xprice) <- "numeric"
  #Create matrices from test sample
  yprice_out <- testset %>%
    filter(ticker == i) %>%
    select("Normalized") %>%
    as.matrix()
  yprice_out <- yprice_out[,-1]
  class(yprice_out) <- "numeric"
  yprice_out <- as.matrix(yprice_out)
  xprice_out <- testset %>%
    filter(ticker == i) %>%
    select("lNormalized", "llNormalized", "norm_volume", "lVolume", "hits", "lhits", "llhits") %>%
    as.matrix()
  xprice_out <- xprice_out[,-1]
  class(xprice_out) <- "numeric"
  #LASSO
  predset <- testset %>%
    filter(ticker == i)
  cv_price <- cv.glmnet(y=yprice, x=xprice) 
  cv_price_pred <- predict(cv_price, newx=xprice_out, s='lambda.min') #How does lasso predict test data
  mse_cv_price <- mean((cv_price_pred-yprice_out)^2) #mse to compare with ols
  confusionp <- tibble(actual=yprice_out, pred = cv_price_pred, mse = mse_cv_price, Date=predset$Date, ticker=i) #combine results
  lasso_price <- rbind(lasso_price, confusionp) #add results to aggregate lasso table
  coeffs <- cbind(as.matrix(coef(cv_price)), ticker=i) #create table with coefficients from lasso, which ones are kept?
  lasso_price_coef <- rbind(lasso_price_coef, coeffs) #add coefficients to aggregate table
  mse_table <- mse_table %>% #add mse to aggregate table
    mutate(mse_price_lasso = replace(mse_price_lasso, ticker==i, mse_cv_price))

}

setDT(lasso_price_coef, keep.rownames = "term") #from package data.table. Converts the rownames into values in the table
lasso_price_coef <- lasso_price_coef %>% #tidy
  rename(coefficient = 2) %>%
  mutate(sign = ifelse(coefficient != 0, 1, 0))%>%
  mutate(coefficient = as.numeric(as.character(coefficient))) 
lasso_price_coef[,2] <- round(lasso_price_coef[,2],3)
lasso_price[,1:3] <- round(lasso_price[,1:3],3)

##LASSO VOLUME
lasso_vol <- tibble() #initialise tibbles similar to the ones in OLS
lasso_vol_coef <- tibble() #initialise tibbles similar to the ones in OLS

##LOOP THROUGH COMPANIES
for (i in companies) {
  #Create matrices to be used in glmnet
  yvol <- trainset %>%
    filter(ticker == i) %>%
    select("norm_volume") %>%
    as.matrix()
  yvol <- yvol[,-1]
  class(yvol) <- "numeric"
  yvol <- as.matrix(yvol)
  xvol <- trainset %>%
    filter(ticker == i) %>%
    select("lVolume", "llVolume","Normalized", "lNormalized", "hits", "lhits", "llhits") %>%
    as.matrix()
  xvol <- xvol[,-1]
  class(xvol) <- "numeric"
  #Create matrices from test sample
  yvol_out <- testset %>%
    filter(ticker == i) %>%
    select("norm_volume") %>%
    as.matrix()
  yvol_out <- yvol_out[,-1]
  class(yvol_out) <- "numeric"
  yvol_out <- as.matrix(yvol_out)
  xvol_out <- testset %>%
    filter(ticker == i) %>%
    select("lVolume", "llVolume", "Normalized", "lNormalized", "hits", "lhits", "llhits") %>%
    as.matrix()
  xvol_out <- xvol_out[,-1]
  class(xvol_out) <- "numeric"
  #LASSO
  predset <- testset %>%
    filter(ticker == i)
  cv_vol <- cv.glmnet(y=yvol, x=xvol)
  cv_vol_pred <- predict(cv_vol, newx=xvol_out, s='lambda.min') #How does lasso predict test data
  mse_cv_vol <- mean((cv_vol_pred-yvol_out)^2) #mse to compare with ols
  confusionv <- tibble(actual=yvol_out, pred = cv_vol_pred, mse = mse_cv_vol, Date=predset$Date, ticker=i)#combine results
  lasso_vol <- rbind(lasso_vol, confusionv) #add results to aggregate lasso table
  coeffs <- cbind(as.matrix(coef(cv_vol)), ticker=i) #create table with coefficients from lasso, which ones are kept?
  lasso_vol_coef <- rbind(lasso_vol_coef, coeffs)#add coefficients to aggregate table
  mse_table <- mse_table %>% #add mse to aggregate table
    mutate(mse_vol_lasso = replace(mse_vol_lasso, ticker==i, mse_cv_vol))

}

setDT(lasso_vol_coef, keep.rownames = "term") #from package data.table. Converts the rownames into values in the table
lasso_vol_coef <- lasso_vol_coef %>% #tidy
  rename(coefficient = 2) %>%
  mutate(sign = ifelse(coefficient != 0, 1, 0)) %>%
  mutate(coefficient = as.numeric(as.character(coefficient))) 
lasso_vol_coef[,2] <- round(lasso_vol_coef[,2],3)
lasso_vol[,1:3] <- round(lasso_vol[,1:3],3)


mse_table[,-1] <- round(mse_table[,-1],3)


##Create tables to use in paper
ols_price <- ols_price %>%
  as.matrix()
write.table(ols_price, file = "ols_price.txt", sep = ",", quote = FALSE, row.names = F)
ols_vol <- ols_vol %>%
  as.matrix()
write.table(ols_vol, file = "ols_vol.txt", sep = ",", quote = FALSE, row.names = F)
lasso_price_coef <- lasso_price_coef %>%
  as.matrix()
write.table(lasso_price_coef, file = "lasso_price_coef.txt", sep = ",", quote = FALSE, row.names = F)
lasso_vol_coef <- lasso_vol_coef %>%
  as.matrix()
write.table(lasso_vol_coef, file = "lasso_vol_coef.txt", sep = ",", quote = FALSE, row.names = F)
mse_table <- mse_table %>%
  as.matrix()
write.table(mse_table, file = "mse_table.txt", sep = ",", quote = FALSE, row.names = F)
