library(glmnet)
library(corrplot)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(glmnet)

data <- read.csv("C:/Users/choin/Downloads/Hannanote/Marketing_Campaign/clean_merge_data.csv")
data <- data[-c(1,2)]
head(data)
sum(is.na(data))
str(data)
data = data[ , ! names(data) %in% c("Dt_Customer")] 
data$Age <- as.numeric(data$Age)
data$Education <- as.factor(data$Education)
data$Marital_Status <- as.factor(data$Marital_Status)
data$age_class <- as.factor(data$age_class)
data$income_class <- as.factor(data$income_class)
data$Country <- as.factor(data$Country)
data[6:26] <- lapply(data[6:26], as.numeric)
data[28:30] <- lapply(data[28:30], as.numeric)
summary(data)

num_data <- data %>% select_if(is.numeric) %>% select(-total_children,-total_amount,-total_num_purchase, -AcceptedCmp3, -AcceptedCmp4,-AcceptedCmp5, -AcceptedCmp1, -AcceptedCmp2,-Complain,-Response)
str(num_data)
cor(num_data)
corrplot(cor(num_data), method='circle')


# Calculate the average spending for each category
avg_spend <- colMeans(data[, c('MntWines', "MntFruits", "MntMeatProducts", "MntFishProducts", "MntSweetProducts", "MntGoldProds")], na.rm = TRUE)

pro_lab <- c("Wines","Fruits", "Meat", "Fish", "Sweet", "Gold")

# Create a bar plot
barplot(avg_spend,
        names.arg = pro_lab,
        main = "Avg Spending by Product",
        xlab = "Product Category",
        ylab = "Avg Spending")

# Calculate the average channel for each category
avg_spend_cha <- colMeans(data[, c("NumDealsPurchases","NumWebPurchases", "NumCatalogPurchases","NumStorePurchases")], na.rm = TRUE)

cha_lab <- c("Deals","Web", "Catalog", "Store")

# Create a bar plot
barplot(avg_spend_cha,
        names.arg = cha_lab,
        main = "Avg Spending by Product",
        xlab = "Product Category",
        ylab = "Avg Spending")

# Calculate the total number of purchases by country
total_purc_count <- data %>% group_by(Country) %>%
  summarise(Total_purchase = sum(total_num_purchase, na.rm = TRUE)) %>%
  arrange(desc(Total_purchase))

total_purchases <- total_purc_count$Total_purchase
countries <- total_purc_count$Country

# Create a bar plot
barplot(total_purchases,
        names.arg = countries)

# Calculate the total number of spent by country
total_spent_count <- data %>% group_by(Country) %>%
  summarise(Total_spent = sum(total_amount, na.rm = TRUE)) %>%
  arrange(desc(Total_spent))

total_spents <- total_spent_count$Total_spent
countries <- total_spent_count$Country

# Create a bar plot
barplot(total_spents,
        names.arg = countries)


ggplot(data, aes(x= income_class, y = total_amount)) +
  geom_boxplot()

ggplot(data, aes(x= age_class, y = total_amount)) +
  geom_boxplot()

ggplot(data, aes(x= Education, y = total_amount)) +
  geom_boxplot()

ggplot(data, aes(x= Marital_Status, y = total_amount)) +
  geom_boxplot()

# Calculate the percentage of acceptance for each campaign
cam_success <- data %>%
  select(AcceptedCmp1, AcceptedCmp2, AcceptedCmp3, AcceptedCmp4, AcceptedCmp5, Response) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "Campaign", values_to = "Percent") %>%
  mutate(Percent = Percent * 100) %>%
  arrange(Percent)

ggplot(cam_success, aes(x = reorder(Campaign, Percent), y = Percent)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip coordinates to make the bars horizontal
  labs(x = "Campaign", y = "Accepted (%)", title = "Marketing Campaign Success Rate") 

#Regression
numdata1 <- num_data %>% mutate(total_num_purchase = data$total_num_purchase)
str(numdata1)

model1 <- lm(total_num_purchase~., data = numdata1)
summary(model1)

numdata2 <- numdata1[,c(2:12,18)]
str(numdata2)

model2 <- lm(total_num_purchase~., data = numdata2)
summary(model2)

numdata3 <- numdata2[,c(2:4,6:12)]
str(numdata3)

model3 <- lm(total_num_purchase~., data = numdata3)
summary(model3)

# campaign regression
numdata4 <- data %>% select_if(is.numeric) %>% select(-ID,-total_children,-total_amount,-NumDealsPurchases, -NumWebPurchases, -NumCatalogPurchases, -NumStorePurchases, -NumWebVisitsMonth)
str(numdata4)

model4 <- lm(total_num_purchase~., data = numdata4)
summary(model4)

numdata5 <- numdata4[, c(2:4, 6:11,14,19)]
str(numdata5)

model5 <- lm(total_num_purchase~., data = numdata5)
summary(model5)

# Ridge 
x <- as.matrix(numdata5[,1:10]) 
y <- as.double(numdata5[,11]) 
ridge <-cv.glmnet(x,y, family = 'gaussian', alpha = 0) 
plot(ridge)

ridge$lambda.min

coef(ridge, s=ridge$lambda.min)

# Lasso
lasso <- cv.glmnet(x,y, family= 'gaussian', alpha = 1) 
plot(lasso) 

lasso$lambda.min
coef(lasso, s=lasso$lambda.min)

# Elastic Net
elasticNet <- cv.glmnet(x,y, family='gaussian', alpha = 0.5) 
plot(elasticNet) 

elasticNet$lambda.min
coef(elasticNet, s= elasticNet$lambda.min)
