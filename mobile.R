# Mobile Price
install.packages("corrgram")

library(ggplot2)
library(corrgram)

mobile_test_df <- read.csv("Data/test.csv")
mobile_train_df <- read.csv("Data/train.csv")

# Remove id in data test (first column)
mobile_test_df <- mobile_test_df[-1]

str(mobile_train_df)
dim(mobile_train_df)
head(mobile_train_df)
corrgram(mobile_train_df[6:16], order = TRUE,
         upper.panel = panel.pie)

#copy data type from numeric to factor
mobile_train_df$price_range <- factor(mobile_train_df$price_range, levels = c(0,1,2,3),
                       labels = c("low cost", "medium cost", "high cost", "very high cost"))

mobile_train_df$blue <- factor(mobile_train_df$blue, levels = c(0,1),
                                  labels = c("not", "yes"))
mobile_train_df$dual_sim <- factor(mobile_train_df$dual_sim, levels = c(0,1),
                               labels = c("not", "yes"))
mobile_train_df$four_g <- factor(mobile_train_df$four_g, levels = c(0,1),
                               labels = c("not", "yes"))
mobile_train_df$three_g <- factor(mobile_train_df$three_g, levels = c(0,1),
                               labels = c("not", "yes"))
mobile_train_df$touch_screen <- factor(mobile_train_df$touch_screen, levels = c(0,1),
                               labels = c("not", "yes"))
mobile_train_df$wifi <- factor(mobile_train_df$wifi, levels = c(0,1),
                               labels = c("not", "yes"))
## Univariate Data Analysis
ggplot(data=mobile_train_df, aes(x = price_range)) +
  geom_bar()
ggplot(data = mobile_train_df, aes(y=price_range)) +
  geom_boxplot() +
  labs(title = "Mobile Price Classification", y="Price Range")

## Bivariate Data Analysis
ggplot(mobile_train_df, aes(x=price_range, fill = blue)) +
  geom_bar(position = "dodge")
ggplot(mobile_train_df, aes(x=price_range, fill = dual_sim)) +
  geom_bar(position = "dodge")
ggplot(mobile_train_df, aes(x=price_range, fill = four_g)) +
  geom_bar(position = "dodge")
ggplot(mobile_train_df, aes(x=price_range, fill = three_g)) +
  geom_bar(position = "dodge")
ggplot(mobile_train_df, aes(x=price_range, fill = touch_screen)) +
  geom_bar(position = "dodge")
ggplot(mobile_train_df, aes(x=price_range, fill = wifi)) +
  geom_bar(position = "dodge")

ggplot(data=mobile_train_df, aes(x=ram, y=n_cores,
                        shape=price_range, color=price_range)) +
  geom_point() +
  labs(title = "Mobile Price Classification", x="Ram", y="Cores")

# change to numeric 
mobile_train_df$fc <- as.numeric(mobile_train_df$fc)
mobile_train_df$int_memory <- as.numeric(mobile_train_df$int_memory)
mobile_train_df$mobile_wt <- as.numeric(mobile_train_df$mobile_wt)
mobile_train_df$n_cores <- as.numeric(mobile_train_df$n_cores)
mobile_train_df$px_height <- as.numeric(mobile_train_df$px_height)
mobile_train_df$px_width <- as.numeric(mobile_train_df$px_width)
mobile_train_df$ram <- as.numeric(mobile_train_df$ram)
mobile_train_df$sc_h <- as.numeric(mobile_train_df$sc_h)
mobile_train_df$sc_w <- as.numeric(mobile_train_df$sc_w)
mobile_train_df$talk_time <- as.numeric(mobile_train_df$talk_time)

# Model
## logistic regression
fit.logit <- glm(price_range~. ,
                 data = mobile_train_df,
                 family = binomial)
summary(fit.logit)

## Decision tree
library(party)
fit.ctree <- ctree(price_range~. , data = mobile_train_df)
plot(fit.ctree, main = "Conditional Inference Tree")

## Random forest
library(randomForest)

set.seed(2021)
fit.forest <- randomForest(formula = price_range ~ ., data = mobile_train_df,
                           na.action = na.roughfix,
                           importance = TRUE)
fit.forest

## Support Vector Machine
library(e1071)
set.seed(2021)
fit.svm <- svm(price_range~., data=mobile_train_df)
fit.svm

performance <- function(table, n=2){
  tp = table[2,2]
  tn = table[1,1]
  fp = table[1,2]
  fn = table[2,1]
  
  sensitivity = tp/(tp+fn) # recall
  specificity = tn/(tn+fp)
  ppp = tp/(tp+fp) # precision
  npp = tn/(tn+fn)
  hitrate = (tp+tn)/(tp+tn+fp+fn) # accuracy
  
  result <- paste("Sensitivity = ", round(sensitivity, n),
                  "\nSpecificity = ", round(specificity, n),
                  "\nPositive Predictive Value = ", round(ppp,n), "\nNegative Predictive Value = ",round(npp, n),
                  "nAccuracy = ", round(hitrate, n))
  cat(result)
}
  
prob <- predict(fit.logit, test_df, type = "response")
  logit.pred <- factor(prob > 0.5, levels = c(FALSE, TRUE),
                       labels = c("low cost", "medium cost", ""))
  
  logit.perf <- table(test_df$diagnosis, logit.pred,
                      dnn = c("Actual", "predicted"))
  logit.perf
  
  performance(logit.perf)

  

