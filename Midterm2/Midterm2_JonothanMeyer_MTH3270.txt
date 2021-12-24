# Jonothan Meyer
# Data Science - MTH 3270
# 05/02/21
# Midterm Project 2

small_business <- read.csv("C:\\Users\\Jonothan\\Desktop\\MSU-Spring2021\\Data Science\\Midterm\\icesiv_contest.csv")

library(dplyr)
library(ggplot2)
library(rpart)
library(randomForest)
library(partykit)
names(small_business)
head(small_business)


#1. Carry out a multiple regression analysis. You may choose any response variable
#(Y ) for your model, but it must be a numerical variable (not categorical). Likewise,
#you may use any explanatory (X) variables, but they too must be numerical (not
#categorical). Note that a categorical variable that's been coded using integer values
#is still considered to be a categorical variable.
# Summarize your fitted model by reporting the estimated model coefficients.
# Interpret the estimated model (coeffcients).
#Report the value of at least one measure of how well the model fits the data
#(e.g. the R2).
sb <- small_business %>% select(AGE1, PAYROLL_NOISY, EMPLOYMENT_NOISY, RECEIPTS_NOISY, PRMINC1, EDUC1,HOURS1,PCT1) #%>%
  #filter(PAYROLL_NOISY < 500, EMPLOYMENT_NOISY < 100, RECEIPTS_NOISY <1000)

index <- sample(1:nrow(sb), 20000, replace = FALSE)
head(sb[index,])

pairs(sb[index,])


#plot(PCT1~PAYROLL_NOISY, data=sb[index,])

prime.lm <- lm(PCT1~PAYROLL_NOISY+RECEIPTS_NOISY+EMPLOYMENT_NOISY, data=sb)
prime.lm <- lm(PCT1~PAYROLL_NOISY+RECEIPTS_NOISY, data=sb)
summary(prime.lm)
prime.lm


#2. Carry out a logistic regression analyses for predicting whether a business is the
#primary source of income for the first owner based on other explanatory variables
#from the data set.
#For the response (Y ) variable, you'll use the dichotomous PRMINC1 variable taking
#the value 1 if yes and 2 if no. You may use any explanatory (X) variable(s), but
#they must be numerical (not categorical).


sb <- small_business %>% select(AGE1, PAYROLL_NOISY, EMPLOYMENT_NOISY, RECEIPTS_NOISY, PRMINC1, EDUC1,HOURS1,PCT1) #%>%
  #filter(RECEIPTS_NOISY <5000, EMPLOYMENT_NOISY < 10, PAYROLL_NOISY < 1000)
index <- sample(1:nrow(sb), 1000000, replace = FALSE)

sb <- mutate(sb, PRMINC1 = ifelse(PRMINC1 == 1, yes = 1,
                                          no = 0))

# Building Logistic Regression Model
#employ.glm <- glm(PRMINC1~EMPLOYMENT_NOISY+PAYROLL_NOISY+RECEIPTS_NOISY, data = sb[index,], family = "binomial")
employ.glm <- glm(PRMINC1~HOURS1+EDUC1+EMPLOYMENT_NOISY+PAYROLL_NOISY+RECEIPTS_NOISY+AGE1, data = sb[index,], family = "binomial")
#employ.glm <- glm(PRMINC1~HOURS1+EDUC1+EMPLOYMENT_NOISY+PAYROLL_NOISY+RECEIPTS_NOISY+AGE1, data = sb, family = "binomial")
summary(employ.glm)
confint(employ.glm)

# Making predictions with this model
pred_index <- sample(1:nrow(sb), 5000, replace = FALSE) # Pulling x amount of random samples
y_pred <- predict(employ.glm, sb[pred_index,], type = "response") # Making predictons with our model
y_pred <- round(y_pred) # Rounding those preditions to 0 or 1
act_pred <- data.frame(Actual = sb[pred_index,]$PRMINC1, Predicted = y_pred) # Finding the actual response results
confusion <- table(act_pred) # Building a confusion table with the predictions and answers
confusion
length_values <- act_pred %>% filter(!is.na(Actual), !is.na(Predicted)) # Removing NA rows
sum(diag(confusion)) / nrow(length_values) # Finding accuracy of model

# Looking at a visualization of how 'HOURS1' relates to 'PRMINC1' in a Log Reg model.
ggplot(data = sb[pred_index,], mapping = aes(x = HOURS1, y = PRMINC1)) +
  ggtitle("'HOURS1 Relationship with the Probability or PRMINC1")
  geom_point() + geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE)

#3. Carry a machine learning classification procedure (decision tree, random forest, k
# nearest neighbor, or artificial neural network { your choice) for predicting one of
#the following categorical variables (your choice). You may use any explanatory (X)
#variables, but they must be numerical (not categorical).

#Education level of the first business owner EDUC1. You'll need to convert
#the 1, 2, ..., 7 values to "character" (so they won't be treated as numeri-
#cal responses by the model-fitting function in R) using as.character() with
#mutate().

sb <- small_business %>% select(AGE1, PAYROLL_NOISY, EMPLOYMENT_NOISY, RECEIPTS_NOISY, PRMINC1, EDUC1,HOURS1,PCT1)
sb <- mutate(sb, PRMINC1 = ifelse(PRMINC1 == 1, yes = 1,
                                    no = 0))

my.tree <- rpart(factor(PRMINC1) ~ EMPLOYMENT_NOISY+PAYROLL_NOISY+RECEIPTS_NOISY+AGE1,
                 data = sb)
                 control = rpart.control(minsplit = 1000)

par(xpd = TRUE)
#plot(my.tree, compress = TRUE)
#text(my.tree, use.n = TRUE)
#class(my.tree)

my.party.tree <- as.party(my.tree)
plot(my.party.tree)


#-----------------------------------------Other Answer for Question 3----------------------------

sb <- small_business %>% select(AGE1, PAYROLL_NOISY, EMPLOYMENT_NOISY, RECEIPTS_NOISY, PRMINC1, EDUC1,HOURS1, PCT1)
sb1 <- sb[complete.cases(sb),]
index <- sample(1:nrow(sb1), 5000, replace = FALSE)
my.forest <- randomForest(factor(EDUC1) ~ EMPLOYMENT_NOISY+PAYROLL_NOISY+RECEIPTS_NOISY+PCT1,
                          data = sb1[index,],
                          ntree = 800,
                          mtry = 5)
index2 <- sample(1:nrow(sb1), 1500, replace = FALSE)
pred <- predict(my.forest, newdata = sb1[index2,])
act_pred <- data.frame(Actual = sb1[index2,]$EDUC1, Predicted = pred)
head(act_pred)
confusion <- table(act_pred)
sum(diag(confusion)) / nrow(sb1[index2,])
my.forest
newdata <- data.frame(EMPLOYMENT_NOISY = c(1, 100), PAYROLL_NOISY = c(0, 5000), RECEIPTS_NOISY = c(50, 10000), PCT1 = c(10, 99))
predict(my.forest, newdata = newdata, type = "class")

sum(diag(my.forest$confusion)) / nrow(sb1[index,])


# Visualization just looking at the relationship of a few predictors
ggplot(data = sb1[index,], mapping = aes(x = RECEIPTS_NOISY, y = EMPLOYMENT_NOISY, color = factor(EDUC1), size = PAYROLL_NOISY)) +
  geom_point() +
  labs(title = "# of Employee's and Recipts of First Business Owners")


#Aside
summary <- small_business %>% select(AGE1, PAYROLL_NOISY, EMPLOYMENT_NOISY, RECEIPTS_NOISY, PRMINC1, EDUC1,HOURS1, PCT1) %>%
        group_by(EDUC1) %>% filter(!is.na(EDUC1)) %>% summarize(n = n(), prop = n/(nrow(small_business)-169742), mean_rec = mean(RECEIPTS_NOISY),
                                                                mean_emp = mean(EMPLOYMENT_NOISY))

summary
barplot(mean_emp~factor(EDUC1), data = summary)


