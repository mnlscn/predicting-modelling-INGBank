################################
# LIBRARIES
###############################
library(xtable)
library(gmodels)
library(ggplot2)
library(corrplot)
library(car)
library(descr)
library(stargazer) #Create a ready to use table in LaTeX style
# https://www.r-bloggers.com/2013/01/stargazer-package-for-beautiful-latex-tables-from-r-statistical-models-output/


########################################
# EXPLORATORY ANALYSIS & PREPROCESSING
#######################################
# Read the data set -------------------------------------------------------------------------------

# clear workspace
rm(list = ls())

wd <- "/Users/manuelscionti/Desktop/UNIVERSITA'/ERASMUS/CORSI/Predictive Modelling/Data/"
setwd(wd)

xsell <- get(load("xsell.RData"))

#recodings might be needed
xsell$vol_eur_inflows <- as.numeric(xsell$vol_eur_inflows)
xsell$vol_eur_outflows <- as.numeric(xsell$vol_eur_outflows)
xsell$vol_eur_debit <- as.numeric(xsell$vol_eur_debit)
xsell$vol_eur_credit <- as.numeric(xsell$vol_eur_credit)
xsell$income <- as.numeric(xsell$income)

######################
# UNIVARIATE ANALYSIS
#####################

View(xsell)
head(xsell)
names(xsell)
summary(xsell)

# Analysis of first variables
table(xsell$age)
table(xsell$xsell)
barplot(table(xsell$age))

# unbalance class check
print(table(xsell$xsell))
print(prop.table(table(xsell$xsell))) #prob


###################
# HANDLING MISSING DATA
###################

# replace missing values by 0....
xsell$vol_eur_inflows[is.na(xsell$vol_eur_inflows)] <- 0
xsell$vol_eur_outflows[is.na(xsell$vol_eur_outflows)] <- 0
xsell$vol_eur_debit[is.na(xsell$vol_eur_debit)] <- 0
xsell$vol_eur_credit[is.na(xsell$vol_eur_credit)] <- 0
xsell$income[is.na(xsell$income)] <- 0


# ....or replace by mean
xsell$income[is.na(xsell$income)] <- round(mean(xsell$income, na.rm = TRUE))
xsell$ext_city_size[is.na(xsell$ext_city_size)] <- round(mean(xsell$ext_city_size, na.rm = TRUE))
xsell$ext_house_size[is.na(xsell$ext_house_size)] <- round(mean(xsell$ext_house_size, na.rm = TRUE))
xsell$ext_purchase_power[is.na(xsell$ext_purchase_power)] <- round(mean(xsell$ext_purchase_power, na.rm = TRUE))

# checking missing data
any(is.na(xsell))

###################
# FEATURE ENGINEERING
###################

#Create DummyVariable to handle categorical vars ####

xsell$married <- ifelse(xsell$marital_status=="VH", 1, 0)
table(xsell$married)

xsell <- xsell[,-c(6,10,14)]

# create an "overdraft" variable: if the client has used the overdraft within 90 days
xsell$overdraft <- ifelse(xsell$nr_days_when_giro_below_0 >0, 1, 0)
table(xsell$overdraft)
# check how important that variable is
aggregate(xsell ~ overdraft, data=xsell, FUN="mean")

#define some nonlinear Effects
xsell$age_2 <- xsell$age ^ 2
xsell$age_3 <- xsell$age ^ 3

stargazer(xsell, median = T,table.placement = "H")
###################
# BIVARIATE ANALYSIS 
###################

# Aggregate in a plot savings xsell and age
xsell_agg <- aggregate(xsell ~ age, data=xsell, FUN="mean")
plot(xsell_agg,type="p",pch=19,col="blue",xlab="",ylab="",ylim=c(0,0.2))
title(main="xsell likelihood; split by Customers'Age",  xlab="Age (years)", ylab="Xsell Likelihood")

qplot(x=xsell_agg$age,y=xsell_agg$xsell,main="XSELL Likelihood; split by Customers'Age",
      xlab="Age (years)", ylab="xsell", color=I("blue"))  + theme_gray(base_size = 18)


# same for variable "Logins mobile"
xsell_agg <- aggregate(xsell ~ logins_mobile, data=xsell, FUN="mean")
qplot(x=xsell_agg$logins_mobile,y=xsell_agg$xsell,main="XSELL Likelihood; split by Mobile Logins",
      xlab="Nr of mobile logins", ylab="xsell",  xlim=c(0,100), ylim=c(0,0.2), color=I("blue"))  + theme_gray(base_size = 18)


# same for variable "Logins Desktop"
xsell_agg <- aggregate(xsell ~ logins_desktop, data=xsell, FUN="mean")
qplot(x=xsell_agg$logins_desktop,y=xsell_agg$xsell,main="XSELL Likelihood; split by Desktop Logins",
      xlab="Nr of mobile logins", ylab="xsell",  xlim=c(0,50), ylim=c(0,0.2), color=I("blue"))  + theme_gray(base_size = 18)



# same for variable "customer tenure"
xsell_agg <- aggregate(xsell ~ customer_tenure_months, data=xsell, FUN="mean")
qplot(x=xsell_agg$customer_tenure_months,y=xsell_agg$xsell,main="XSELL Likelihood; split by Customer Tenure",
      xlab="Customer tenure in months", ylab="xsell", ylim=c(0,0.2) , xlim=c(0,200), color=I("blue"))  + theme_gray(base_size = 18)


# same for variable "girocard trx"
xsell_agg <- aggregate(xsell ~ nr_girocard_trx_90d, data=xsell, FUN="mean")
qplot(x=xsell_agg$nr_girocard_trx_90d,y=xsell_agg$xsell,main="XSELL Likelihood; split by Girocard usage",
      xlab="# Giro trx", ylab="xsell",  xlim=c(1,150), ylim=c(0,0.5),color=I("blue"))  + theme_gray(base_size = 18)


###################
# CORRELATIONS 
###################
# DA SISTEMARE, FORSE FARE DOPO LA FEATURE REDUCTION

# select only the numeric variables
xsell_numeric<-xsell[sapply(xsell, is.numeric)]
correl_matrix<-cor(xsell_numeric,use="pairwise.complete.obs") # correlation matrix
corrplot(correl_matrix) # correlation plot




#############################################
#FEATURE REDUCTION
#############################################

# We run a simple Logistic Regression for diagnostic and first idea of the coefficients 
full_model <- xsell ~  acad_title + age + calls + complaints+customer_tenure_months+logins_desktop+logins_mobile+
  member_get_member_recommender+member_get_member_recommended+
  +nr_products+nr_relocations+
  nr_girocard_trx_90d + income+ overdraft + ext_city_size+
  ext_house_size+ext_purchase_power+married + age_2 


logit_all <- glm(xsell ~., family=binomial, data=xsell)
summary(logit_all)
vif(logit_all) # Variance Inflation Factor (multicollinearity)
logit_all$deviance
logit_all$null.deviance
LogRegR2(logit_all)


#############################################
#FEATURE REDUCTION  - STEPWISE SELECTION
#############################################
logit_step <- step(logit_all, direction="both", trace=1)
formula(logit_step)
summary(logit_step)
logit_step$anova


#############################################
#FEATURE REDUCTION  - LASSO REGRESSION
#############################################

library(glmnet)
# glmnet requires a matrix of indep. vars and a vector with the dep. var

x_lasso_xsell <- xsell[,c("acad_title","age","calls" , "complaints" , "customer_tenure_months",
                          "logins_desktop" ,  "logins_mobile"     ,
                          "member_get_member_recommender", "member_get_member_recommended",
                          "nr_products" ,  "vol_eur_outflows"    ,
                          "prod_mortgages"  ,"prod_brokerage","prod_savings" ,
                          "nr_relocations" ,  "vol_eur_debit",
                          "nr_girocard_trx_90d","income",
                          "overdraft" , "ext_city_size",
                          "ext_house_size" , "ext_purchase_power" , "loan_mailing","age_2","age_3")]

summary(x_lasso_xsell)
y_lasso_xsell <- xsell$xsell
y_lasso_xsell <-data.frame(y_lasso_xsell)

# try 100 different lambdas to find the best
lasso <- cv.glmnet(as.matrix(x_lasso_xsell), as.matrix(y_lasso_xsell), type.measure="class",alpha=1, nlambda=100,family="binomial")

lasso$lambda.min # Optimal log(Lambda)
log(lasso$lambda.min) # Log(Lambda) of the simplest model that has an error statistically
# comparable to the error of the model with "optimal" lambda
# The error is within one standard deviation of the error produced by the optimal lambda

plot(lasso)


coef(lasso, s=lasso$lambda.min) # get coefficients from the optimal lambda


#coefficients with a lambda of lambda=0,005, resulting in ca. 10 remaining variables
# ln of 0,005 = -5,3
glmmod <- glmnet(as.matrix(x_lasso_xsell), as.matrix(y_lasso_xsell), type.measure="class",
                 alpha=1, lambda=0.0075,family="binomial")
glmmod$beta



#############################################
#               MACHINE LEARNING
#############################################

# 1) Split dataset into training and validation ####
set.seed(12345) # fix random number generator seed for reproducibility
xsell_random <- xsell[order(runif(100000)),] #sort the data set randomly
xsell_valid <- xsell_random[1:20000, ]       # 20% / 20000 observations in the validation dataset
xsell_train <- xsell_random[20001:100000, ]   # 80% / 80000 in the training data set


#############################################
#               LOGISTIC REGRESSION
#############################################

model <- xsell ~ age + logins_mobile + customer_tenure_months +overdraft + vol_eur_inflows + vol_eur_outflows + nr_girocard_trx_90d + prod_mortgages + income

logit <- glm(model, data = xsell_train, family="binomial")
summary(logit)


# 3) Generate preditions ####

# Prediction on the validation dataset
xsell_valid$pred_logit  <- predict(logit,newdata=xsell_valid, type="response")


# Recode the prediction into a binary variable
xsell_valid$predict <- ifelse(xsell_valid$pred_logit >.1,1,0)

table(xsell_valid$xsell, xsell_valid$predict)



# 5) Generate a confusion matrix with caret ####
library(caret)
library(e1071)

conf_matrix<-confusionMatrix(as.factor(xsell_valid$predict),as.factor(xsell_valid$xsell), 
                             positive="1", # which value is what we're trying to predict? Here, 1 (xsell)
                             dnn = c("Prediction", "True Data"))
conf_matrix

#F1 Score

conf_matrix$byClass["F1"]

# 7) ROC und AUC ####

library(pROC)

roc1 <- roc(xsell_valid$xsell,xsell_valid$pred_logit, percent=TRUE, plot=TRUE, print.auc=TRUE,grid=TRUE)
roc1a <- auc(xsell_valid$xsell,xsell_valid$pred_logit, percent=TRUE, plot=TRUE, print.auc=TRUE,grid=TRUE)
smooth(roc1)
power.roc.test(roc1)
coords(roc1, "best", ret=c("threshold", "specificity", "1-npv"), transpose=TRUE)
# threshold 0.08

#############################################
#               DECISION TREE
#############################################
#install.packages("rpart")
library(rpart)


tree<-rpart(formula=model , # Dependent variable and independent variables
            data=xsell_train, # Training data set
            na.action = na.rpart, # What should be done with missings?
            method="class", # Decision problem (here: categorical variable, so "classification")
            parms = list(split = 'gini'), # Gini Index as split criterion
            control=rpart.control(minsplit=20, # Minimal number of obs. in a node before a split is attempted
                                  cp = 0.0001, # Complexity param.: min. increase in split criterion to grow the tree
                                  #usesurrogate = 2, # How many surrogates are used
                                  maxdepth = 15)) #max. tree depth

#library(rpart.plot)
#rpart.plot(tree,extra=101)
# library(rattle)
# library(rpart.plot)
# library(RColorBrewer)
# fancyRpartPlot(tree, caption = NULL)


summary(tree)
printcp(tree)
tree$variable.importance

#############################################
#              PRUNING - DECISION TREE
#############################################

# now let the tree fully grow without any limitations
treeMAX <- rpart(model, data = xsell_train, method="class",minsplit=2, cp=0)
plot(treeMAX)
#text(treeMAX, xpd=TRUE) #add txt, but chart becomes a bit full...


# Finding the best cp and PRUNING

# checking for the optimal pruning step
treeMAX$cptable
plotcp(treeMAX)

# pruning the model at the best cp value
treeOpt <- prune(treeMAX, cp= 1.374141e-04)
plot(treeOpt)
#text(treeOpt, xpd=TRUE, cex=0.8)

# Predictions
xsell_valid$pred_tree1<-predict(treeOpt, newdata=xsell_valid, type="prob")[,2]


# Compare models via confusion matrix ####
library(caret)
xsell_valid$pred_tree_01 <- ifelse(xsell_valid$pred_tree1 >.1,1,0)
conf_matrix<-confusionMatrix(as.factor(xsell_valid$pred_tree_01),as.factor(xsell_valid$xsell),
                             positive="1", # which value is what we're trying to predict? Here, 1 (xsell)
                             dnn = c("Prediction", "Actual Data"))
conf_matrix
conf_matrix$byClass["F1"]


xsell_valid$pred_ctree <- as.numeric(xsell_valid$pred_ctree)
roc2 <- roc(xsell_valid$xsell,xsell_valid$pred_tree1, percent=TRUE, plot=TRUE, print.auc=TRUE,grid=TRUE)

library(rpart.plot)
tree$variable.importance # get the unscaled variable importance measure
plot(tree$variable.importance) # get the unscaled variable importance measure



#############################################
#               RANDOM FOREST
#############################################
### Build Random Forest Model ####

#install.packages("randomForest")
library(randomForest)

# CHECK VARIABLES
#or alternatively a full model with almost all vars
full_model <- xsell ~  acad_title + age + calls + complaints+customer_tenure_months+logins_desktop+logins_mobile+
  member_get_member_recommender+member_get_member_recommended+
  +nr_products+nr_relocations+
  nr_girocard_trx_90d + income+ overdraft + ext_city_size+
  ext_house_size+ext_purchase_power+married

# Recode xsell variable into a factor. Basically random forests requests a factor variable. I skip this recommendation for this exercise
xsell_train$xsell <- as.factor(xsell_train$xsell)
# Model the Random Forest
set.seed(12345) # fix random number generator seed for reproducibility
rf <- randomForest(full_model, # Model
                   data=xsell_train, # dataset (without missings)
                   ntree=200,       # Number of trees (more trees can improve performance, but takes more time to run)
                   mtry=3,          # m parameter: number of randomly selected variables the tree can consider for each split
                   nodesize=50,    # minimum number of observations in leaf nodes
                   maxnodes=10,     # max amount of nodes
                   replace=TRUE,    # Sampling with replacement (Y/N). If FALSE, then there's no bagging, you're just slicing your dataset
                   importance=TRUE, # calculate variable importance measure (Y/N) - helps to interpret the model!
                   sampsize=20000)   # size of each sample

# Check the results
rf
#plot(rf)

importance(rf)
varImpPlot(rf, type=2)
varUsed(rf)


tree10 <- getTree(rf, k=10, labelVar=TRUE)
tree10



# Generate preditions
# type="response" produces a binary prediction; "prob" the coresponding likelihood
xsell_valid$pred_rf <- predict(rf,newdata=xsell_valid, type="response", predict.all=FALSE)



# let'S check all predictions
pred_rf <- predict(rf,newdata=xsell_valid, type="response", predict.all=TRUE)
pred_rf <-as.data.frame(pred_rf)




### Compare models via confusion matrix ####
library(caret)
xsell_valid$rf_01 <- ifelse(xsell_valid$pred_rf >.1,1,0)
conf_matrix_rf<-confusionMatrix(as.factor(xsell_valid$rf_01),as.factor(xsell_valid$xsell),
                                positive="1", # which value is what we're trying to predict? Here, 1 (xsell)
                                dnn = c("Prediction", "True Data"))

conf_matrix_rf
conf_matrix_rf$byClass["F1"]



#### Compare models via ROC ####
library(pROC)

roc <- roc(xsell_valid$xsell,xsell_valid$pred_rf, percent=TRUE, plot=TRUE, print.auc=TRUE,grid=TRUE)

plot(roc,col="red",print.auc=TRUE)


#Variable importance
importance(rf)
varImpPlot(rf, type=2)

#another way of a graphical solution with package vip
#install.packages("vip")
library(vip) 
vip(rf, bar = TRUE, horizontal = FALSE, size = 1.5)  




#############################################
#               XGBOOST
#############################################


# Recode churn variable into a factor.
xsell_train$xsell <- factor(xsell_train$xsell)


#### Algorithm XGBOOST ####
install.packages("xgboost")
install.packages("Rtools")
install.packages("Matrix")
library(Rtools)
library(xgboost)
library(Matrix)

# Prepare the data (sparse matrix format)
train_m <-sparse.model.matrix(model, data = xsell_train)
valid_m <-sparse.model.matrix(model, data = xsell_valid)


train_label <-ifelse(xsell_train$xsell==1,1,0)
valid_label <-ifelse(xsell_valid$xsell==1,1,0)

# Run the model
set.seed(1234)
xgb <- xgboost(data=train_m,label=train_label,max.depth=3,eta=0.01,subsample=0.5,
               nrounds=600,objective="binary:logistic", verbose=0)

print(xgb,verbose=TRUE)
xgb


# feature performance and relevance
importance_matrix <- xgb.importance(model = xgb)
print(importance_matrix)
xgb.plot.importance(importance_matrix, cex=1.2)

#install.packages("DiagrammeR")
library(DiagrammeR)
xgb.plot.tree(model=xgb, trees=0, show_node_id=TRUE)

# Make predictions
xsell_valid$pred_xgb <- predict(xgb, newdata = valid_m)
xsell_valid$pred_xgb_factor <- factor(ifelse(xsell_valid$pred_xgb>.1,1,0))

summary(xsell_valid$pred_xgb_factor)


### Check accuracy with the confusion matrix ####
library(caret)

conf_matrix_xgb<-confusionMatrix(as.factor(xsell_valid$pred_xgb_factor),as.factor(xsell_valid$xsell),
                                 positive="1", # which value is what we're trying to predict? Here, 1 (xsell)
                                 dnn = c("Prediction", "True Data"))

conf_matrix_xgb
conf_matrix_xgb$byClass["F1"]



roc <- roc(xsell_valid$xsell,xsell_valid$pred_xgb, percent=TRUE, plot=TRUE, print.auc=TRUE,grid=TRUE)


#Variable importance
# feature performance and relevance
importance_matrix <- xgb.importance(model = xgb)
print(importance_matrix)
xgb.plot.importance(importance_matrix, cex=1.2)






