# Read the data set -------------------------------------------------------------------------------

# clear workspace
rm(list = ls())

wd <- "/Users/manuelscionti/Desktop/UNIVERSITA'/ERASMUS/CORSI/Predictive Modelling/Data/"
# # mofify path depending on where you saved the dataset

setwd(wd)

xsell <- get(load("xsell.RData"))

#recodings might be needed
xsell$vol_eur_inflows <- as.numeric(xsell$vol_eur_inflows)
xsell$vol_eur_outflows <- as.numeric(xsell$vol_eur_outflows)
xsell$vol_eur_debit <- as.numeric(xsell$vol_eur_debit)
xsell$vol_eur_credit <- as.numeric(xsell$vol_eur_credit)
xsell$income <- as.numeric(xsell$income)

# make first univariate analyses-----------------------------------------------------------------

View(xsell)
head(xsell)
names(xsell)
summary(xsell)

library(xtable)
library(gmodels)

# Analysis of first variables
table(xsell$age)
table(xsell$xsell)

barplot(table(xsell$age))


# check and modify missings ----------------------------------------------------------------------

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

# Aggregate in a plot savings xsell and age
xsell_agg <- aggregate(xsell ~ age, data=xsell, FUN="mean")
plot(xsell_agg,type="p",pch=19,col="blue",xlab="",ylab="",ylim=c(0,0.2))
title(main="xsell likelihood; split by Customers'Age",  xlab="Age (years)", ylab="Xsell Likelihood")

# ...or use ggplot (nicer formatting)
#install.packages("ggplot2")
library(ggplot2)

qplot(x=xsell_agg$age,y=xsell_agg$xsell,main="XSELL Likelihood; split by Customers'Age",
      xlab="Age (years)", ylab="xsell", color=I("blue"))  + theme_gray(base_size = 18)

# do same for variable "Logins mobile"
xsell_agg <- aggregate(xsell ~ logins_mobile, data=xsell, FUN="mean")
qplot(x=xsell_agg$logins_mobile,y=xsell_agg$xsell,main="XSELL Likelihood; split by Mobile Logins",
      xlab="Nr of mobile logins", ylab="xsell",  xlim=c(0,100), ylim=c(0,0.2), color=I("blue"))  + theme_gray(base_size = 18)

#Create DummyVariable to handle categorical vars ####


xsell$married <- ifelse(xsell$marital_status=="VH", 1, 0)
table(xsell$married)

# Create marital status dummies
#install.packages("psych")
library(psych)
status_dummies <- dummy.code(xsell$marital_status)

# Transform them to a factor variable (otherwise R thinks it's numeric!)
status_dummies <-apply(status_dummies,FUN=as.factor,MARGIN=2)
xsell <- data.frame(xsell,status_dummies) # or cbind(xsell,status_dummies)


# create an "overdraft" variable: if the client has used the overdraft within 90 days
xsell$overdraft <- ifelse(xsell$nr_days_when_giro_below_0 >0, 1, 0)
table(xsell$overdraft)
# check how important that variable is
aggregate(xsell ~ overdraft, data=xsell, FUN="mean")



### Weight of Evidence (WoE) ####

#install.packages("woe")
library(woe)
prop.table(table(xsell$marital_status))

woe <-woe(xsell,"marital_status", FALSE, "xsell", C_Bin=7,Bad=0,Good=1)

# add to xsell dataset
woe$marital_status <- woe$BIN 
xsell <- merge(xsell, woe[,c(8,12)], by="marital_status")
table(xsell$WOE)


# alternatively:
#install.packages("corrplot")
library(corrplot)
# select only the numeric variables
# sapply tests for each column, whether it is numeric or not
xsell_numeric<-xsell[sapply(xsell, is.numeric)]
correl_matrix<-cor(xsell_numeric,use="pairwise.complete.obs") # correlation matrix
corrplot(correl_matrix) # correlation plot


# save the new variables. We will use them in the next sessions
xsell_cleaned -> xsell

######## FEATURE REDUCTION ########

######### STEPWISE SELECTION ###############

# Need to be adjusted with actual variables
full_model <- xsell ~  acad_title + age + calls + complaints+customer_tenure_months+logins_desktop+logins_mobile+
  member_get_member_recommender+member_get_member_recommended+
  +nr_products+nr_relocations+
  nr_girocard_trx_90d + income+ overdraft + ext_city_size+
  ext_house_size+ext_purchase_power+married + age_2 


logit_all <- glm(full_model, family=binomial, data=xsell)
summary(logit_all)

logit_step <- step(logit_all, direction="both", trace=1)
formula(logit_step)
summary(logit_step)
logit_step$anova

### Lasso Regression ####
#install.packages("glmnet")
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
                          "ext_house_size" , "ext_purchase_power" ,  "loan_mailing")]

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


######## MACHINE LEARNING PART #############

# 1) Split dataset into training and validation ####
set.seed(12345) # fix random number generator seed for reproducibility
xsell_random <- xsell[order(runif(100000)),] #sort the data set randomly
xsell_valid <- xsell_random[1:20000, ]       # 20% / 20000 observations in the validation dataset
xsell_train <- xsell_random[20001:100000, ]   # 80% / 80000 in the training data set



# 2) Estimate a model using the training dataset ####
model <- xsell ~ age + logins_mobile + customer_tenure_months +overdraft + vol_eur_inflows + vol_eur_outflows + nr_girocard_trx_90d

logit <- glm(model, data = xsell_train, family="binomial")
summary(logit)


# 3) Generate preditions ####

# Prediction on the validation dataset
xsell_valid$pred_logit  <- predict(logit,newdata=xsell_valid, type="response")


# Recode the prediction into a binary variable
xsell_valid$predict <- ifelse(xsell_valid$pred_logit >.1,1,0)

table(xsell_valid$xsell, xsell_valid$predict)



# 5) Generate a confusion matrix with caret ####
#install.packages("caret")
#install.packages("e1071")
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


plot(roc1)

ggroc(roc1)
smooth(roc1)
power.roc.test(roc1)

plot(roc1,col="red",print.auc=TRUE)
lines(roc1,col="red") # plot with grid in the background



#install.packages("ggplot2")
library(ggplot2)
ggroc(roc1, colour="red")

coords(roc1, "best", ret=c("threshold", "specificity", "1-npv"), transpose=TRUE)




