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

# replace missing values by 0
xsell$vol_eur_inflows[is.na(xsell$vol_eur_inflows)] <- 0
xsell$vol_eur_outflows[is.na(xsell$vol_eur_outflows)] <- 0
xsell$vol_eur_debit[is.na(xsell$vol_eur_debit)] <- 0
xsell$vol_eur_credit[is.na(xsell$vol_eur_credit)] <- 0
xsell$income[is.na(xsell$income)] <- 0


# replace by mean
xsell$income[is.na(xsell$income)] <- round(mean(xsell$income, na.rm = TRUE))
xsell$ext_city_size[is.na(xsell$ext_city_size)] <- round(mean(xsell$ext_city_size, na.rm = TRUE))
xsell$ext_house_size[is.na(xsell$ext_house_size)] <- round(mean(xsell$ext_house_size, na.rm = TRUE))
xsell$ext_purchase_power[is.na(xsell$ext_purchase_power)] <- round(mean(xsell$ext_purchase_power, na.rm = TRUE))

# checking missing data
any(is.na(xsell))

# cols.name <- colnames(xsell)
# stargazer(cols.name)

########################
# FEATURE ENGINEERING
#######################

### Create DummyVariable to handle categorical vars ####

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

xsell$xsell <- as.factor(xsell$xsell)
library(ggplot2)
ggplot(xsell, aes(x = xsell, fill = xsell)) +
  geom_bar() +
  scale_fill_manual(values = c("orange", "blue")) +
  labs(title = "Frequency of xsell",
       x = "xsell",
       y = "Frequency")

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


# ###################
# # CORRELATIONS
# ###################
#
# select only the numeric variables
xsell_numeric<-xsell[sapply(xsell, is.numeric)]
correl_matrix<-cor(xsell_numeric,use="pairwise.complete.obs") # correlation matrix
#stargazer(correl_matrix) # correlation plot




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

xsell_valid_SMOTE <- xsell_valid # test with SMOTE
xsell_train_SMOTE <- xsell_train # train with SMOTE




#############################################
#               S.M.O.T.E.
#############################################
#install.packages("performanceEstimation")
library(performanceEstimation)

print(table(xsell_train_SMOTE$xsell))
prop.table(table(xsell_train_SMOTE$xsell))
xsell_train_SMOTE$xsell <- as.factor(xsell_train_SMOTE$xsell)
xsell_train_SMOTE <- smote(xsell ~ ., xsell_train_SMOTE, perc.over=4, perc.under=2)
print(table(xsell_train_SMOTE$xsell))
prop.table(table(xsell_train_SMOTE$xsell))

xsell_train_SMOTE$xsell <- as.numeric(xsell_train_SMOTE$xsell)-1



##################################
#             UPLIFT
#################################


# 2) Estimate a model using the training dataset ####



model <- xsell ~ age + logins_mobile + customer_tenure_months +overdraft + vol_eur_inflows + vol_eur_outflows + nr_girocard_trx_90d



logit <- glm(model, data = xsell_train, family="binomial")
summary(logit)

#table modello
stargazer(logit)

# Prediction on the validation dataset
xsell_valid$pred_logit  <- predict(logit,newdata=xsell_valid, type="response")


# Recode the prediction into a binary variable
xsell_valid$predict <- ifelse(xsell_valid$pred_logit >.1,1,0)

table(xsell_valid$xsell, xsell_valid$predict)


# Lift and Gain Charts #################################################################

# Calculate deciles using the predicted xsell probability with package "Stat Measures"
#install.packages("StatMeasures")
library(StatMeasures)
#library(remotes)
#install_version("StatMeasures", "1.0")


# -------------------

xsell_valid$decile <-decile(vector=xsell_valid$pred_logit)
table(xsell_valid$decile)

# Calculate mean by deciles

# detach(ModelMetrics:data.table)
# install.packages("data.table", version = "1.14.8")
library(data.table) # take your time to learn how to use data.table, it makes your like a lot easier in many situations! 
# detach("package:ModelMetrics", unload = TRUE)

xsell_valid<-data.table(xsell_valid)
xsell_valid[,lapply(.SD,mean),by=decile, .SDcols=c("pred_logit","xsell")]

# Check the sum and mean of xsell in the top deciles
sum(xsell_valid$xsell) #number of xsellers in the validation dataset 
t1 <- subset(xsell_valid,decile>=8) # Get the only the customers of the top-3 deciles (8, 9 and 10)
mean(t1$xsell) #number of xsellers in the top-3-deciles

# Plot deciles (Lift chart)
library(ggplot2)
theme_set(theme_grey(base_size = 12)) # adjust font size
ggplot(xsell_valid[,.(mean_xsell=mean(xsell)),by=decile], #dataset to plot: calculates mean xsell per decile
       aes(x=decile, fill=I("darkblue"), y=mean_xsell)) + ylab("Average (True) xsell Probability") +
  xlab("Deciles by Predicted xsell Probability") + geom_bar(stat = "identity") + 
  scale_y_continuous(limits=c(0,0.3),labels = scales::percent) +
  scale_x_continuous(breaks=seq(1:10)) #+geom_smooth(method = "lm", colour = "darkcyan", size=1.2, aes(fill=I("gray22")))



# Gain chart
#install.packages("gains")
library(gains)
gains(xsell_valid$xsell,xsell_valid$pred_logit)
par(lwd=2, ps=12) # line thickness
plot(gains(xsell_valid$xsell,xsell_valid$pred_logit),
     xlab="Deciles",ylab="Mean xsell",
     legend=c("Mean xsell","Cumul. Mean xsell","Mean Predicted xsell"))

#check the "loan_mailing" variable

table(xsell$loan_mailing)
table(xsell$loan_mailing, xsell$xsell)

mean(xsell$xsell[xsell$loan_mailing==0])
mean(xsell$xsell[xsell$loan_mailing==1])

#### Graphs ####

### Dataset with average Pr(Response) by age
library(data.table)
pred_means <- NULL
pred_means<-data.table(xsell)[,.(avg_xsell_tr=mean(xsell[loan_mailing==1]),avg_xsell_ctl=mean(xsell[loan_mailing==0])),by=age] 
pred_means$uplift<-pred_means$avg_xsell_tr-pred_means$avg_xsell_ctl

library(ggplot2)

theme_set(theme_grey(base_size = 12))
ggplot(pred_means[age<=60], aes(age)) + 
  geom_point(aes(y = avg_xsell_tr, colour = "avg_xsell_tr"), lwd=1.2)+ 
  geom_point(aes(y = avg_xsell_ctl, colour = "avg_xsell_ctl"), lwd=1.2) + 
  # geom_line(data=pred_means, aes(y=avg_nogiro, x=age, color = "avg_nomail")) +
  # geom_line(data=pred_means, aes(y=avg_giro, x=age, color = "avg_v1_mail")) +
  #geom_smooth(data=pred_means, aes(y=avg_nomail, x=logins_mobile, color = "avg_nomail"), method = "glm", 
  #            method.args = list(family = "binomial"), se=FALSE) +
  #geom_smooth(data=pred_means, aes(y=avg_mail, x=logins_mobile, color = "avg_mail"), method = "glm", 
  #            method.args = list(family = "binomial"), se=FALSE) +
  labs(title = "", x = "Age", y = "Pr(Response)") + 
  scale_color_manual(name="Model",
                     values=c(avg_xsell_tr="red",avg_xsell_ctl="blue"))



qplot(pred_means$age, pred_means$uplift,xlab="Age", ylab="Mean-Uplift Pr(Response)")  +
  geom_line(data = pred_means, aes(x=age, y=uplift, color="uplift"), show.legend=FALSE) + 
  geom_point(color='blue', show.legend=FALSE) +
  scale_color_manual(name="Model",
                     values=c(uplift="blue"))


# library(devtools)
# install_github("https://github.com/cran/uplift/tree/master")
library(uplift)


# Important: upliftRF doesn't support factor variables - need to transform to numeric / dummies
# Important: upliftRF throws an error if the variables in the dataset have labels 


# add "+trt(giro_mailing)" to the model
model_upl <- xsell ~ age + logins_mobile + customer_tenure_months +overdraft + vol_eur_inflows + vol_eur_outflows + nr_girocard_trx_90d + trt(loan_mailing)


# Check balance of predictors between Treatment and Control (conditional independence of treatment and covariates)
# P-values are two-sided tests for H0 "no difference"
# We see many low p-values here, what do they tell you?

checkBalance(model_upl,data = xsell_train, report = "all")

# Details of the model: compute the average value of the response variable for each predictor by treatment indicator
explore(model_upl, data = xsell_train)
stargazer(explore(model_upl, data = xsell_train))

# Net Information Value and Weight of Evidence
niv.1 <- niv(model_upl,data = xsell_train, plotit = TRUE)
niv.1$niv
niv.1$nwoe

###### CCIF - Causal Conditional Inference Forest #########
upCCIF <- ccif(formula = model_upl,
               data = xsell_train,
               ntree = 50, 
               split_method = "Chisq", 
               pvalue = 0.05, 
               verbose = TRUE)

summary(upCCIF)
varImportance(upCCIF, plotit=TRUE, normalize=FALSE)

# Predictions on validation dataset, Note that thereare two predictions; with/without treatment
pred_CCIF <- NULL
pred_CCIF <- as.data.frame(predict(upCCIF, xsell_valid, predict.all=FALSE))
# rename the prediction variables
colnames(pred_CCIF)<-c("pred_CCIF_mail","pred_CCIF_nomail")
xsell_valid <- cbind(xsell_valid, pred_CCIF)

# Calculate uplift
xsell_valid$pred_CCIF_net <- xsell_valid$pred_CCIF_mail - xsell_valid$pred_CCIF_nomail
mean(xsell_valid$pred_CCIF_net)
mean(xsell_valid$pred_CCIF_mail)
mean(xsell_valid$pred_CCIF_nomail)


# Check the performance in 10 segments 
perf_CCIF_valid<-performance(xsell_valid$pred_CCIF_mail, xsell_valid$pred_CCIF_nomail, xsell_valid$xsell, xsell_valid$loan_mailing, direction=1, groups=10)
perf_CCIF_valid

### Dataset with average Pr(Response) by age
library(data.table)
pred_means <- NULL #clean old version
pred_means<-data.table(xsell_valid)[,.(avg_nomail=mean(pred_CCIF_nomail),avg_mail=mean(pred_CCIF_mail)),by=age] 


# Net response probabilities 
pred_means$uplift<-pred_means$avg_mail-pred_means$avg_nomail

# See differences graphically 

library(ggplot2)


theme_set(theme_grey(base_size = 12))
ggplot(pred_means, aes(age)) + 
  geom_point(aes(y = avg_nomail, colour = "avg_nomail"), lwd=1.2)+ 
  geom_point(aes(y = avg_mail, colour = "avg_mail"), lwd=1.2) + 
  geom_line(data=pred_means, aes(y=avg_nomail, x=age, color = "avg_nomail")) +
  geom_line(data=pred_means, aes(y=avg_mail, x=age, color = "avg_mail")) +
  #geom_smooth(data=pred_means, aes(y=avg_nomail, x=age, color = "avg_nomail"), method = "glm", 
  #            method.args = list(family = "binomial"), se=FALSE) +
  #geom_smooth(data=pred_means, aes(y=avg_mail, x=age, color = "avg_mail"), method = "glm", 
  #            method.args = list(family = "binomial"), se=FALSE) +
  labs(title = "", x = "Age", y = "Pr(Response)") + 
  scale_color_manual(name="Model",
                     values=c(avg_nomail="red",avg_mail="blue"))


qplot(pred_means$age, pred_means$uplift,xlab="Age", ylab="Mean-Uplift Pr(Response)")  +
  geom_line(data = pred_means, aes(x=age, y=uplift, color="uplift"), show.legend=FALSE) + 
  geom_point(color='blue', show.legend=FALSE) +
  scale_color_manual(name="Model",
                     values=c(uplift="blue"))


##### UpliftKNN #####
# Not working due to computing power limitation

xsell_valid <- xsell_valid[,1:32]

upKNN <- upliftKNN(xsell_train, xsell_valid, xsell_train$xsell, xsell_train$loan_mailing, k = 1, dist.method = "euclidean", 
                   p = 2, ties.meth = "min", agg.method = "mean")


### Random Forest Uplift Model ####

upl1 <- upliftRF(model_upl,
                 data = xsell_train,
                 split_method = "Chisq",
                 ntree=50,
                 verbose=TRUE)

summary(upl1)
varImportance(upl1, plotit=TRUE, normalize=FALSE)


# Predictions on validation dataset, Note that thereare two predictions; with/without treatment
pred_upl <- NULL
pred_upl <- as.data.frame(predict(upl1, xsell_valid, predict.all=FALSE))
# rename the prediction variables
colnames(pred_upl)<-c("pred_upl_mail","pred_upl_nomail")
xsell_valid <- cbind(xsell_valid, pred_upl)

# Calculate uplift
xsell_valid$pred_upl_net <- xsell_valid$pred_upl_mail - xsell_valid$pred_upl_nomail
mean(xsell_valid$pred_upl_net)
mean(xsell_valid$pred_upl_mail)
mean(xsell_valid$pred_upl_nomail)


# Check the performance in 10 segments 
perf_upl1_valid<-performance(xsell_valid$pred_upl_mail, xsell_valid$pred_upl_nomail, xsell_valid$xsell, xsell_valid$loan_mailing, direction=1, groups=10)
perf_upl1_valid



#### Graphs Uplift Model ####

### Dataset with average Pr(Response) by age
library(data.table)
pred_means <- NULL #clean old version
pred_means<-data.table(xsell_valid)[,.(avg_nomail=mean(pred_upl_nomail),avg_mail=mean(pred_upl_mail)),by=age] 


# Net response probabilities 
pred_means$uplift<-pred_means$avg_mail-pred_means$avg_nomail

# See differences graphically 

library(ggplot2)


theme_set(theme_grey(base_size = 12))
ggplot(pred_means, aes(age)) + 
  geom_point(aes(y = avg_nomail, colour = "avg_nomail"), lwd=1.2)+ 
  geom_point(aes(y = avg_mail, colour = "avg_mail"), lwd=1.2) + 
  geom_line(data=pred_means, aes(y=avg_nomail, x=age, color = "avg_nomail")) +
  geom_line(data=pred_means, aes(y=avg_mail, x=age, color = "avg_mail")) +
  #geom_smooth(data=pred_means, aes(y=avg_nomail, x=age, color = "avg_nomail"), method = "glm", 
  #            method.args = list(family = "binomial"), se=FALSE) +
  #geom_smooth(data=pred_means, aes(y=avg_mail, x=age, color = "avg_mail"), method = "glm", 
  #            method.args = list(family = "binomial"), se=FALSE) +
  labs(title = "", x = "Age", y = "Pr(Response)") + 
  scale_color_manual(name="Model",
                     values=c(avg_nomail="red",avg_mail="blue"))


qplot(pred_means$age, pred_means$uplift,xlab="Age", ylab="Mean-Uplift Pr(Response)")  +
  geom_line(data = pred_means, aes(x=age, y=uplift, color="uplift"), show.legend=FALSE) + 
  geom_point(color='blue', show.legend=FALSE) +
  scale_color_manual(name="Model",
                     values=c(uplift="blue"))


##################################
#             UPLIFT - SMOTE
#################################

# 2) Estimate a model using the training dataset ####
model_SMOTE <- xsell ~ age + logins_mobile + customer_tenure_months +overdraft + vol_eur_inflows + vol_eur_outflows + nr_girocard_trx_90d

logit_SMOTE <- glm(model_SMOTE, data = xsell_train_SMOTE, family="binomial")
summary(logit_SMOTE)

# Prediction on the validation dataset
xsell_valid_SMOTE$pred_logit  <- predict(logit_SMOTE,newdata=xsell_valid_SMOTE, type="response")


# Recode the prediction into a binary variable
xsell_valid_SMOTE$predict <- ifelse(xsell_valid_SMOTE$pred_logit >.36,1,0)

table(xsell_valid_SMOTE$xsell, xsell_valid_SMOTE$predict)


# Lift and Gain Charts #################################################################

# Calculate deciles using the predicted xsell probability with package "Stat Measures"
#install.packages("StatMeasures")
library(StatMeasures)
#library(remotes)
#install_version("StatMeasures", "1.0")


# -------------------

xsell_valid_SMOTE$decile <-decile(vector=xsell_valid_SMOTE$pred_logit)
table(xsell_valid_SMOTE$decile)

# Calculate mean by deciles

# detach(ModelMetrics:data.table)
# install.packages("data.table", version = "1.14.8")
library(data.table) # take your time to learn how to use data.table, it makes your like a lot easier in many situations! 
# detach("package:ModelMetrics", unload = TRUE)

xsell_valid_SMOTE<-data.table(xsell_valid_SMOTE)
xsell_valid_SMOTE[,lapply(.SD,mean),by=decile, .SDcols=c("pred_logit","xsell")]

# Check the sum and mean of xsell in the top deciles
sum(xsell_valid_SMOTE$xsell) #number of xsellers in the validation dataset 
t1 <- subset(xsell_valid_SMOTE,decile>=8) # Get the only the customers of the top-3 deciles (8, 9 and 10)
mean(t1$xsell) #number of xsellers in the top-3-deciles

# Plot deciles (Lift chart)
library(ggplot2)
theme_set(theme_grey(base_size = 12)) # adjust font size
ggplot(xsell_valid_SMOTE[,.(mean_xsell=mean(xsell)),by=decile], #dataset to plot: calculates mean xsell per decile
       aes(x=decile, fill=I("darkblue"), y=mean_xsell)) + ylab("Average (True) xsell Probability") +
  xlab("Deciles by Predicted xsell Probability") + geom_bar(stat = "identity") + 
  scale_y_continuous(limits=c(0,0.3),labels = scales::percent) +
  scale_x_continuous(breaks=seq(1:10)) #+geom_smooth(method = "lm", colour = "darkcyan", size=1.2, aes(fill=I("gray22")))



# Gain chart
#install.packages("gains")
library(gains)
gains(xsell_valid_SMOTE$xsell,xsell_valid_SMOTE$pred_logit)

stargazer(gains.tab)

par(lwd=2, ps=12) # line thickness
plot(gains(xsell_valid_SMOTE$xsell,xsell_valid_SMOTE$pred_logit),
     xlab="Deciles",ylab="Mean xsell",
     legend=c("Mean xsell","Cumul. Mean xsell","Mean Predicted xsell"))

#check the "loan_mailing" variable

table(xsell$loan_mailing)
table(xsell$loan_mailing, xsell$xsell)

mean(xsell$xsell[xsell$loan_mailing==0])
mean(xsell$xsell[xsell$loan_mailing==1])

#### Graphs ####

### Dataset with average Pr(Response) by age
library(data.table)
pred_means <- NULL
pred_means<-data.table(xsell)[,.(avg_xsell_tr=mean(xsell[loan_mailing==1]),avg_xsell_ctl=mean(xsell[loan_mailing==0])),by=age] 
pred_means$uplift<-pred_means$avg_xsell_tr-pred_means$avg_xsell_ctl

library(ggplot2)

theme_set(theme_grey(base_size = 12))
ggplot(pred_means[age<=60], aes(age)) + 
  geom_point(aes(y = avg_xsell_tr, colour = "avg_xsell_tr"), lwd=1.2)+ 
  geom_point(aes(y = avg_xsell_ctl, colour = "avg_xsell_ctl"), lwd=1.2) + 
  # geom_line(data=pred_means, aes(y=avg_nogiro, x=age, color = "avg_nomail")) +
  # geom_line(data=pred_means, aes(y=avg_giro, x=age, color = "avg_v1_mail")) +
  #geom_smooth(data=pred_means, aes(y=avg_nomail, x=logins_mobile, color = "avg_nomail"), method = "glm", 
  #            method.args = list(family = "binomial"), se=FALSE) +
  #geom_smooth(data=pred_means, aes(y=avg_mail, x=logins_mobile, color = "avg_mail"), method = "glm", 
  #            method.args = list(family = "binomial"), se=FALSE) +
  labs(title = "", x = "Age", y = "Pr(Response)") + 
  scale_color_manual(name="Model",
                     values=c(avg_xsell_tr="red",avg_xsell_ctl="blue"))



qplot(pred_means$age, pred_means$uplift,xlab="Age", ylab="Mean-Uplift Pr(Response)")  +
  geom_line(data = pred_means, aes(x=age, y=uplift, color="uplift"), show.legend=FALSE) + 
  geom_point(color='blue', show.legend=FALSE) +
  scale_color_manual(name="Model",
                     values=c(uplift="blue"))



# library(devtools)
# install_github("https://github.com/cran/uplift/tree/master")
library(uplift)


# Important: upliftRF doesn't support factor variables - need to transform to numeric / dummies
# Important: upliftRF throws an error if the variables in the dataset have labels 


# add "+trt(giro_mailing)" to the model
model_upl_SMOTE <- xsell ~ age + logins_mobile + customer_tenure_months +overdraft + vol_eur_inflows + vol_eur_outflows + nr_girocard_trx_90d + trt(loan_mailing)
xsell_train_SMOTE <- subset(xsell_train_SMOTE, loan_mailing %in% c(0, 1))
xsell_train_SMOTE$loan_mailing <- as.factor(xsell_train_SMOTE$loan_mailing)
xsell_train_SMOTE$loan_mailing <- as.numeric(xsell_train_SMOTE$loan_mailing)-1


# Check balance of predictors between Treatment and Control (conditional independence of treatment and covariates)
# P-values are two-sided tests for H0 "no difference"
# We see many low p-values here, what do they tell you?

checkBalance(model_upl_SMOTE,data = xsell_train_SMOTE, report = "all")

# Details of the model: compute the average value of the response variable for each predictor by treatment indicator
explore(model_upl_SMOTE, data = xsell_train_SMOTE)
stargazer(explore(model_upl_SMOTE, data = xsell_train_SMOTE))

# Net Information Value and Weight of Evidence
niv.1 <- niv(model_upl_SMOTE,data = xsell_train_SMOTE, plotit = TRUE)
niv.1$niv
niv.1$nwoe

###### CCIF #########
upCCIF <- ccif(formula = model_upl_SMOTE,
               data = xsell_train_SMOTE,
               ntree = 50, 
               split_method = "Chisq", 
               pvalue = 0.05, 
               verbose = TRUE)
summary(upCCIF)
varImportance(upCCIF, plotit=TRUE, normalize=FALSE)

# Predictions on validation dataset, Note that thereare two predictions; with/without treatment
library(dplyr)
xsell_valid_SMOTE <- xsell_valid_SMOTE %>% mutate_at(1:32, as.numeric)
xsell_train_SMOTE <- xsell_train_SMOTE %>% mutate_at(1:32, as.numeric)
xsell_valid_SMOTE <- xsell_valid_SMOTE[,1:32]

pred_CCIF <- NULL
pred_CCIF <- as.data.frame(predict(upCCIF, xsell_valid_SMOTE, predict.all=FALSE))
# rename the prediction variables
colnames(pred_CCIF)<-c("pred_CCIF_mail","pred_CCIF_nomail")
xsell_valid_SMOTE <- cbind(xsell_valid_SMOTE, pred_CCIF)

# Calculate uplift
xsell_valid_SMOTE$pred_CCIF_net <- xsell_valid_SMOTE$pred_CCIF_mail - xsell_valid_SMOTE$pred_CCIF_nomail
mean(xsell_valid_SMOTE$pred_CCIF_net)
mean(xsell_valid_SMOTE$pred_CCIF_mail)
mean(xsell_valid_SMOTE$pred_CCIF_nomail)


# Check the performance in 10 segments 
perf_CCIF_valid<-performance(xsell_valid_SMOTE$pred_CCIF_mail, xsell_valid_SMOTE$pred_CCIF_nomail, xsell_valid_SMOTE$xsell, xsell_valid_SMOTE$loan_mailing, direction=1, groups=10)
perf_CCIF_valid

### Dataset with average Pr(Response) by age
library(data.table)
pred_means <- NULL #clean old version
pred_means<-data.table(xsell_valid_SMOTE)[,.(avg_nomail=mean(pred_CCIF_nomail),avg_mail=mean(pred_CCIF_mail)),by=age] 


# Net response probabilities 
pred_means$uplift<-pred_means$avg_mail-pred_means$avg_nomail

# See differences graphically 

library(ggplot2)


theme_set(theme_grey(base_size = 12))
ggplot(pred_means, aes(age)) + 
  geom_point(aes(y = avg_nomail, colour = "avg_nomail"), lwd=1.2)+ 
  geom_point(aes(y = avg_mail, colour = "avg_mail"), lwd=1.2) + 
  geom_line(data=pred_means, aes(y=avg_nomail, x=age, color = "avg_nomail")) +
  geom_line(data=pred_means, aes(y=avg_mail, x=age, color = "avg_mail")) +
  #geom_smooth(data=pred_means, aes(y=avg_nomail, x=age, color = "avg_nomail"), method = "glm", 
  #            method.args = list(family = "binomial"), se=FALSE) +
  #geom_smooth(data=pred_means, aes(y=avg_mail, x=age, color = "avg_mail"), method = "glm", 
  #            method.args = list(family = "binomial"), se=FALSE) +
  labs(title = "", x = "Age", y = "Pr(Response)") + 
  scale_color_manual(name="Model",
                     values=c(avg_nomail="red",avg_mail="blue"))


qplot(pred_means$age, pred_means$uplift,xlab="Age", ylab="Mean-Uplift Pr(Response)")  +
  geom_line(data = pred_means, aes(x=age, y=uplift, color="uplift"), show.legend=FALSE) + 
  geom_point(color='blue', show.legend=FALSE) +
  scale_color_manual(name="Model",
                     values=c(uplift="blue"))






##### UpliftKNN #####

xsell_valid_SMOTE <- xsell_valid_SMOTE[,1:32]

upKNN <- upliftKNN(xsell_train_SMOTE, xsell_valid_SMOTE, xsell_train_SMOTE$xsell, xsell_train_SMOTE$loan_mailing, k = 1, dist.method = "euclidean", 
                   p = 2, ties.meth = "min", agg.method = "mean")

#####################################
### Random Forest Uplift Model ####
###################################


upl1_SMOTE <- upliftRF(model_upl_SMOTE,
                 data = xsell_train_SMOTE,
                 split_method = "Chisq",
                 ntree=50,
                 verbose=TRUE)

summary(upl1_SMOTE)
varImportance(upl1_SMOTE, plotit=TRUE, normalize=FALSE)
library(dplyr)
xsell_valid_SMOTE <- xsell_valid_SMOTE %>% mutate_at(1:32, as.numeric)
xsell_train_SMOTE <- xsell_train_SMOTE %>% mutate_at(1:32, as.numeric)
xsell_valid_SMOTE <- xsell_valid_SMOTE[,1:32]


# Predictions on validation dataset, Note that thereare two predictions; with/without treatment
pred_upl_SMOTE <- NULL
pred_upl_SMOTE <- as.data.frame(predict(upl1, xsell_valid_SMOTE, predict.all=FALSE))
# rename the prediction variables
colnames(pred_upl_SMOTE)<-c("pred_upl_mail","pred_upl_nomail")
xsell_valid_SMOTE <- cbind(xsell_valid_SMOTE, pred_upl_SMOTE)

# Calculate uplift
xsell_valid_SMOTE$pred_upl_net <- xsell_valid_SMOTE$pred_upl_mail - xsell_valid_SMOTE$pred_upl_nomail
mean(xsell_valid_SMOTE$pred_upl_net)
mean(xsell_valid_SMOTE$pred_upl_mail)
mean(xsell_valid_SMOTE$pred_upl_nomail)


# Check the performance in 10 segments 
perf_upl1_valid<-performance(xsell_valid_SMOTE$pred_upl_mail, xsell_valid_SMOTE$pred_upl_nomail, xsell_valid_SMOTE$xsell, xsell_valid_SMOTE$loan_mailing, direction=1, groups=10)
perf_upl1_valid





#### Graphs Uplift Model ####

### Dataset with average Pr(Response) by age
library(data.table)
pred_means <- NULL #clean old version
pred_means<-data.table(xsell_valid_SMOTE)[,.(avg_nomail=mean(pred_upl_nomail),avg_mail=mean(pred_upl_mail)),by=age] 


# Net response probabilities 
pred_means$uplift<-pred_means$avg_mail-pred_means$avg_nomail

# See differences graphically 

library(ggplot2)


theme_set(theme_grey(base_size = 12))
ggplot(pred_means, aes(age)) + 
  geom_point(aes(y = avg_nomail, colour = "avg_nomail"), lwd=1.2)+ 
  geom_point(aes(y = avg_mail, colour = "avg_mail"), lwd=1.2) + 
  geom_line(data=pred_means, aes(y=avg_nomail, x=age, color = "avg_nomail")) +
  geom_line(data=pred_means, aes(y=avg_mail, x=age, color = "avg_mail")) +
  #geom_smooth(data=pred_means, aes(y=avg_nomail, x=age, color = "avg_nomail"), method = "glm", 
  #            method.args = list(family = "binomial"), se=FALSE) +
  #geom_smooth(data=pred_means, aes(y=avg_mail, x=age, color = "avg_mail"), method = "glm", 
  #            method.args = list(family = "binomial"), se=FALSE) +
  labs(title = "", x = "Age", y = "Pr(Response)") + 
  scale_color_manual(name="Model",
                     values=c(avg_nomail="red",avg_mail="blue"))


qplot(pred_means$age, pred_means$uplift,xlab="Age", ylab="Mean-Uplift Pr(Response)")  +
  geom_line(data = pred_means, aes(x=age, y=uplift, color="uplift"), show.legend=FALSE) + 
  geom_point(color='blue', show.legend=FALSE) +
  scale_color_manual(name="Model",
                     values=c(uplift="blue"))
















