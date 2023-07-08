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


# Classification Tree with CTREE (multivar. Model) ####

#install.packages("rpart")
library(rpart)

# Build a more complex tree with the full model ####

tree<-rpart(formula=model , # Dependent variable and independent variables ("." for all variables in the data)
            data=xsell_train, # Training data set
            na.action = na.rpart, # What should be done with missings?
            method="class", # Decision problem (here: categorical variable, so "classification")
            parms = list(split = 'gini'), # Gini Index as split criterion
            control=rpart.control(minsplit=20, # Minimal number of obs. in a node before a split is attempted
                                  cp = 0.0001, # Complexity param.: min. increase in split criterion to grow the tree
                                  #usesurrogate = 2, # How many surrogates are used
                                  maxdepth = 15)) #max. tree depth

library(rpart.plot)
rpart.plot(tree,extra=101)


#install.packages("rattle")
#install.packages("rpart.plot")
#install.packages("RColorBrewer")
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(tree, caption = NULL)


summary(tree)
printcp(tree)
tree$variable.importance

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



########## RANDOM FOREST ############
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
plot(rf)

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




###### XGBOOST ######


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


## -------------- Dependency Plot --------


#dedicated package for PDPs
install.packages("pdp")
library(pdp)
#library(ggplot2)

# Partial Dependence Plot Decision Tree#
library(plotmo)
library(ggplot2)

plotmo(tree, type="prob")

varImp(tree)

######### PDP in RANDOM FOREST ##################   
partialPlot(rf, pred.data=xsell_valid, x.var ="nr_products")

pd <- partial(rf, pred.var = c("age", "customer_tenure_months"))
pdp1 <- plotPartial(pd)


partial(rf, # model from which to generate the plot
        pred.var = c("age","customer_tenure_months"), # variables for which to measure the marginal effects 
        prob=TRUE, # convert logits to probabilities
        type="auto", # automatically identify the type of prediction (classification or regression)
        check.class=FALSE, # skip check of classes (sometimes gives errors)
        plot=TRUE, # plot the results
        plot.engine = "ggplot2", # use ggplot for a "nicer" plot
        trim.outliers=TRUE # trim outliers
) +
  ggtitle("Predicted xsell Probability RandomForest")

plot(gbm,i.var=3)


partial(gbm, # model from which to generate the plot
        pred.var = c("age","nr_products"), # variables for which to measure the marginal effects 
        prob=TRUE, # convert logits to probabilities
        n.trees=400,
        type="auto", # automatically identify the type of prediction (classification or regression)
        check.class=FALSE, # skip check of classes (sometimes gives errors)
        plot=TRUE, # plot the results
        plot.engine = "ggplot2", # use ggplot for a "nicer" plot
        trim.outliers=TRUE # trim outliers
) +
  ggtitle("Predicted xsell Probability Gradient Boosting")



#########LIME###################################
library(lime)
explain_lime <- lime(xsell_train, gbm)
explanation_lime <- explain(xsell_xs, explain_lime)

#############shap for Random Forest #####################
install.packages("iml")
library(iml)
# separate independent vars inthe model
X <- xsell_train[ ,c("age","logins_mobile", "customer_tenure_months", "overdraft", "vol_eur_inflows" , "vol_eur_outflows" , "nr_girocard_trx_90d")]

predictor <- Predictor$new(rf, data=X, y=xsell_train$xsell)

# Shapley values: plot for different observations
shapley <- Shapley$new(predictor,x.interest=X[50, ])
shapley$results
plot(shapley) 
shapley <- Shapley$new(predictor, x.interest = X[10, ])
shapley$plot()




###########SHAP PLOTS for xgb; dedicated package SHAP for XGB #######################
install.packages("SHAPforxgboost")
library(SHAPforxgboost)


# **SHAP summary plots, use validation data set (smaller and faster)**

shap_long <- shap.prep(xgb_model = xgb, X_train = as.matrix(valid_m))
shap.plot.summary(shap_long)
#set boundaries for the x axes
shap.plot.summary(shap_long, x_bound  = 2)

# or directly
shap.plot.summary.wrap1(xgb, X = as.matrix(valid_m))
shap.plot.summary.wrap1(xgb, X = as.matrix(valid_m), top_n = 5)




# **SHAP dependence plot**
shap.plot.dependence(data_long = shap_long, x = "customer_tenure_months")
shap.plot.dependence(data_long = shap_long, x= "age",
                     color_feature = "Column_WV")
############################### TUNING MODELS ######################################################################

# Tuning of RPART complexity (significance) ###########################

library(caret)
# define settings
grid_rpart_cp <- expand.grid(.cp=seq(0.00001,0.0001,0.00001)) 
control_1 <- trainControl(method="cv", number=3) # 3-fold cross-validation

# Recode xsell variable into a factor 
xsell_train$xsell <- factor(xsell_train$xsell)

set.seed(1234)
rpart_cp <- train(model, data=xsell_train,method="rpart", tuneGrid=grid_rpart_cp, trControl=control_1,na.action=na.pass, metric="Accuracy")

# See results
rpart_cp$results
plot(rpart_cp)





# Tuning of CTREE ###########################

ct  <-expand.grid(.mincriterion=c(0.99, 0.95, 0.9, 0.85))
ctree_tuning <- train(model, data=xsell_train, method="ctree", tuneGrid=ct)


ctree_tuning
plot(ctree_tuning)



### Tuning RF ###########################################

rf_grid <-expand.grid(.mtry=c(2,3,4)) # different settings of mtry can be tested
training <- train(model, data=xsell_train, method="rf", ntree=500, sampsize=40000,  tuneGrid=rf_grid)
training


library(randomForest)
# let'S use e1071: a more flexible algorithm for RF tuning
library(e1071)
nodesize.tuning <- c(50,100,200)
ntree.tuning <- c(200,500,1000)
set.seed(1234) # fix random number generator seed for reproducibility

rf.tuning <- tune.randomForest(model, 
                               data=xsell_train, 
                               replace=TRUE, 
                               sampsize=40000,  
                               mtry=2,
                               nodesize=nodesize.tuning,
                               ntree = ntree.tuning)
rf.tuning

# Use the tuning results to re-run the model

set.seed(1234) # fix random number generator seed for reproducibility
rf_opt <- randomForest(model, data=xsell_train, 
                       ntree=500,       # number of trees
                       mtry=2,          # number variables selected at each node
                       nodesize=50,     # minimum node size
                       maxnodes=3,     # max amount of nodes
                       replace=TRUE,    # sample selection type
                       sampsize=20000)   # size of each sample

# Add prediction to validation data
xsell_valid$pred_rfopt <- predict(rf_opt,newdata=xsell_valid, type="response", na.action=na.pass)


#### now lets calculate the forecast and check the AUC of both versions
xsell_valid$pred_rf <- predict(rf, newdata=xsell_valid,type="response", na.action=na.pass)

summary(xsell_valid$pred_rf)
summary(xsell_valid$pred_rfopt)

library(pROC) 
### Compare models with ROC  Doing this, pls rund the "normal" RF model form session 6 and compare ####
roc_rf <- roc(xsell_valid$xsell,xsell_valid$pred_rf, percent=TRUE, plot=TRUE, print.auc=TRUE,grid=TRUE)
roc_rfopt <- roc(xsell_valid$xsell,xsell_valid$pred_rfopt, percent=TRUE, plot=TRUE, print.auc=TRUE,grid=TRUE)

plot(roc_rf,col="red",print.auc=TRUE)
plot(roc_rfopt,col="blue",print.auc=TRUE,add=TRUE)




### Tuning GBM #####################################################################

library(caret)


control_arg<-trainControl(method="cv", number=3) # only 3 to run faster, you can play with this number later

grid <- expand.grid(.interaction.depth=seq(3,5, by=1), 
                    .n.trees=seq(400,1000, by=200),
                    .shrinkage=c(0.001,0.01,0.05),
                    .n.minobsinnode=seq(100,1100, by=500))

set.seed(1234)
tune_gbm <- train(model, data=xsell_train, method="gbm",distribution="bernoulli",tuneGrid=grid,
                  bag.fraction=0.25, na.action=na.pass, # na.pass important for caret not to exit due to missing values, as GBM can handle them. 
                  trControl=control_arg, verbose=TRUE)
tune_gbm

library(gbm)
#now use gbm with tuned parameters
gbm_opt <- gbm(formula=model,		             # Model 
               distribution="bernoulli",     # Option needed for binary response variable
               data=xsell_train, 	           # data set
               n.trees=600,		               # Number trees / iterations
               interaction.depth=3,          # max depth of each tree
               shrinkage=0.01, 	             # Learning (shrinkage) rate
               n.minobsinnode=100,	           # Minimum number of obs per node
               bag.fraction = 0.5, 	           # size of random samples (if <1 you're also doing bagging)
               verbose=FALSE)	               # show in-between-steps TRUE/FALSE



# compare the tuned model with the original one form the "ensemble" session; rund the gdm model from that session
# Generate predictions
xsell_valid$pred_gbm    <- predict(gbm, newdata=xsell_valid, type="response", na.action=na.pass) # this is the standard model
xsell_valid$pred_gbmopt <- predict(gbm_opt, newdata=xsell_valid,type="response",na.action = na.pass) # the tuned model 

# are they different?
cor(xsell_valid$pred_gbm,xsell_valid$pred_gbmopt)

### Compare models with ROC  ####
roc_gbm <-    roc(xsell_valid$xsell,xsell_valid$pred_gbm, percent=TRUE, plot=TRUE, print.auc=TRUE,grid=TRUE)
roc_gbmopt <- roc(xsell_valid$xsell,xsell_valid$pred_gbmopt, percent=TRUE, plot=TRUE, print.auc=TRUE,grid=TRUE)


#### UPLIFT PART







