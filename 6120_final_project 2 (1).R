setwd("D:/UVA/STAT6120/Final Project")
individual <- read.csv("individual.csv")
household <- read.csv("household.csv")
houseind_m <- merge(individual, household, by.x = "hhid", by.y = "hhid")
houseind <- houseind_m[houseind_m$wave.x == houseind_m$wave.y, ]

houseind$wave <- houseind$wave.x
houseind$commid <- houseind$commid.x
houseind$region <- houseind$region.x
houseind <- subset(houseind, select = -c(commid.x, commid.y, wave.x, wave.y, region.x, region.y))

##replace 9 or -99 in binary variables with NAs
houseind$bicycle[houseind$bicycle == 9] <- NA
houseind$motorcycle[houseind$motorcycle == 9] <- NA
houseind$car[houseind$car == 9] <- NA
houseind$refrigerator[houseind$refrigerator == 9] <- NA
houseind$pressure_cooker[houseind$pressure_cooker == 9] <- NA
houseind$computer[houseind$computer == 9] <- NA
houseind$dvd[houseind$dvd == 9] <- NA
houseind$dwelling_size[houseind$dwelling_size == -99] <- NA

##change NA in binary variables to mode
NA_to_median <- function(x) { x[is.na(x)] <- median(x, na.rm = TRUE); x }
houseind[, 11:32] <- lapply(houseind[, 11:32], NA_to_median)
summary(houseind)

##change binary variables to factors
houseind[, 11:32] <- lapply(houseind[, 11:32], as.factor)
houseind[, 35]<- as.factor(houseind[,35])


##impute hh_size by other variables
data_hh <- subset(houseind, select = -c(protein,carb,fat,kcal,dwelling_size, region))
lm_hh <- lm(hh_size ~. , data = data_hh)
summary(lm_hh)
hh_size_predicted <- predict(lm_hh, data_hh[is.na(data_hh$hh_size),])
houseind$hh_size[is.na(houseind$hh_size)] <- hh_size_predicted

##impute dwelling_size by other variables
data_dwelling <- subset(houseind, select = -c(protein,carb,fat,kcal,hh_size, region))
lm_dw <- lm(dwelling_size ~. , data = data_dwelling)
summary(lm_dw)
dw_size_predicted <- predict(lm_dw, data_dwelling[is.na(data_dwelling$dwelling_size),])
houseind$dwelling_size[is.na(houseind$dwelling_size)] <- dw_size_predicted

#separate training set and testing set
set.seed(999)
a <-sample(5, length(houseind$hhid),replace = T)
b <- a ==1
houseind <- subset(houseind, select = -c(hhid, indid, protein, fat, carb, commid))
test <- houseind[b,]
train <- houseind[!b, ]
test.y <- test[, 1]
test.x <- test[, -1]
train.y <- as.matrix(train[, 1])
train.x <- as.matrix(train[, -1])

##function that calculate MSPE
f_mspe <- function(yi, yi_pred){
  mean((yi-yi_pred)^2)
}

##From this point on, houseind dataset is good to use
###ols regression
ols <- lm(kcal ~ . , data= train)
summary(ols)
anova(ols)
ols.predicted <- predict(ols, newdata = test.x)
ols_mspe <- f_mspe(test.y, ols.predicted)
ols_mspe

##check residual plot 
plot(resid(ols), train$kcal)
##check cook's distance
library(faraway)
cook <- cooks.distance(ols)
halfnorm(cook, nlab = 5, ylab = "Cook's Distance")
rownames(train)[79624]
train[79800,]
##remove outliers
train_rm_ol  <- train[-c(79624, 79800, 80035, 64278,64271), ]
ols_rm_ol <- lm(kcal ~ . , data= train_rm_ol)
summary(ols_rm_ol)
anova(ols_rm_ol)
ols_rm_ol.predicted <- predict(ols_rm_ol, newdata = test.x)
ols_rm_ol_mspe <- f_mspe(test.y, ols_rm_ol.predicted)
ols_rm_ol_mspe


##ols without dvd, electric fan 
ols_small <- lm(kcal ~. - electric_fan -dvd , data = train_rm_ol)
summary(ols_small)
anova(ols_small)
ols_small_predicted <- predict(ols_small, newdata = test.x)
ols_small_mspe <- f_mspe(test.y, ols_small_predicted)
ols_small_mspe

anova(ols_small, ols_rm_ol)

##Least Trimmed Squares
library(MASS)
lts <- ltsreg(kcal ~ . , data= train)
lts.predicted <- predict(lts, newdata = test.x)
lts_mspe <- f_mspe(test.y, lts.predicted)
lts_mspe


###Huber's Method
huber <- rlm(kcal ~ . , data= train)
summary(huber)
huber.predicted <- predict(huber, newdata = test.x)
huber_mspe <- f_mspe(test.y, huber.predicted)
huber_mspe

###LAD
library(quantreg)
lad <- rq(kcal ~ . , data= train)
summary(lad)
lad.predicted <- predict(lad, newdata = test.x)
lad_mspe <- f_mspe(test.y, lad.predicted)
lad_mspe



####Ridge regression
library(glmnet)
ridge <- lm.ridge(train.x, train.y, )

ridge$lambda.min
coef(ridge, s = "lambda.min")
test.x<- as.matrix(test.x)
ridge_pred <- predict(ridge_fit, test.x)
MSE_ridge <- mean((ridge_pred-test.y)^2)





lm1<-lm(kcal~., data=lm_train)
summary(lm)
##regrouping region 
lm_train <-train
lm_train$region2= lm_train$region
levels(lm_train$region2)<-c("11" ,"21" ,"23", "31_55" ,"32", "37" ,"41" ,"42" ,"43" ,"45" ,"52", "31_55")
lm_test <- test
lm_test$region2= lm_test$region
levels(lm_test$region2)<-c("11" ,"21" ,"23", "31_55" ,"32", "37" ,"41" ,"42" ,"43" ,"45" ,"52", "31_55")
lm_test.x<- lm_test[,-1]
#lm with regrouped region and without not significant variables in lm1
lm_train2<-subset(lm_train,select = -c(toilet,car,bw_tv,refrigerator,sewing_machine,electric_cooking_pot,pressure_cooker,region))#(toilet,car,bw_tv,refrigerator,sewing,ele_cook,pressure_cook,region)
lm2<-lm(kcal~., data=lm_train2)
summary(lm2)
anova(ols, lm2)
lm2.predicted <- predict(lm2, newdata = lm_test.x)
lm2_mspe <- f_mspe(test.y, lm2.predicted)
lm2_mspe

###impute hh_size and dwelling_size by median
##houseind$hh_size[is.na(houseind$hh_size)] <- 4
##houseind$dwelling_size[is.na(houseind$dwelling_size)] <- 90

###new variable nutrition as sum of protein fat carb and kcal
##houseind[, 3:6] <- lapply(houseind[, 3:6], as.numeric)
##houseind$nutrition <- apply(houseind[, 3:6], 1, mean)


