setwd("d:/Rdata/Rdata")
getwd()
par(family = "AppleGothic")

data = read.csv("sales_data_new.csv")
head(data)

data[,3:10]

## κ±΄κ°?λ£μ ??? ?€μ€? ?κ· λΆμ
helth_beverage=data[1:60,]
helth_beverage
helth_beverage_feature = helth_beverage[4:10]
helth_beverage_feature
cov(helth_beverage_feature)
cor(helth_beverage_feature)

model = lm(QTY~.,helth_beverage_feature)
summary(model)

## Forward selection
model = lm(QTY~SALEDAY,helth_beverage_feature)
summary(model)
model = lm(QTY~SALEDAY+PRICE,helth_beverage_feature)
summary(model)
model = lm(QTY~SALEDAY+PRICE+ITEM_CNT,helth_beverage_feature)
summary(model)
model = lm(QTY~SALEDAY+PRICE+ITEM_CNT+MAXTEMP,helth_beverage_feature)
summary(model)
model = lm(QTY~SALEDAY+PRICE+ITEM_CNT+MAXTEMP+RAIN_DAY,helth_beverage_feature)
summary(model)
model = lm(QTY~SALEDAY+PRICE+ITEM_CNT+MAXTEMP+RAIN_DAY+HOLIDAY,helth_beverage_feature)
summary(model)

## All Subsets Regression
install.packages("leaps")
library(leaps)
leaps=regsubsets(QTY~.,data=helth_beverage_feature,nbest=5)
summary(leaps)
plot(leaps)
#best
model = lm(QTY~SALEDAY+PRICE+RAIN_DAY,helth_beverage_feature)
summary(model)

helth_beverage_feature_train = helth_beverage_feature[1:42,]
helth_beverage_feature_test = helth_beverage_feature[42:60,]
dim(helth_beverage_feature_train)
dim(helth_beverage_feature_test)
## λͺ¨λ  featureλ₯? ?¬?¨? ?€μ€νκ·
lm.fit = lm(QTY~., data = helth_beverage_feature)
summary(lm.fit)

## forward?? backward λ°©μ? ? λΆ ?¨? μ’μ? κ²°κ³Όλ§? κ°? Έ?¨?€.(both)
lm.fit2 = step(lm.fit,method="both")
summary(lm.fit2)
lm.yhat2=predict(lm.fit2,newdata=helth_beverage_feature)
kk=mean((lm.yhat2-helth_beverage_feature$QTY)^2)
sqrt(kk)
plot(lm.yhat2,Boston_test$medv)
abline(a=0,b=1,col=2)

## ?μ°¨λΆ?


## κ³Όμ¦?λ£μ ??? ?€μ€? ?κ· λΆμ
juice_beverage=data[61:120,]
juice_beverage
juice_beverage_feature = juice_beverage[4:10]
juice_beverage_feature
cov(juice_beverage_feature)
cor(juice_beverage_feature)

lm.fit = lm(QTY~., data = juice_beverage_feature)
summary(lm.fit)

## forward?? backward λ°©μ? ? λΆ ?¨? μ’μ? κ²°κ³Όλ§? κ°? Έ?¨?€.(both)
lm.fit2 = step(lm.fit,method="both")
summary(lm.fit2)
lm.yhat2=predict(lm.fit2,newdata=Boston_test)
kk=mean((lm.yhat2-Boston_test$medv)^2)
sqrt(kk)
plot(lm.yhat2,Boston_test$medv)
abline(a=0,b=1,col=2)


## μ°? ?λ£μ ??? ?€μ€? ?κ· λΆμ
tea_beverage=data[120:180,]
tea_beverage
tea_beverage_feature = tea_beverage[4:10]
tea_beverage_feature
cov(tea_beverage_feature)
cor(tea_beverage_feature)


data_feature = data[,2:3]
View(data)
cov(data)     # κ³΅λΆ?° ?¨?
cor(attitude)     # ?κ΄κ³μ
