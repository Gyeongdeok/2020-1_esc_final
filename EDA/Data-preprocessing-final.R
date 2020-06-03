setwd("C:/Users/Duck/Desktop/2020-1학기/ESC")

bankrupt=read.csv("bankrupt.csv",na.strings = "?")

# Investigating NA structure
head(bankrupt)
library(mice)
na.count.c=apply(bankrupt,2,function(x) sum(is.na(x)))
na.count.r=apply(bankrupt,1,function(x) sum(is.na(x)))
sort(na.count.c) # #of NA by columns
sort(na.count.r) # #of NA by rows

library(VIM)
require(VIM)
aggr(bankrupt,prop=F,numbers=T)

x=as.data.frame(abs(is.na(bankrupt)))
y=apply(x,2,function(x) sum(x)>0)
round(cor(x[y]),2)

## NA
#sales=0(9번변수, 14개), short-term liabilities=0(51번변수, 28개)인 companies제거(해당 두 변수가 분모로 들어가는게 많아서).
library(dplyr)
sum(bankrupt$Attr9==0)
sum(bankrupt$Attr51==0)

bankrupt <- bankrupt %>%
  filter(Attr9!=0 | Attr51!=0) %>% # 0인것 제거 
  select(-Attr37) #NA 3100개라서 제거

#inverntory 0을 min으로 대체. inventory가 분모로 들어가는 45번 변수와 60번 변수는 재계산하여 NA imputation.
inventory <- (bankrupt$Attr20*bankrupt$Attr9*exp(bankrupt$Attr29))/365
inventory[inventory==0]<-min(inventory[inventory>0],na.rm=T)
netprofit <- bankrupt$Attr1*exp(bankrupt$Attr29)
sales <- bankrupt$Attr9 * exp(bankrupt$Attr29)
bankrupt <- bankrupt %>%  
  transform(Attr45=netprofit/inventory, Attr60=sales/inventory) 

#Current asset + Fixed asset = Total asset 식을 사용하여 asset의 부족한 변수 계산으로 보완.
#계산으로 보완한식을 64, 54, 53, 28번 변수에 대입하여 NA imputation. 
total_asset <- exp(bankrupt$Attr29)
short_liable <- (1/bankrupt$Attr63) * sales
current_asset <- bankrupt$Attr4 * short_liable
fixed_asset <- (1/bankrupt$Attr64) * sales

current_asset[is.na(current_asset)] <- total_asset[is.na(current_asset)]-fixed_asset[is.na(current_asset)]
fixed_asset[is.na(fixed_asset)] <- total_asset[is.na(fixed_asset)]-current_asset[is.na(fixed_asset)]
constant_capital <- (bankrupt$Attr38 * total_asset)
equity <- (bankrupt$Attr10 * total_asset)

bankrupt <- bankrupt %>%
  transform(Attr64=sales/fixed_asset, Attr54=constant_capital/fixed_asset,
            Attr53=equity/fixed_asset, Attr28 = Attr55/fixed_asset)

# 그 후 나머지 변수들은 규형이 형이 처리해 주셨습니다.(liabilities 관련 식)
#Short liability + Long liability = Total liability
# 4번 12번 17번 NA imputation

totallib=bankrupt$Attr2*exp(bankrupt$Attr29)
longlib=bankrupt$Attr59*bankrupt$Attr10*exp(bankrupt$Attr29)
shortlib=bankrupt$Attr9*exp(bankrupt$Attr29)/bankrupt$Attr63
short_na=which(is.na(shortlib))
shortlib[short_na]=totallib[short_na]-longlib[short_na]
grossprofit=bankrupt$Attr18*exp(bankrupt$Attr29)
costproduct=(1-bankrupt$Attr56)*bankrupt$Attr9*exp(bankrupt$Attr29)

bankrupt <- bankrupt %>%
  transform(Attr4=current_asset/shortlib, Attr12=grossprofit/shortlib,
            Attr17=total_asset/totallib)

# 나머지 변수 NA imputation
netprofit=bankrupt$Attr1*exp(bankrupt$Attr29)
profitonsales=bankrupt$Attr35*exp(bankrupt$Attr29)
profitoper=bankrupt$Attr22*exp(bankrupt$Attr29)
equity=bankrupt$Attr10*exp(bankrupt$Attr29)
constant=bankrupt$Attr38*exp(bankrupt$Attr29)

bankrupt <- bankrupt %>%
  transform(Attr19 = (Attr18*Attr29)/(Attr9*exp(Attr29)),
            Attr23 = netprofit / sales,
            Attr28 = Attr55 / fixed_asset,
            Attr39 = profitonsales/sales,
            Attr42 = profitoper/sales,
            Attr44 = ((Attr9*exp(Attr29)/Attr64)*365)/sales,
            Attr49 = (Attr7*exp(Attr29))/sales,
            Attr50 = current_asset/totallib,
            Attr52 = (shortlib*365)/costproduct,
            Attr53 = equity/fixed_asset,
            Attr54 = constant/fixed_asset,
            Attr56 = (sales-costproduct)/sales,
            Attr62 = (shortlib*365)/sales,
            Attr63 = sales/shortlib)

inf = which(bankrupt$Attr4==Inf | bankrupt$Attr63==Inf| bankrupt$Attr62==Inf
            |bankrupt$Attr12==Inf|bankrupt$Attr52==Inf) #88개행 inf 제거
bankrupt <- bankrupt[-inf,]

bankrupt <- bankrupt[-which(is.na(bankrupt$Attr57)),]

# 21번 변수 encoding 
bankrupt <- bankrupt %>%
  transform(Attr21=ifelse(is.na(Attr21),1,0)) 

na.count.c=apply(bankrupt,2,function(x) sum(is.na(x)))
na.count.r=apply(bankrupt,1,function(x) sum(is.na(x)))
sort(na.count.c) # #of NA by columns
sort(na.count.r) # #of NA by rows
summary(data$Attr27)


write.csv(bankrupt,'data.csv')

#규형이형이 나머지 부분 NA 처리 해주셨습니다!

# score comparison(개인적으로 스코어 계산을 위해 사용한 식입니다.)
library(rpart)
score <- function(data){
  set.seed(2009)
  nobs = nrow(data)
  i = sample(1:nobs, round(nobs*0.5))
  train = data[i,]; test = data[-i,]
  tree <- rpart(class~., data=train, method="class",xval=0, cp=0.001, minsplit=10)
  pred = predict(tree, newdata=test, type='prob')[,2]
  print(F1_Score(y_pred=pred, y_true=test$class, positive='1'))
}


# Outlier detection (Uni/Multivariate outlier)
# NA 결측 처리가 완료된 데이터 이용
data=read.csv("final_KNN.csv")

na.count.c=apply(data,2,function(x) sum(is.na(x)))
na.count.r=apply(data,1,function(x) sum(is.na(x)))
sort(na.count.c) # #of NA by columns

'''(univariate outlier calculation)
upper <- function(data) fivenum(data)[4]
lower <- function(data) fivenum(data)[2]
iqr <- function(data) fivenum(data)[4]-fivenum(data)[2]

outlier.r <- apply(data,1,function(x) sum((x>(upper(x)+(iqr(x)*10)))|(x<(lower(x)-(iqr(x)*10)))))
outlier.c <- apply(data,2,function(x) sum((x>(upper(x)+(iqr(x)*10)))|(x<(lower(x)-(iqr(x)*10)))))
'''

library(MLmetrics)
library(DMwR)
outlier.lof <- lofactor(data, k=2)
outlier.sort <- sort(outlier.lof,decreasing=T)[1:10]
outlier.row <- which(outlier.lof>10) 

plot(density(outlier.lof))
sum(outlier.lof>1)

'''
lof는 density가 상이한 클러스트들에서
각 클러스트와의 거리가 떨어진 점을 발견해줍니다.
Multivariate outlier detection에 적합합니다.
'''

library(solitude)
iforest <- isolationForest$new()
iforest$mtry <- 1
iforest$fit(data)

print(iforest$scores)
data$pred <- iforest$predict(data)
which((data$pred$anomaly_score)>0.45)

score(data[-outlier.row,-ncol(data)])

data <- data[-outlier.row,]
write.csv(data,'final_data.csv')
