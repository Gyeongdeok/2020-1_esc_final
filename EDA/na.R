setwd("C://Users/R/Desktop/4학년 1학기/ESC/파이널")
data=read.csv("bankrupt.csv")
data=data %>%
  filter(Attr9!=0|Attr51!=0)
x=apply(data,2,function(x) {sum(is.na(x))})
attach(data)
totallib=Attr2*exp(Attr29)
longlib=Attr59*Attr10*exp(Attr29)
shortlib=Attr9*exp(Attr29)/Attr63
longlib[4092]=totallib[4092]-shortlib[4092]
x=which(is.na(shortlib))
shortlib[x]=totallib[x]-longlib[x]
sales=data$Attr9*exp(data$Attr29)
totalassets=exp(Attr29)
fixedassets=Attr55/Attr28
currentassets=Attr50*Attr2*exp(Attr29)
x=which(is.na(fixedassets))
fixedassets[x]=totalassets[x]-currentassets[x]
x=which(is.na(currentassets))
currentassets[x]=totalassets[x]-fixedassets[x]

x=which(is.na(data$Attr4))
data$Attr4[x]=currentassets[x]/shortlib[x]
grossprofit=Attr18*exp(Attr29)
x=which(is.na(data$Attr12))
data$Attr12[x]=grossprofit[x]/shortlib[x]
x=which(is.na(data$Attr17))
data$Attr17[x]=totalassets[x]/totallib[x]
costproduct=(1-Attr56)*Attr9*exp(Attr29)


# 19 채우는 식X18*exp(X29)	X19*X9*exp(X29)
x=which(is.na(data$Attr19))
data$Attr19[x]=((Attr18[x]*exp(Attr29[x]))/(Attr9[x]*exp(Attr29[x])))
#21 na가 있으면 신생기업일수도
netprofit=Attr1*exp(Attr29)
x=which(is.na(data$Attr23))
data$Attr23[x]=netprofit[x]/sales[x]
x=which(is.na(data$Attr28))
data$Attr28[x]=Attr55[x]/fixedassets[x]
#interest=(Attr14-Attr18)*exp(Attr29) 이상함
profitonsales=Attr35*exp(Attr29)
x=which(is.na(data$Attr39))
data$Attr39[x]=profitonsales[x]/sales[x]
profitoper=Attr22*exp(Attr29)
x=which(is.na(data$Attr42))
data$Attr42[x]=profitoper[x]/sales[x]

x=which(is.na(data$Attr44))
data$Attr44[x]=((Attr9[x]*exp(Attr29[x])/Attr64[x])*365)/sales[x]
x=which(is.na(data$Attr49))
data$Attr49[x]=(Attr7[x]*exp(Attr29[x]))/sales[x]
x=which(is.na(data$Attr50))
data$Attr50[x]=currentassets[x]/totallib[x]
x=which(is.na(data$Attr52))
data$Attr52[x]=(shortlib[x]*365)/costproduct[x]
equity=Attr10*exp(Attr29)
x=which(is.na(data$Attr53))
data$Attr53[x]=(equity[x])/fixedassets[x]
constant=Attr38*exp(Attr29)
x=which(is.na(data$Attr54))
data$Attr54[x]=(constant[x])/fixedassets[x]
x=which(is.na(data$Attr56))
data$Attr56[x]=(sales[x]-costproduct[x])/sales[x]


x=which(is.na(data$Attr62))
data$Attr62[x]=(shortlib[x]*365)/sales[x]
x=which(is.na(data$Attr63))
data$Attr63[x]=sales[x]/shortlib[x]

x=apply(data,2,function(x) {sum(is.na(x))})

data=read.csv(data,"dataset1.csv")