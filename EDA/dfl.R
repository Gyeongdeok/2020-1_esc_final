setwd("C:/Users/USER/Desktop")
bankrupt=read.csv("bankrupt.csv",na.strings = "?")
attach(bankrupt)

dfl <- function(number){
  # factor -> numeric
  attr=bankrupt[,number]
  if(is.factor(attr)){
    attr <- as.numeric(as.character(attr))
  }
  #na제거
  attr=na.omit(attr)
  
  data <- sort(attr); preq <- c(); cumul <- c(); rate <- c()
  point <- quantile(attr,c(0,0.05,0.125,0.25,0.4,0.6,0.75,0.875,0.95,1))
  
  for(i in 1:9){
    if(i==1){
      preq[i] <- length(data[data>=point[i]&data<=point[i+1]])
      cumul[i] <- sum(preq[1:i])
    }
    else{
      preq[i] <- length(data[data>point[i]&data<=point[i+1]])
      cumul[i] <- sum(preq[1:i])
    }
  }
  
  rate[1] <- sum(class[order(attr)][1:cumul[1]])
  for(i in 1:8){
    rate[i+1] <- sum(class[order(attr)][(cumul[i]+1):(cumul[i+1])])
  }
  par(mar = c(5, 4, 4, 6) + 0.1)
  plot(preq/sum(preq),type='h',lwd=30,ylim=c(0,0.25),col="darkgrey",axes=FALSE,xlab="",ylab=""
       ,main=colnames(bankrupt[number]),lend=1)
  axis(side = 2, )
  mtext("구성비", side = 2, line = 2.5);
  box();par(new = TRUE)
  plot(rate/preq,type="l",axes=FALSE,xlab="",ylab="",col="blue",lwd=2);par(new=T)
  plot(rate/preq,type="p",axes=FALSE,xlab="",ylab="",pch=15,lwd=3,col="blue")
  axis(side = 1, at = 1:9, labels = 1:9)
  axis(side = 4, col = "blue", col.axis = "blue")
  mtext("불량률", side = 4, col = "blue", line = 2.5);#par(new=T)
  #legend("top", legend = c("구성비", "불량률"), text.col = c("darkgrey", "blue"), 
  #pch = c(16,15), col = c("darkgrey", "blue")) 
}
#saving plots automatically in the working directory.
windows(width=1400,height=800)
par(mfrow=c(3,3))
auto_dfl <- function(n){
  par(mfrow=c(3,3))
  for(i in n:(n+8)) dfl(i)
}
for(i in 1:7){
  plot_filename = paste("dfl",i,".jpg",sep="")
  jpeg(filename=plot_filename,width=1400,height=800,quality=100)
  auto_dfl(9*i-8)
  dev.off()
}
plot_filename = paste("dfl",8,".jpg",sep="")
jpeg(filename=plot_filename,width=1400,height=800,quality=100)
dfl(64)
dev.off()
