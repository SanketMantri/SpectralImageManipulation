library(R.matlab)
library(ridge)
library(MASS)
library(caret)
library(caTools)
library(grid)
library(igraph)
A <- readMat('paviauni.mat')
clrs<-A$X
gt<-A$groundtruth
codli<-NULL
indices<-NULL
plot<-NULL
for (i in 0:9)
{
  codli[[paste(i)]]<-which(gt==1,arr.ind = TRUE)
}
names(codli)
par(mfrow=c(2,5))
for(i in 1:10)
{
  temp<-codli[[i]][sample(1:nrow(codli[[i]]),size=(ifelse(nrow(codli[[i]])<100,nrow(codli[[i]]),100)),replace=F),]
  indices[[paste(i)]]<-A$X[temp[,1],temp[,2],]<-temp
  plot[[i]]<-clrs[(indices[[i]][,1]),(indices[[i]][1,2]),]
  matplot(t(plot[[i]]), col = i,type="l", lwd = 1, main = i)
}


redchannel<-A$X[,,57]
greenchannel<-A$X[,,27]
bluechannel<-A$X[,,17]
img<-A$X[,,1:3]

r <- (redchannel-min(redchannel))/abs(diff(range(redchannel)))
g <- (greenchannel-min(c(greenchannel)))/abs(diff(range(greenchannel)))
b <- (bluechannel-min(c(bluechannel)))/abs(diff(range(bluechannel)))

Arrayforimage<-array(c(r,g,b),dim(img))
grid.raster(Arrayforimage)