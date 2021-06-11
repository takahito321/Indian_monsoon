library(Hmisc)

tab<-read.table("dTT_latest.txt") # read dTT from Pradhan et al. 2017
dTT<-tab[,1]
year<-tab[,2]
day<-tab[,3]
t1<-which(day==1)
m<-length(t1)               # total 73 years from 1948 to 2020

# identify onset dates (and "withdrawal")
n<-length(dTT)  # total number of days
t<-1:n          # days from 1948 January 1st
cnd<-0          # cnd=1 for monsoon and 0 for non-monsoon (cnd=0 on Jan 1st)
tm<-numeric(0)  # onset and withdrawal days from 1948 January 1st
for(i in 1:n){
  if(cnd==0){
    if(dTT[i]>0 && day[i]<200){
      tm<-c(tm,i)
      cnd<-1
    }
  } else{
    if(dTT[i]<=0 && day[i]>=200){
      tm<-c(tm,i)
      cnd<-0
    }
  }
}
ton<-tm[seq(1,length(tm),by=2)] # ton only onset days
day_on<-day[ton]                # onset dates in year

# data
x<-read.table("69.dat")
x<-as.matrix(x)
xx<-x[,-74]
y<-matrix(day_on,nrow(xx),73,byrow=TRUE)
RMSE<-x[,74]  # RMSE of proximity

# top-100  ensemble average
x2<-xx[RMSE<=sort(RMSE)[100],]
POD2<-colMeans(x2)

# plot #########################################################################
#postscript(file="Fig3.eps", horizontal=TRUE, encoding="WinAnsi.enc",width=12,height=5)
mat <- matrix(1:1, 2, 1, byrow = TRUE) # 30 panels
layout(mat)
par(mar=c(0,0,0,0))                     # graphic parameters
par(oma=c(1.5,0.6,1,1.5))
par(mai = c(0.4, 0.9, 0, 0))
cx<-1.4
pc<-5

at<-1981:2020
lab<-c(rep("",3),1985,rep("",4),1990,rep("",4),1995,rep("",4),2000,rep("",4),2005,rep("",4),2010,rep("",4),2015,rep("",4),2020)

# Case 1982-2020 (39 years)
yu<-40
years<-(2020-yu+1):2020
day_on_used<-day_on[(m-yu+1):m]
POD2_used<-POD2[(length(POD2)-yu+1):length(POD2)]
plot(years,day_on_used,type="b",xlim=c(1980,2020.5),ylim=c(132,170),lwd=2,xlab="Year",ylab="Onset Date (OD)",bty = "n",cex=cx,cex.lab=cx,cex.axis=cx,pch=pc,xaxs="i") # actual onset day
polygon(c(years,rev(years)),c(day_on_used-7,rev(day_on_used)+7),border = NA, col="gray90")
grid(lwd=1.5)
lines(years,day_on_used,type="b",lwd=2,cex=cx,pch=pc) # actual onset day
lines(years,POD2_used,type="b",col=6,lwd=2,cex=cx) # predicton
axis(side=1, at=at, labels=lab, cex.axis=1)
mtext("Year",side=1,line=2.6,cex=1.5)
legend(2010.2,139.5,c("Actual OD","Predicted OD"),col=c(1,6),pch=c(5,1),lwd=1.5,cex=cx)
minor.tick(nx=10, ny=10, tick.ratio=0.4)  
#dev.off()
