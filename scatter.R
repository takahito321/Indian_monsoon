x<-read.table("69.dat")

tab<-read.table("dTT_latest.txt") 
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
x<-as.matrix(x)
xx<-x[,-74]
y<-matrix(day_on,nrow(xx),73,byrow=TRUE)
RMSE1<-x[,74]  # RMSE of proximity

# prediction year
l<-40
yp<-(73-l+1):73

# each RMSE
RMSE2<-numeric(nrow(xx))
for(i in 1:nrow(xx)){
    RMSE2[i]<-sqrt(mean((y[i,yp]-xx[i,yp])^2))
}

#nl<-lm(RMSE2~RMSE1)
nl<-nls(RMSE2~a+b*RMSE1+c*RMSE1^2+d*RMSE1^3,start=list(a=1,b=1000,c=1,d=1))
cor(RMSE2,predict(nl))

png(filename = "scatter.png")
plot(RMSE1,RMSE2,type="p",xlab="RMSE between the actual and estimated proximity functions",ylab="RMSE between the actual and predicted onset dates (day)",main="100,000 ensemble forecaset",col="gray",ylim=c(4,20))
xa<-seq(0.02,0.04,by=0.001)  
lines(xa,predict(nl, newdata=data.frame(RMSE1=xa)),col=2,lty=2,lwd=2)
s<-sort(RMSE1)
abline(v=s[100],lty=3,lwd=2)
points(RMSE1[which.min(RMSE2)],RMSE2[which.min(RMSE2)],pch=3,cex=2,col=4,lwd=2)
#nt<-c(10,20,30,50,100,200,400,1000,2000,4000,10000,20000,50000,80000,95000)
#ynt<-nt
#for(i in 1:length(nt)){
# ynt[i]<-mean(RMSE2[RMSE1<=s[nt[i]]])
#}
#lines(s[nt],ynt,type="b",pch=3,col=4)
dev.off()
