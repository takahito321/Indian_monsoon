tab<-read.table("dTT_latest.txt") # read dTT from dTT_latest.txt
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
ton<-tm[seq(1,length(tm),by=2)]  # ton only onset days
toff<-tm[seq(2,length(tm),by=2)] # toff only withdrawal days
day_on<-day[ton]                 # onset dates in year
day_off<-day[toff]               # onset dates in year


#postscript(file="onset_date_no_MOK.eps", horizontal=TRUE, encoding="WinAnsi.enc",width=12)
mat <- matrix(c(1,2), 2, 1, byrow = TRUE) # 30 panels
layout(mat)
par(mar=c(0,0,0,0))                     # graphic parameters
par(oma=c(0,0,0,0))
par(mai = c(0.7, 0.9, 0.0, 0.0))
cx<-1.4

at<-c(1,50,100,150,200,250,300,350)
lab<-at

plot(day[year==2019],dTT[year==2019],xlim=c(0,365),ylim=c(-6,3.5),type="l",ylab=expression(paste(Delta,"TT (",degree,"C)")),xlab="",bty = "n",xaxt="n",xaxs = "i",cex.lab=cx,cex.axis=cx,lwd=2)
abline(v=t1[yp-1]+151,col="gray") # JUN 1 in 2018, JAN1+(30+28+31+30+31+1)=JAN1+151=152
for(i in 1948:2019) lines(day[year==i],dTT[year==i],col="gray")
abline(h=0)
lines(day[year==2019],dTT[year==2019],col=1,lwd=2)
abline(v=day_on[72],col=2,lty=2,lwd=2)
abline(v=day_off[72],col=4,lty=2,lwd=2)
axis(side=1, at=at,labels=lab,cex.axis=cx)
mtext("Day of year", side=1, line=2.8, cex=cx)
mtext("(a)", side = 3, adj = 0.01, line = -1.0, cex=cx)

years<-1948:2020
plot(years,day_on,type="b",col=1,lwd=2,cex=cx,xlim=c(1948,2020),ylim=c(133,167),xlab="",ylab="Onset date",bty = "n",cex.lab=cx,cex.axis=cx) 
mtext("(b)", side = 3, adj = 0.01, line = -1.0, cex=cx)
mtext("Year",side=1,line=2.4,cex=1.5)
grid()
#dev.off()

# write onset date in the file named 'onset_date.txt'
dat<- data.frame(years, day_on)
write.table(dat, file = "onset_date.txt", col.names=F, row.names=F)
