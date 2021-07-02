library(igraph)
library(Hmisc)

tab<-read.table("dTT_latest.txt") # read dTT from Pradhan et al. 2017
dTT<-tab[,1]
year<-tab[,2]
day<-tab[,3]
t1<-which(day==1)
m<-length(t1)             

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
toff<-tm[seq(2,length(tm),by=2)] # toff only withdrawal days
day_on<-day[ton]                # onset dates in year

# proximity function
proximity<-numeric(n)
ip<-1
for(i in 2:length(tm)){
  ix<-(which(t==tm[i-1])+1):which(t==tm[i])
  if(i%%2==ip){
    proximity[ix]<-1-(tm[i]-t[ix])/(tm[i]-tm[i-1])
  } else{
    proximity[ix]<-(tm[i]-t[ix])/(tm[i]-tm[i-1])
  }
}

season<-cos(2*pi*(t-152.25)/365.25) # seasonal cycle


# JFM 3
yy<-1948:2020
dTT.each<-rep(NA,73)
for(i in 1:73){
  dTT.each[i]<-mean(dTT[t1[i]:(t1[i]+69)])     
}
cor(dTT.each,day_on,use="complete.obs")

# Reservoir ###############################################################################################
# prepare time series #####################################################################################
###########################################################################################################
# scale
dTT_x<-(dTT-mean(dTT[year!=2020]))/sd(dTT[year!=2020])
season_x<-(season-mean(season[year!=2020]))/sd(season[year!=2020])
xx<-proximity                 # target
u<-rbind(dTT_x,season_x)      # input
M<-nrow(u)                    # the number of inputs for learning

# Making reservoir ########################################################################################
N<-100              # the number of reservoir nodes
D<-20/400*N         # average degree of a reservoir node
rho<-1.0            # spectral radius
b<-1e-10            # ridge regression parameter
sig<-0.04
xi<-0.01

# Onset day predection for 30 years ###########################################################################
te<-ton                # te = array for expected onset day
sl<-ton
cn<-ton
ta<-t1-60              # left edge of interval used for extrapolation
tb<-t1+69              # right edge of interval used for extrapolation
l<-40
yp<-(73-l+1):73
lean<-c(ton[3]:ton[min(yp-1)])

set.seed(15)
g<-erdos.renyi.game(N,D/N,directed=T) 
A<-as.matrix(get.adjacency(g))
for(i in 1:nrow(A)) A[i,]<-A[i,]*runif(ncol(A),-1,1)
sc<-max(abs(eigen(A)$values))
A<-rho*A/sc         # weighted adjacency matrix of the reservoir
Win<-matrix(NA,N,M) # Win
for(i in 1:M) Win[,i]<-runif(N,-1,1)
  
# Learning ############################################################################
r<-matrix(0,N,n)      # reservoir state
for(i in 2:n) r[,i]<-tanh(A%*%r[,i-1]+sig*Win%*%u[,i-1]+xi)
s.m<-mean(xx[lean])
dS<-xx[lean]-s.m
r.m<-rowMeans(r[,lean])
dR<-r[,lean]-matrix(r.m,N,length(lean))
Wout<-dS%*%t(dR)%*%solve(dR%*%t(dR)+b*diag(N))
cs<--(Wout%*%r.m-s.m)
s<-as.vector(Wout%*%r)+rep(cs,n) 


# prediction by extrapolation ####################################################
for(i in yp){  
  te[i]<-ta[i]+(tb[i]-ta[i])/(s[tb[i]]-s[ta[i]])*(1-s[ta[i]]) # onset date counted from Jan 1 1948 
  sl[i]<-(s[tb[i]]-s[ta[i]])/(tb[i]-ta[i]) # slope of proximity function
  cn[i]<-s[ta[i]]                          # proximity at Nov 2
}
day_pred<-day[as.integer(te)] 

yy<-1948:2020

# calculating winter average temperature
dTT.ave<-0 
for(i in 34:72){
  dTT.ave<-dTT.ave+mean(dTT[t1[i]:(t1[i]+69)])      # good
}
dTT.ave<-dTT.ave/(72-33) # winter average

# classify years into warm winter years and cold winter years
la<-numeric(0)       # indices corresponding to warm winter years
el<-numeric(0)       # indices corresponding to cold winter years
dTT.la<-numeric(365) # average daily temperature evolution corresponding to warm winter years
dTT.el<-numeric(365) # average daily temperature evolution corresponding to cold winter years
for(i in 34:72){
  if( mean(dTT[t1[i]:(t1[i]+69)]) > dTT.ave){     
    la<-c(la,i)  # indices of warm winter years
    dTT.la<-dTT.la+dTT[t1[i]:(t1[i]+364)]
  }
  if( mean(dTT[t1[i]:(t1[i]+69)]) < dTT.ave){    
    el<-c(el,i)  # indices of cold winter years
    dTT.el<-dTT.el+dTT[t1[i]:(t1[i]+364)]
  }
}
dTT.la<-dTT.la/length(la) # average daily temperature evolution corresponding to warm winter years
dTT.el<-dTT.el/length(el) # average daily temperature evolution corresponding to cold winter years


#postscript(file="Fig5.eps", horizontal=TRUE, encoding="WinAnsi.enc",width=12)
mat <- matrix(c(1,1,2,3), 2, 2, byrow = TRUE) # 30 panels
layout(mat)
par(mar=c(0,0,0,0))                     # graphic parameters
par(oma=c(0,0,0,0))
par(mai = c(0.7, 0.9, 0.2, 0.2))
cx<-1.5

at<-c(1,50,100,150,200,250,300,350)
lab<-at

plot(1:365,dTT.la,xlim=c(0,365),ylim=c(-6,3.5),type="l",ylab=expression(paste(Delta,"TT (",degree,"C)")),xlab="",bty = "n",xaxt="n",xaxs = "i",cex.lab=cx,cex.axis=cx,lwd=2)
abline(v=t1[yp-1]+151,col="gray")
for(i in la) lines(day[year==(i+1947)],dTT[year==(i+1947)],col="hotpink1",lwd=0.1)
for(i in el) lines(day[year==(i+1947)],dTT[year==(i+1947)],col="lightblue2",lwd=0.1)
lines(1:365,dTT.la,col="red",lwd=4)
lines(1:365,dTT.el,col="blue",lwd=4)
abline(h=0)
axis(side=1, at=at,labels=lab,cex.axis=cx)
mtext("Day of year", side=1, line=2.8, cex=cx)
text(7,3,"(a)",cex=1.8)
legend(15,3.7,c(expression(paste(Delta,"TT>mean")),expression(paste(Delta,"TT<mean"))),col=c(2,4),lwd=c(3,3),cex=1.4,title="First 70 days")
arrows(1,-1,70,-1,length = 0.25, angle = 30, code = 3)
minor.tick(nx=5, ny=2, tick.ratio=0.4) 

plot(cn[34:72],sl[34:72],ylim=c(0.0037,0.0046),type="p",col="gray",cex.lab=cx,cex.axis=cx,cex.main=cx,ylab="Slope between NOV 02 and MAR 10",xlab="Proximity at Nov 02",main="Proximity function",bty = "n")
lines(cn[el],sl[el],type="p",col=4,pch=15,cex=2)
lines(cn[la],sl[la],type="p",col=2,pch=17,cex=2)
legend(0.14,0.0046,c(expression(paste(Delta,"TT>mean")),expression(paste(Delta,"TT<mean"))),col=c(2,4),pch=c(17,15),cex=1.4,title="First 70 days")
abline(h=mean(sl[la]),col=2,lty=2,lwd=2)
abline(h=mean(sl[el]),col=4,lty=2,lwd=2)
text(0.078,0.0046,"(b)",cex=1.8)

plot(yy[34:72],day_pred[34:72],ylim=c(140,170),,type="p",col="gray",cex.lab=cx,cex.axis=cx,ylab="Predicted onset date",xlab="Year",bty = "n")
lines(yy[la],day_pred[la],type="p",col=2,pch=17,cex=2)
lines(yy[el],day_pred[el],type="p",col=4,pch=15,cex=2)
abline(h=mean(day_pred[la]),col=2,lty=2,lwd=2)
abline(h=mean(day_pred[el]),col=4,lty=2,lwd=2)
legend(2005,170,c(expression(paste(Delta,"TT>mean")),expression(paste(Delta,"TT<mean"))),col=c(2,4),pch=c(17,15),cex=1.4,title="First 70 days")
minor.tick(nx=10, ny=5, tick.ratio=0.4) 
text(1982,170,"(c)",cex=1.8)
#dev.off()

c(mean(day_pred[la]),mean(day_pred[el]))
