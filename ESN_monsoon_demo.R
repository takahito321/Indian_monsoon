library(igraph)

tab<-read.table("dTT_latest.txt") # read dTT 
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
for(i in yp){  # i=1 may be noncomputable
  te[i]<-ta[i]+(tb[i]-ta[i])/(s[tb[i]]-s[ta[i]])*(1-s[ta[i]])
}
day_pred<-day[as.integer(te)] 

#postscript(file="esn_monsoon_demo.eps", horizontal=TRUE, encoding="WinAnsi.enc")
mat <- matrix(1:6, 3, 2, byrow = FALSE) # 30 panels
layout(mat)
par(mar=c(0,0,0,0))                     # graphic parameters
par(oma=c(2,0.2,0.4,0))
par(mai = c(0.3, 0.7, 0.2, 0.1))
cx<-1.9
cx2<-1.2

ys<-5
xl<-c(t1[ys-2]+190,t1[ys]+190)
lab<-c("1-1-1949","1-6-1949","1-1-1951","1-6-1951","1-1-1952","1-6-1952","1-1-1953","1-6-1953")
at<-c(t1[ys-2],t1[ys-2]+151.25,t1[ys-1],t1[ys-1]+151.25,t1[ys],t1[ys]+151.25,t1[ys+1],t1[ys+1]+151.25)

plot(t,dTT,type="l",ylab=expression(paste(Delta,"TT (",degree,"C)")),xlab="",xlim=xl,bty = "n",xaxt="n",xaxs = "i",cex.lab=cx,cex.axis=cx,lwd=2)
for(i in 1:length(ton)) abline(v=ton[i],col=2,lty=2,lwd=2)
abline(h=0,lwd=0.8)
axis(side=1, at=at,labels=lab,cex.axis=1.7)
mtext("(a)", side = 3, adj = 0.01, line = -1.0, cex=cx2)

plot(t,r[1,],type="l",ylab=expression(paste(r[i])),xlab="",xlim=xl,ylim=c(min(r),max(r)),bty = "n",xaxt="n",xaxs = "i",cex.lab=(cx+0.3),cex.axis=cx,col=1)
for(i in 2:25) lines(t,r[i,],col=i)
axis(side=1, at=at,labels=lab,cex.axis=cx)
mtext("(c)", side = 3, adj = 0.01, line = -1.0, cex=cx2)

plot(t,xx,col=1,lty=2,type="l",ylab="Proximity",xlab="",xlim=xl,ylim=c(0,1.15),xaxt="n",bty = "n",xaxt="n",xaxs = "i",cex.lab=cx,cex.axis=cx,lwd=2)
for(i in 1:length(ton)) abline(v=ton[i],col=2,lty=2,lwd=2)
lines(t,s,lwd=2,col="darkorange")
abline(h=1,lty=2,col="gray40")
axis(side=1, at=at,labels=lab,cex.axis=cx)
mtext("Day (dd-mm-yyyy)", side=1, line=2.8, cex=1.2)
mtext("(e) Training", side = 3, adj = 0.01, line = -1.0, cex=cx2)

ys<-72
xl<-c(t1[ys-2]+190,t1[ys]+190)
lab<-c("1-1-2017","1-6-2017","1-1-2018","1-6-2018","1-1-2019","1-6-2019","1-1-2020","1-6-2020")
at<-c(t1[ys-2],t1[ys-2]+151.25,t1[ys-1],t1[ys-1]+151.25,t1[ys],t1[ys]+151.25,t1[ys+1],t1[ys+1]+151.25)

plot(t,dTT,type="l",ylab=expression(paste(Delta,"TT (",degree,"C)")),xlab="",xlim=xl,bty = "n",xaxt="n",xaxs = "i",cex.lab=cx,cex.axis=cx,lwd=2)
for(i in 1:length(ton)) abline(v=ton[i],col=2,lty=2,lwd=2)
abline(h=0,lwd=0.8)
axis(side=1, at=at,labels=lab,cex.axis=cx)
mtext("(b)", side = 3, adj = 0.01, line = -1.0, cex=cx2)

plot(t,r[1,],type="l",ylab=expression(paste(r[i])),xlab="",xlim=xl,ylim=c(min(r),max(r)),bty = "n",xaxt="n",xaxs = "i",cex.lab=(cx+0.3),cex.axis=cx,col=1)
for(i in 2:25) lines(t,r[i,],col=i)
axis(side=1, at=at,labels=lab,cex.axis=cx)
mtext("(d)", side = 3, adj = 0.01, line = -1.0, cex=cx2)

plot(t,s,col="darkorange",type="l",ylab="Proximity",xlab="",xlim=xl,ylim=c(0,1.15),xaxt="n",bty = "n",xaxt="n",xaxs = "i",cex.lab=cx,cex.axis=cx,lwd=2)
for(i in 1:length(ton)) abline(v=ton[i],col=2,lty=2,lwd=2)
for(i in 2:length(t1)){
  cl<-"dodgerblue"
  points(t1[i]-60,s[t1[i]-60],col=cl,cex=3,pch=3,lwd=3)
  points(t1[i]+70,s[t1[i]+69],col=cl,cex=3,pch=3,lwd=3)
  segments(t1[i]-60,s[t1[i]-60],t1[i]+day_pred[i],1,col=cl,lwd=3,lty=2)
  arrows(t1[i]+day_pred[i]-1,1.15,t1[i]+day_pred[i]-1,1,length=0.15,col=cl,lwd=3)
}
abline(h=1,lty=2,col="gray40")
axis(side=1, at=at,labels=lab,cex.axis=cx)
mtext("Day (dd-mm-yyyy)", side=1, line=2.8, cex=1.2)
mtext("(f) Prediction", side = 3, adj = 0.01, line = -1.0, cex=cx2)
#dev.off()
