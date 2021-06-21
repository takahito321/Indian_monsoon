library(igraph)

tr<-as.integer( commandArgs(trailingOnly=TRUE)[1] )

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
ton<-tm[seq(1,length(tm),by=2)] # ton only onset days
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
N<-100                        # the number of reservoir nodes
D<-20/400*N                   # average degree of a reservoir node
rho<-1.0                      # spectral radius
b<-1e-10                      # ridge regression parameter
sig<-0.04
xi<-0.01
l<-40
yp<-(73-l+1):73
lean<-c(ton[3]:ton[min(yp-1)])
ns<-2:n                       # total running time of reservoir network (2: is fixed)
te<-ton                       # te = array for expected onset day
ta<-t1-60                     # left edge of interval used for extrapolation
tb<-t1+tr                     # right edge of interval used for extrapolation

ensemble<-100000
RMSEP<-numeric(ensemble)
RMSES<-numeric(ensemble)
x<-matrix(c(ton,NA),ensemble,74,byrow=TRUE)

for(j in 1:ensemble){
set.seed(j)
g<-erdos.renyi.game(N,D/N,directed=T) 
A<-as.matrix(get.adjacency(g))
for(i in 1:nrow(A)) A[i,]<-A[i,]*runif(ncol(A),-1,1)
sc<-max(abs(eigen(A)$values))
A<-rho*A/sc                   # weighted adjacency matrix of the reservoir
Win<-matrix(NA,N,M)           # Win
for(i in 1:M) Win[,i]<-runif(N,-1,1)
  
r<-matrix(0,N,n)              # reservoir state
for(i in ns) r[,i]<-tanh(A%*%r[,i-1]+sig*Win%*%u[,i-1]+xi)
s.m<-mean(xx[lean])
dS<-xx[lean]-s.m
r.m<-rowMeans(r[,lean])
dR<-r[,lean]-matrix(r.m,N,length(lean))
Wout<-dS%*%t(dR)%*%solve(dR%*%t(dR)+b*diag(N))
cs<--(Wout%*%r.m-s.m)
s<-as.vector(Wout%*%r)+rep(cs,n)
x[j,74]<-sqrt(mean( (s[lean]-xx[lean])^2 ) )

for(i in 4:m) te[i]<-ta[i]+(tb[i]-ta[i])/(s[tb[i]]-s[ta[i]])*(1-s[ta[i]])
x[j,1:73]<-day[abs(as.integer(te))]
}

filename<-paste(tr,".dat",sep="")
dat<- data.frame(x)
write.table(dat, file = filename, col.names=F, row.names=F)
