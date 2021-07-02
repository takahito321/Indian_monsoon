# 1. set or go to a work directory
# 2. make a directory to transfter NCEP temperature data; here NCEP_Temperature
# downloading data #############################################################################################
if(FALSE){
  yy<-1948:2020
  for(i in 1:length(yy)){
      curl<-paste("ftp://ftp2.psl.noaa.gov/Datasets/ncep.reanalysis.dailyavgs/pressure/air.",yy[i],".nc",sep="")
      cdestfile<-paste("NCEP_Temperature/air.",yy[i],".nc",sep="")
      download.file(curl,cdestfile,mode="wb") # note that exsisting files are replaced by new files if mode="wb"
  }
}

# 3. Calculate the meridional temperature difference between the northern and southern boxes ###############################
library(ncdf4) # install.packages("ncdf4") if you don't have  
ncin <- nc_open("NCEP_Temperature/air.2019.nc")
lon<-ncvar_get(ncin,"lon") # all longitudes 
lat<-ncvar_get(ncin,"lat") # all latitudes
level<-ncvar_get(ncin,"level") # all vertical levels

# ----- test calculation ---------- #
ix<-which(lon>=40 & lon<=100) # select lon
iy<-which(lat>=5 & lat<=35)   # select lat
t<-ncvar_get(ncin,"time")     # time
u<-ncvar_get(ncin,"air", start=c(min(ix),min(iy),5,1), count=c(length(ix),length(iy),5,length(t))) # u[lon,lat,level,time] 
x<-apply(u,4,mean)

nc_close(ncin)

# Calculate the meridional temperature difference between the northern and southern boxes ###############################
# year 1948-2020
yy<-1948:2020
xn<-numeric(0) # north box
xs<-numeric(0) # south box
day<-numeric(0)
year<-numeric(0)
ix<-which(lon>=30 & lon<=110) # Xavier et al. 2007 # Pradhan 2017 -> ix<-which(lon>=30 & lon<=110) 
iy<-which(lat>=(-15) & lat<=35) # length(iy)=21, lat[iy[1:13]]=35-5, lat[iy[13:21]]
for(i in 1:length(yy)){
    cdestfile<-paste("NCEP_Temperature/air.",yy[i],".nc",sep="")
    ncin <- nc_open(cdestfile)
    t<-ncvar_get(ncin,"time")
    u<-ncvar_get(ncin,"air", start=c(min(ix),min(iy),5,1), count=c(length(ix),length(iy),6,length(t))) # u[lon,lat,level,time]
    xn<-c(xn,apply(u[, 1:13,-5,],4,mean)) # -5 to remove 250-hpa level, lat[iy][1:13]=35-5N
    xs<-c(xs,apply(u[,13:21,-5,],4,mean)) # -5 to remove 250-hpa level, lat[iy][13:21]=5N-15S
    day<-c(day,1:length(t))
    year<-c(year,rep(yy[i],length(t)))
    nc_close(ncin)
}

# write the temperature difference xn-xs in the file named 'dTT_latest.txt'
dat<- data.frame(xn-xs, year, day, xn, xs)
write.table(dat, file = "dTT_latest.txt", col.names=F, row.names=F)
