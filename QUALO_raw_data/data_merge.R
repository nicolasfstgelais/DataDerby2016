rm(list=ls(all=TRUE))

library(plyr)
library(lubridate)
library(rms)
library(RCurl)


isd_stations_search(lat = NULL, lon = NULL, radius = NULL, bbox = NULL)

setwd("C://Users//Nicolas//Desktop//QUAL")
geoMean <- function(x){
  (prod(x))^(1/length(x))
}

precip<-read.csv(text=getURL("https://raw.githubusercontent.com/nicolasfstgelais/DataDerby2016/master/QUALO_raw_data/trudeau_precip.csv"), header=T)
weather<-read.csv(text=getURL("https://raw.githubusercontent.com/nicolasfstgelais/DataDerby2016/master/QUALO_raw_data/trudeau_weather_gen.csv"), header=T)


#import urls for QUALO file and merge them in QUALO
urls<-read.csv(text=getURL("https://raw.githubusercontent.com/nicolasfstgelais/DataDerby2016/master/QUALO_raw_data/urls"), header=T)
for(i in 1:nrow(urls)){
  if(i==1)QUALO<-read.csv(text=getURL(as.character(urls[i,])), header=TRUE)[,1:7]
  if(i!=1)QUALO=rbind(QUALO,read.csv(text=getURL(as.character(urls[i,])), header = TRUE)[,1:7])
}

#merge together all QUALO files
# select variables that were measured each year
cnames=c("Point d'echantillonnage","Date","Temperature(oc)","Conductivite(us/cm2)","pH","Signe","Coliformes (colonies/100ml)")
colnames(QUALO)=cnames
#change , for . for temperature
QUALO$`Temperature(oc)`=gsub(",",".",QUALO$`Temperature(oc)`)
#all variables as numeric
QUALO$`Temperature(oc)`=as.numeric(QUALO$`Temperature(oc)`)
QUALO$`Conductivite(us/cm2)`=as.numeric(QUALO$`Conductivite(us/cm2)`)
QUALO$pH=as.numeric(QUALO$pH)
QUALO$`Coliformes (colonies/100ml)`=as.numeric(QUALO$`Coliformes (colonies/100ml)`)

setwd("C://Users//Nicolas//Desktop//QUAL")
WQ$Signe=NULL
#write.csv(WQ,"QUALO_2003-2014.csv")
WQ2=read.csv("QUALO_2003-2014.csv")
which(is.na(WQ2$Temperature.oc.))

## normalize date format in the weather and precip files
for(i in 1:nrow(precip)){
  t=paste(substring(precip[i,"DATE"],c(1,5,7),c(4,6,8)),sep="-")
   precip[i,"DATE"]=paste(t[1],t[2],t[3],sep="-")}

do.call(strsplit(rbind,as.character(weather[,"DATE"]),"-"))
precip$m=as.data.frame(do.call(rbind,strsplit(as.character(precip[,"DATE"]),"-")))[2]
precip$y=as.data.frame(do.call(rbind,strsplit(as.character(precip[,"DATE"]),"-")))[1]

i=2
for(i in 1:nrow(weather)){
  weather[i,"date"]=strsplit(as.character(weather[i,"time"])," ")[[1]][1]
   }

  #weather$date=paste(weather$year,weather$month,weather$day,sep="-")

QUALO[,"Date"]=as.character(QUALO[,"Date"])
i=9973
for(i in 1:nrow(QUALO)){
  dateT=as.character(QUALO[i,"Date"])
  if(grepl("/",dateT))QUALO[i,"Date"]=gsub("/","-",dateT)
  if(grepl(":",dateT))QUALO[i,"Date"]=strsplit(dateT," ")[[1]][1]
}

dates=as.character(QUALO$Date)
  weather[as.character(weather$date)%in%dates,]


weatherSel=weather[,c("year","month","day","wd","ws","temp","dew_point","atmos_pres","rh")]

## summarize weather for every date
weatherSel$p0=NA;
weatherSel$p1=NA;
weatherSel$p2=NA;
weatherSel$p3=NA;
weatherSel$p4=NA;
weatherSel$p5=NA;
weatherSel$t1=NA;
weatherSel$t2=NA;
weatherSel$t3=NA;
weatherSel$t4=NA;
weatherSel$t5=NA;
WQ$pm2=NA;
WQ$pm5=NA
WQ$m=NA;WQ$y=NA
  
for(i in 1:nrow(WQ))
{
  ds=which(wtr$DATE%in%WQ[i,"Date"])
  ds2=which(wtr2$date%in%WQ[i,"Date"])
  if(length(ds)!=0){
  WQ[i,"p0"]=wtr[ds,"PRCP"]
  WQ[i,"p1"]=wtr[(ds-1),"PRCP"]
  WQ[i,"p2"]=wtr[(ds-2),"PRCP"]
  WQ[i,"p3"]=wtr[(ds-3),"PRCP"]
  WQ[i,"p4"]=wtr[(ds-4),"PRCP"]
  WQ[i,"p5"]=wtr[(ds-5),"PRCP"]
  WQ[i,"m"]=wtr[ds,"m"]
  WQ[i,"y"]=wtr[ds,"y"]
  WQ[i,"t0"]=mean(wtr[(ds),"TMAX"])/10

  WQ[i,"t0"]=mean(wtr[(ds-2):ds,"TMAX"])/10
  WQ[i,"t5"]=mean(wtr[(ds-5):ds,"TMAX"])/10
  }}
 modB=NA
 modL=NA
 c=0
 i="FSL-360"
s=as.matrix(unique(as.character(WQ$Point.d.√.chantillonnage)))
 for(i in unique(as.character(WQ$Point.d.√.chantillonnage))){
  #print(i)
  sub=WQ[WQ$Point.d.√.chantillonnage%in%i,]
  sub$lim=NA;
  sub$lim[sub$Coliformes.f√.caux..colonies.100mL.<201]=0
  sub$lim[sub$Coliformes.f√.caux..colonies.100mL.>201]=1
  m=rpart::rpart(data.matrix(sub$lim)~.,as.data.frame(sub[,c("p0","p1","p2","p5","pm2","pm5","t2","t5","m","y")]))
  print(m)
  plot(m)
  text(m)
  rpart::plotcp(m)
mp=rpart::prune(m,0.062)
mp
rpart::rsq.rpart(mp)
mp$where
mean(sub$lim[mp$where==2])
mean(sub$lim[mp$where==3],na.rm = T)
  sub=sub[!is.na(sub$lim),]
  if(sum(sub$lim,na.rm = T)>1){
  mod1=lrm(sub$lim~sub$p2)
  mod2=lm(sub$lim~sub$p2+sub$p0+sub$t2)
  modB[c]=mod1$stats["R2"];modL[c]=summary(mod2)$r.squared
  }
  c=c+1
}
 options(scipen=999)
modL
j
a=summary(mod2)
temp
models

a
plot(temp$lim~temp$p2)

unique(WQ2014$Point.d.√É.chantillonnage)
sub=WQ2014[WQ2014$Point.d.√É.chantillonnage%in%"BLAP-4.5",]
sub=sub[order(sub$Date),]
sites=grepl("FSL",WQ$Point.d.√É.chantillonnage)
sub=WQ[sites,];sub=sub[order(sub$Date),]
plot(sub$Coliformes.f√É.caux..colonies.100mL.~)
