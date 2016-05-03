rm(list=ls(all=TRUE))
library(plyr)
library(lubridate)
library(rms)
library(RCurl)

setwd("C://Users//Nicolas//Desktop//QUAL")
geoMean <- function(x){
  (prod(x))^(1/length(x))
}

wtr=read.csv("prec_Trudeau.csv",header=T)
wtr2=read.csv("weather_Trudeau.csv",header=T)

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
write.csv(WQ,"QUALO_2003-2014.csv")
WQ2=read.csv("QUALO_2003-2014.csv")
which(is.na(WQ2$Temperature.oc.))

## normalize date format
for(i in 1:nrow(wtr)){
  t=paste(substring(wtr[i,"DATE"],c(1,5,7),c(4,6,8)),sep="-")
   wtr[i,"DATE"]=paste(t[1],t[2],t[3],sep="-")}

do.call(strsplit(rbind,as.character(wtr[,"DATE"]),"-"))
wtr$m=as.data.frame(do.call(rbind,strsplit(as.character(wtr[,"DATE"]),"-")))[2]
wtr$y=as.data.frame(do.call(rbind,strsplit(as.character(wtr[,"DATE"]),"-")))[1]


for(i in 1:nrow(wtr2)){
  wtr2[i,"date"]=strsplit(as.character(wtr2[i,"time"])," ")[[1]][1]
   }

wtr2$date=paste(wtr2$year,wtr2$month,wtr2$day,sep="-")

WQ[,"Date"]=as.character(WQ[,"Date"])
i=9973
for(i in 1:nrow(WQ)){
  dateT=as.character(WQ[i,"Date"])
  if(grepl("/",dateT))WQ[i,"Date"]=gsub("/","-",dateT)
  if(grepl(":",dateT))WQ[i,"Date"]=strsplit(dateT," ")[[1]][1]
}

## summarize weather for every date
WQ$p0=NA;WQ$p2=NA;WQ$p1=NA;WQ$p5=NA;WQ$t2=NA;WQ$t5=NA;WQ$pm2=NA;WQ$pm5=NA
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
