## @knitr panel
library(plm)
library(scales)
source("C:/Users/brand/Dropbox/github/european_indicators/macroR/facetAdjust.R")
#Europe enti?re
newdata<-data.frame()
ordvarnames<-list("caI","ulcI","crI","pdI","dI","hpI","expI","nfaI","eerI","unI","pasI")
#varname="hpI"
for (varname in ordvarnames){
  sub<-subset(procdata,variable==varname)
  sub$date<-year(sub$date)
  subpd<-subset(sub,select=-c(name,isnea,variable))
  pd<-pdata.frame(subpd,index=c("country","date"))
  plm<-plm(value~date,data=pd)
  newdata2<-data.frame(plmcoef=plm$coef,
                       plmse=sqrt(diag(plm$vcov)),
                       time=levels(plm$model$date)[-1],
                       var=varname,
                       name=unique(sub$name))
  newdata<-rbind(newdata,newdata2)
}

p<-ggplot(data=newdata,aes(x=time,y=plmcoef,color=var,group=var)) +
  geom_ribbon(aes(ymin=plmcoef-1.96*plmse,ymax=plmcoef+1.96*plmse),data=newdata,stat="identity",alpha=0.05,linetype=2,show_guide=FALSE) +
  geom_line(size=0.8) + xlab(NULL) + ylab(NULL) +
  facet_wrap(~name,ncol=2,scales="free_y") +
  scale_x_discrete(breaks=pretty_breaks()) +
  theme_bw() + theme(legend.position="none",
                     strip.background=element_blank(),
                     panel.border=element_rect(colour="grey"))
facetAdjust(p)

#coeur et p?riph?rie en zone euro
nealist_long<-list("EE","SI","MT","SK","LU","CY","BG","CZ","DK","HR","HU","LT","LV","PL","RO","SE","UK")
procdata_ea <- subset(procdata,!(country %in% nealist_long))
country_core <-  list("DE","FR","BE","NL","AT","FI")
procdata_ea$core <- procdata_ea$country %in% country_core

plotdata<-data.frame()

for (varname in ordvarnames){
  newdata<-data.frame()
  for (cor in list(TRUE,FALSE)){
    sub<-subset(procdata_ea,variable==varname & core==cor)
    sub$date<-year(sub$date)
    subpd<-subset(sub,select=-c(name,core,isnea,variable))
    pd<-pdata.frame(subpd,index=c("country","date"))
    plm<-plm(value~date,data=pd)
    newdata2<-data.frame(plmcoef=plm$coef,
                         plmse=sqrt(diag(plm$vcov)),
                         time=levels(plm$model$date)[-1],
                         var=varname,
                         name=unique(sub$name),
                         core=cor)
    newdata<-rbind(newdata,newdata2)
  }
  plotdata <- rbind(plotdata,newdata)
}

plotdata$core<-factor(plotdata$core)
levels(plotdata$core)<-c("périphérie","coeur")

p<-ggplot(data=plotdata,aes(x=time,y=plmcoef,linetype=core,colour=var,group=core)) +
  geom_line(size=0.8) + xlab(NULL) + ylab(NULL) +
  facet_wrap(~name,ncol=2,scales="free_y") +
  guides(colour='none',linetype=guide_legend("Pays de la zone euro : ")) +
  scale_x_discrete(breaks=pretty_breaks()) +
  theme_bw() + theme(strip.background=element_blank(),
                     panel.border=element_rect(colour="grey"),
                     legend.position="bottom",
                     legend.key=element_rect(colour="white"))
facetAdjust(p)
