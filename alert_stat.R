## @knitr panel
library(plm)
library(scales)
source("./facetAdjust.R")
#Europe entière
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

#coeur et périphérie en zone euro
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

## @knitr fragil
#setwd("C:/Users/Utilisateur/Dropbox/Indicateurs_SixPack")
#source("./autre/preproc_data.R")
finaldata<-subset(procdata,procdata$isnea==FALSE)

#on crée un test
data <- data.frame()
for (j in 1:length(varnames)) {
  sub<-subset(finaldata,variable==varnames[j])
  testlist<-cbind(sub$value>=6 | sub$value<=-4,
                  sub$value<=-35,
                  sub$value>=5 | sub$value<=-5,
                  sub$value<=-6,
                  sub$value>=9,
                  sub$value>=6,
                  sub$value>=15,
                  sub$value>=160,
                  sub$value>=60,
                  sub$value>=10,
                  sub$value>=16.5)
  add <- ifelse(testlist[,j],1,0)
  data2<-data.frame(sub,test=add)
  data<-rbind(data,data2)
}

#on crée une indicatrice de fragilité (fragil), en fonction de test et du cutoff
dates<-year(levels(factor(data$date)))
newdata <- data.frame()
cutoff <- 0.3

for (co in countryea){
  for (dat in dates){
    sub2<-subset(data,data$country==co & year(data$date)==dat)
    ifelse(mean(sub2$test,na.rm=T)>=cutoff,sub2$fragil<-1,sub2$fragil<-0)
    newdata <- rbind(newdata,sub2)
  } 
}

#on crée le plot pour avoir l'évolution du nombre de pays 
#dont la moyenne est > ou < au cutoff
#à mettre en parallèle avec les graphs des effets années
plotdata<-subset(newdata,variable=="caI")
plotdata$fragil<-factor(plotdata$fragil)
levels(plotdata$fragil)<- list ("fragiles"="1","forts"="0")
qplot(factor(year(date)),data=plotdata,geom="bar",fill=fragil) +
  guides(colour='none',fill=guide_legend("Nombre de pays macroéconomiquement : ")) +
  xlab(NULL) + ylab(NULL) + theme_bw() +
  theme(panel.border=element_rect(colour="grey"),
        legend.position="bottom")


## @knitr auroc
#on crée un plot avec les AUROC
library(ROCR)
varnames<-varnames[-c(6,9)]
title_ind <- list("Compte courant",
                  "Position ext. nette de l'inv.",
                  "Taux de change effectif réel",
                  "Part de marché des exportations",
                  "Coût salarial unitaire nominal",
                  "Indice prix des logements déflaté",
                  "Crédit privé",
                  "Dette privée",
                  "Dette publique",
                  "Taux de chômage",
                  "Passif du secteur financier")
title_ind<-title_ind[-c(6,9)]
#2lags et 0.4 cutoff est le mieux
#pdf("C:/Users/Utilisateur/Dropbox/Indicateurs_SixPack/figure_stat/auroc.pdf")
par(mfrow=c(3,3),mar=c(2,2,2,2))

j<-1
#varname<-"crI"

for (varname in varnames){
  datareg <- subset(newdata,variable==varname)
  datareg2<-data.frame(datareg$date,datareg$value)
  datareg3<-data.frame(datareg$date,datareg$value)
  colnames(datareg2)<-c("date","value")
  colnames(datareg3)<-c("date","value")
  year(datareg2$date)<-year(datareg$date)+1
  year(datareg3$date)<-year(datareg$date)+2
  datareg<-subset(datareg,year(datareg$date)>=2001)
  datareg2<-subset(datareg2,year(datareg2$date)<=2011 & year(datareg2$date)>=2001)
  datareg3<-subset(datareg3,year(datareg3$date)<=2011)
  datareg$value2<-datareg2$value
  datareg$value3<-datareg3$value
  #fit.logit<-glm(fragil ~ value2 + value3 ,data=datareg,family=binomial(link="logit"))
  assign(paste("fit.logit",varname,sep=""),glm(fragil ~ value2 + value3 ,data=datareg,family=binomial(link="logit")))
  datareg$log.pred<-predict(get(paste("fit.logit",varname,sep="")),datareg,type="response")
  preds<-prediction(datareg$log.pred,datareg$fragil)
  perf<-performance(preds,"tpr","fpr")
  perf2<-performance(preds,"auc")
  plot(perf,main=title_ind[j])
  j<-j+1
  abline(0,1,col="red")
  text(0.6,0.2,paste("AUC = ",round(as.numeric(perf2@y.values),4)))
}

plot(0,type="n",xlab="",ylab="",axes=F)

#dev.off()


#library(xtable)
#print(xtable(get("fit.logitcrI")))
#get("fit.logitcrI")$coef[1]

#print(xtable(summary(fit.logitcaI)),caption="compte courant")

# datareg <- subset(newdata,variable %in% c("caI","crI"))
# datareg2<-data.frame(datareg$date,datareg$value)
# datareg3<-data.frame(datareg$date,datareg$value)
# colnames(datareg2)<-c("date","value")
# colnames(datareg3)<-c("date","value")
# year(datareg2$date)<-year(datareg$date)+1
# year(datareg3$date)<-year(datareg$date)+2
# datareg<-subset(datareg,year(datareg$date)>=2001)
# datareg2<-subset(datareg2,year(datareg2$date)<=2011 & year(datareg2$date)>=2001)
# datareg3<-subset(datareg3,year(datareg3$date)<=2011)
# datareg$value2<-datareg2$value
# datareg$value3<-datareg3$value
# subca<-subset(datareg,variable=="caI")
# subcr<-subset(datareg,variable=="crI")
# datalogit<-data.frame(fragil=subcr$fragil,
#                       valcr2=subcr$value2,valcr3=subcr$value3,
#                       valca2=subca$value2,valca3=subca$value3)
# fit.logit<-glm(fragil ~ valcr2 + valca2 + valcr3 ,data=datalogit,family=binomial(link="logit"))
# datalogit$log.pred<-predict(fit.logit,datalogit,type="response")
# preds<-prediction(datalogit$log.pred,datalogit$fragil)
# perf<-performance(preds,"tpr","fpr")
# perf2<-performance(preds,"auc")
# plot(perf,main="crédit+compte courant - 2 lag")
# abline(0,1,col="red")
# text(0.6,0.2,paste("AUC = ",round(as.numeric(perf2@y.values),4)))
# 
# summary(fit.logit)

## @knitr pca
library(reshape2)

prcomp

pcadata<-subset(procdata,select=-c(name,isnea))
head(pcadata)
co<-"AT"
for (co in country){
  subpca <- subset(pcadata,country==co)
  subpca <- na.omit(subpca)
  subpcamat <- as.matrix(acast(subpca,variable~date,value.var="value"))
  
  pca<-prcomp(subpcamat2,scale=TRUE)
}
summary(pca)

print(pca)

library(HSAUR)
data("heptathlon", package = "HSAUR")
head(heptathlon)

tmp <- data.frame(x=gl(2,3, labels=letters[24:25]),
                  y=gl(3,1,6, labels=letters[1:3]), 
                  z=c(1,2,3,3,3,2))

?mlag
