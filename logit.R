setwd("C:/Users/Utilisateur/Dropbox/Indicateurs_SixPack")
source("./autre/preproc_data.R")

finaldata<-subset(finaldata,finaldata$isnea==FALSE)
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
                  sub$value>=10)
  add <- ifelse(testlist[,j],1,0)
  data2<-data.frame(sub,test=add)
  data<-rbind(data,data2)
}

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

#on crée le plot pour avoir l'évolution du nombre de pays dont la moyenne est
# > ou < au cutoff
plotdata<-subset(newdata,variable=="caI")
plotdata$fragil<-factor(plotdata$fragil)
g<-qplot(factor(year(date)),data=plotdata,geom="bar",fill=fragil) +
  xlab(NULL) + ylab(NULL)
g
#à mettre en parallèle avec les graphs des effets années


varname<-"crI"

library(pROC)

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
  #mettre une variable commune
  roc<-roc(fragil ~ value3,data=datareg,plot=T,ci=T)
  
  roc$ci
  #assign(paste("glm",varname,sep="_"),glm(datareg$test2 ~ datareg$value + datareg$value2 + datareg$value3,family="binomial",data=datareg))
    #print(summary(get(paste("glm",varname,sep="_"))))
}

library(ROCR)
obs<-rep(0:1, each=50)
pred<-c(runif(50,min=0,max=0.8),runif(50,min=0.3,max=0.6))
plot(roc(obs,pred))


ROCRpred<-predict(pred,obs)
plot(performance(ROCRpred,'tpr','fpr'))


library(pROC)
data(aSAH)
roc(aSAH$outcome)

#data$variable <- factor(data$variable)

#finaldata$isnea<-factor(finaldata$isnea,labels=c("pays en zone euro","pays hors zone euro"))



summary(glm)
glm$y[2]





sum(newdata$test2)

for (varname in varnames){
  assign(paste("val_",varname,sep=""),subset(data,variable==varname)$value)
}
13*17
length(val_caI)

tab<-xtabs(~test+variable+date,data=data)
str(tab)


m <- matrix(data=cbind(rnorm(30,0),rnorm(30,2),rnorm(30,5)),nrow=30,ncol=3)
apply(m,2,function(x) mean(x[x<0]))

library(ggmap)
library(mapproj)
map <- get_map(location='Paris',zoom=15)
ggmap(map)


