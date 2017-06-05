#------------------------------------------
#NYC 311
#
#by David A. Gelman
#Originally coded on 10/27/2016
#------------------------------------------
#------------------------------------------

rm(list=ls())

#libraries
library(data.table)
library(Quandl)
library(plyr)
library(dplyr)
library(stringi)
library(readr)
library(lmtest)
library(car)
library(robustbase)
library(tseries)
library(orcutt)
library(ggplot2)
library(scales)

#------------------------------------------

#set working directory
#setwd("~NYC311")

#------------------------------------------
#------------------------------------------
#load in the data
#first data source, NYC 311 calls in 2013
#https://data.cityofnewyork.us/Social-Services/311-Service-Requests-2013/e8jc-rs3b
complaint.data=fread("311_Service_Requests_2013.csv", sep=",")
manhattan.complaint=subset(complaint.data,complaint.data$Borough=="MANHATTAN")

#need to replace spaces with "_" in column names
manhattan.complaint=as.data.table(manhattan.complaint)
colnames(manhattan.complaint)=gsub('([[:punct:]])|\\s+','_',colnames(manhattan.complaint))

#now create a day count
manhattan.complaint=manhattan.complaint[order(Created_Date),]
manhattan.complaint$open_date=stri_sub(manhattan.complaint$Created_Date,1,10)
manhattan.complaint$open_date=as.Date(manhattan.complaint$open_date, format="%m/%d/%Y")
manhattan.complaint$dayid=difftime(as.POSIXlt(manhattan.complaint$open_date),
                                 as.POSIXlt(as.Date("01/01/2013",format="%m/%d/%Y")),
                                 units="days")

#now load in the weather data
#from NOAA National Centers for Envrionmental Information
#https://www.ncdc.noaa.gov/cdo-web/datasets#GHCND
#obtained 10/27/2016
weather.data=read.csv("NOAA_NYC_2013Data.csv")

#now create a day count
weather.data$day=as.Date(as.character(weather.data$DATE),format="%Y %m %d")
weather.data$dayid=difftime(as.POSIXlt(weather.data$day),
                                   as.POSIXlt(as.Date("01/01/2013",format="%m/%d/%Y")),
                                   units="days")

#now load in the market data
#from Quandl's native R package
#market.data.nasdaq<-Quandl("NASDAQOMX/NQUSA", start_date="2013-01-01", end_date="2013-12-31")
market.data.dow=Quandl("YAHOO/INDEX_DJI", start_date="2013-01-01", end_date="2013-12-31")
market.data.dow$change=(market.data.dow$Close)-(market.data.dow$Open)
market.data.dow$percent_change=((market.data.dow$change)/(market.data.dow$Open))*100

#now create a day count
market.data.dow$dayid=difftime(as.POSIXlt(market.data.dow$Date,format="%Y-%m-%d"),
                               as.POSIXlt(as.Date("01/01/2013",format="%m/%d/%Y")),
                               units="days")

#------------------------------------------
#what are the days when the market is open?
data.days=unique(market.data.dow$dayid)

#what are the days after the market is open?
data.days.obs=data.days[]+1

#------------------------------------------
#we're only interesed in days the market is open, so let's subset weather and 311 calls and then merge
#all 3 data sources together

weather.data$inday="NA"
for (i in 1: length(weather.data$dayid)){
  weather.data$inday[i]=ifelse(weather.data$dayid[i] %in% data.days,1,0)
  print(i)
}

weather=subset(weather.data,weather.data$inday==1)

#since a for loop will take a long time over 382,522 obersvations and we're looking at days
  #as the unit of analysis, lets go ahead and do that first
manhattan.complaint$dayid=as.numeric(manhattan.complaint$dayid)
complaints=count(manhattan.complaint,c(manhattan.complaint$dayid))

colnames(complaints)[1]="dayid"
colnames(complaints)[2]="count"
complaints$inday="NA"
for (i in 1:length(complaints$dayid)){
  complaints$inday[i]=ifelse(complaints$dayid[i] %in% data.days,1,0)
  print(i)
}
complaints.ftm<-subset(complaints,complaints$inday==1)

#need to reorder the market data for a good combine:
market.data.dow=market.data.dow[order(-dayid),]

data=cbind(weather, market.data.dow,complaints.ftm)

#------------------------------------------
#------------------------------------------
#------------------------------------------
#now data is done and we can look at our 252 observation days
#------------------------------------------
#------------------------------------------
#------------------------------------------

#lets start by quickly looking at the dependent variable:
summary(data$count)
#lots of spread and variation but not too mcuh skew!

#now run a simple linear model
options(scipen = 999)
model1=lm(count~change+
             Volume+
             TMIN+
             PRCP,
           data=data)
print(summary(model1))

#breusch-pagan test for heteroskedasticity: null is homoskedasticity
bptest(model1)

#what about corrected covariance?
coeftest(model1, vcoc=hccm(model1))

#still insignificant so try RSEs
model1robust=lmRob(count~change+
                  Volume+
                  TMIN+
                  PRCP,
                data=data)
print(summary(model1robust))

#normality of residuals; Jarque-Bera, null is normal
jarque.bera.test(model1robust$residuals)

#autocorrelation?
res=residuals(model1robust)
par(mfrow=c(1,2))
plot(res,ylab="Residuals",xlab="DayID")
acf(res,main="ACF of Residuals")

#looks like some definite auto-correlation which is unsurprising given using market data

#now try a fGLS with Cochrane-Orcutt
model1.fgls=cochrane.orcutt(model1)
print(summary(model1.fgls))
print(model1.fgls)


#still insignificant in OLS, now turning to MLE (Binomial)
model2=glm.nb(count~change+
              Volume+
              TMIN+
              PRCP,
            data=data)
print(summary(model2))

#------------------------------------------
#------------------------------------------
#write out the data so I know how big it is and all the data work is saved:
write.csv(data,"NYC311.csv")

#------------------------------------------
#now let's make some plots

#one plot has all three things, counts, close, and TMIN across time
plot.data=as.data.frame(cbind(data$count,data[,35]))
counts.plot=ggplot(plot.data,aes(x=V2)) + geom_line(aes(y=V1))+
   theme_bw()+
  scale_x_continuous(expand=c(0,0),limits=c(0,366))+
  scale_y_continuous(expand=c(0,0), limits=c(0,2500))+                     
   labs(x="Day",y="Number of 311 Calls")
counts.plot

plot.data2=as.data.frame(cbind(data$change,data[,35]))
change.plot=ggplot(plot.data2,aes(x=V2)) + geom_line(aes(y=V1))+
  theme_bw()+
  scale_x_continuous(expand=c(0,0),limits=c(0,366))+
  scale_y_continuous(expand=c(0,0), limits=c(-400,400))+                     
  labs(x="Day",y="Change in the DJI Average")
change.plot

plot.data3=as.data.frame(cbind(data$TMIN,data[,35]))
temp.plot=ggplot(plot.data3,aes(x=V2)) + geom_line(aes(y=V1))+
  theme_bw()+
  scale_x_continuous(expand=c(0,0),limits=c(0,366))+
  scale_y_continuous(expand=c(0,0), limits=c(0,100))+                     
  labs(x="Day",y="Daily Low Temperature (in F)")
temp.plot

#now put the three together
grid.arrange(counts.plot, change.plot, temp.plot,ncol=1,top="Plotting the Raw Data Across 2013")

#------------------------------------------
#now lets look at types of complaints by day
man.complaint.data=as.data.table(cbind(manhattan.complaint$Unique_Key,
                                  manhattan.complaint$Complaint_Type,
                                  as.character(manhattan.complaint$open_date),
                                  manhattan.complaint$dayid))
colnames(man.complaint.data)<-c("unique_key",
                                "complaint_type",
                                "open_date",
                                "dayid")
#how many complaint_types are there?
types=unique(man.complaint.data$complaint_type)
length(unique(man.complaint.data$complaint_type))

#how many of each on each day?
man.complaint.data.ftw=man.complaint.data
man.complaint.data.ftw$complaint_type=as.factor(man.complaint.data.ftw$complaint_type)
man.complaint.data.ftw=as.data.frame(man.complaint.data.ftw)

man.topcomplaints=count(man.complaint.data.ftw,complaint_type)
man.topcomplaints=as.data.table(man.topcomplaints)
man.topcomplaints=man.topcomplaints[order(-n),]

#let's pick a few complaint types of interest, with an empasis on some more frequent ones:

man.complaints=subset(man.complaint.data.ftw,
                      man.complaint.data.ftw$complaint_type=="HEATING"|man.complaint.data.ftw$complaint_type=="Noise - Residential"|man.complaint.data.ftw$complaint_type=="Noise-Commercial"|man.complaint.data.ftw$complaint_type=="Broken Muni Meter"|man.complaint.data.ftw$complaint_type=="Taxi Complaint"|man.complaint.data.ftw$complaint_type=="Rodent"|man.complaint.data.ftw$complaint_type=="Recycling Enforcement"|man.complaint.data.ftw$complaint_type=="Homeless Encampment")
man.comp.counts=count_(man.complaints,vars=c("open_date","complaint_type"))

#okay, now we have several complain type counts per day, lets plot it
man.comp.counts.plotdata=man.comp.counts
man.comp.counts$open_date=as.Date(man.comp.counts.plotdata$open_date)

complaint_type.plot=ggplot(man.comp.counts,
                            aes(x=open_date,y=n,colour=complaint_type,group=complaint_type))+
  geom_line()+theme_bw()
complaint_type.plot<-complaint_type.plot + scale_x_date(limits = as.Date(c("2013-01-01", "2013-12-31")),
                     breaks="1 month",
                  labels=date_format("%b"))
complaint_type.plot=complaint_type.plot+ theme(axis.text.x=element_text(angle=90,hjust=1))+
  labs(main="311 Complaint Types Across 2013",x="",y="Number of 311 Calls")+
  scale_colour_discrete(name="Complaint Types")+
  scale_y_continuous(expand=c(0,0))
complaint_type.plot

