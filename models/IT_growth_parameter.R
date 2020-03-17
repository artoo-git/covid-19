
library(dplyr)
library(reshape2)
library(gtools)
library(ggplot2)
library(gridExtra)

#https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv

#####################

country = c("Italy")

####################


data<-read.csv(file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv",header = TRUE,sep = ",")
#data<-read.csv(file = "~/Windows/antanicov.csv",header = TRUE,sep = ",")

data<-data %>% select(1:2,5:ncol(data))
#subset<-data[which(data$Country.Region == country),]

# the csv has inconsistent ways to name UK ( AS USUALLY happens with Great britain, UK, United Kingdom)
levels(data$Province.State) <- c(levels(data$Province.State),"United Kingdom")
data[data=="UK"]<-as.character("United Kingdom") 


# Subsetting
subset<-data %>% filter_all(all_vars(Country.Region %in% country))
subset<-subset %>% filter_all(all_vars(Province.State %in% c(country,"")))
subset<- subset %>% select(2:ncol(data))

#subset<-subset[,1:52]

names(subset) <- c("Country.Region",(1:(ncol(subset)-1)))


long <- melt(subset, time.var= subset(2:ncol(subset)),id.vars = c("Country.Region"))
colnames(long)<- c("country","day","total")

#long<- long[which(long$value != 0),]# select all rows with zero counts for deletion

long$country<-droplevels(long$country)
long$day<-as.numeric(long$day)
#currently 13/03/20:10h10 GMT csv is wrong on the totals correcting
#####################################################################
long[which(long$country=="Italy" & long$day==51),3]<-15113          #
#long[which(long$country=="Italy" & long$day==55),3]<-27980 
long[55,]<-data.frame(country = "Italy",day = 55, total = 27980)
#long[which(long$country=="United Kingdom" & long$day==51),3]<-590   #
#long[which(long$country=="United Kingdom" & long$day==54),3]<-1543   #
#long[which(long$country=="France" & long$day==51),3]<-2876  
#long[which(long$country=="France" & long$day==54),3]<-5423 

#############################################
####################
#################### Growth parameter: It should be 1 when approaching the inflection point
####################
#############################################
colnames(long)<- c("country","day","total")

long$day<-as.numeric(long$day)

long$daily <- 0 #initialize a count variable

i<-1
# calculate  the difference in scores to model the daily increase and not the total count

while (i <= nrow(long)){
  total<-long$total[i]
  day<- long$day[i]
  cntr<-long$country[i]
  if(total > 1 & day >1){
    prev_tot<-long[which(long$country == cntr & long$day == (day-1)),][3]
    long$daily[i]<-as.numeric(total - prev_tot)
  }else{
    long$daily[i]<-as.numeric(long$total[i])
  }
  i<-i+1
}

 daily<-long[which(long$country=="Italy"),][4]
 num<-daily$daily
 den<-c(1,num)[1:nrow(long)]
 long$growth<-0
 long[which(long$country=="Italy"),][5]<-growth<-num/den
 long[which(long$country=="Italy"),]

