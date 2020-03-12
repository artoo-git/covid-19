######################################################################################

###                                 Online Data Multiple country (please contribute)

#####################################################################################
#
# Thanks to Alaan Rathery for the suggestion of ggplot and with the binding of the prediction dataframe
#

library(dplyr)
library(reshape2)
library(gtools)
library(ggplot2)

#https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv

#####################

          country = c("Italy","Republic of Korea","France","UK","Germany","Switzerland","Iran (Islamic Republic of)")

####################

#data<-read.csv(file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv",header = TRUE,sep = ",")
data<-read.csv(file = "~/Windows/antanicov.csv",header = TRUE,sep = ",")

data<-data %>% select(2,5:ncol(data))

#subset<-data[which(data$Country.Region == country),]

subset<-data %>% filter_all(all_vars(Country.Region %in% country))
names(subset) <- c("Country.Region",1:(ncol(subset)-1))
long <- melt(subset, id.vars = c("Country.Region"))

long$Country.Region<- droplevels(long$Country.Region)



#############################################
####################
#################### observed daily count plot: It should not look exponential if national counter-measures are working
####################
#############################################

colnames(long)<- c("country","day","total")

long$day<-as.numeric(long$day)

long$count <- 0 #initialize a count variable

i<-1
# calculate  the difference in scores to model the daily increase and not the total count

while (i <= nrow(long)){
  total<-long$total[i]
  day<- long$day[i]
  cntr<-long$country[i]
  if(total > 1 & day >1){
    prev_tot<-long[which(long$country == cntr & long$day == (day-1)),][3]
    long$count[i]<-as.numeric(total - prev_tot)
  }else{
    long$count[i]<-as.numeric(long$total[i])
  }
  i<-i+1
}
ggplot(data = long, aes(x=day, y=count, colour=country)) +
  geom_point() +
  geom_line(data = long, aes(x=day, y=count, colour = country))+
  ggtitle(paste("Everyday count of new cases"))

#############################################
####################
#################### Total count plot: It should present with a good fir with a non linear logistic model
####################
#############################################
#subset<-data[which(data$Country.Region == country),]

subset<-data %>% filter_all(all_vars(Country.Region %in% country))
names(subset) <- c("Country.Region",1:(ncol(subset)-1))
long <- melt(subset, id.vars = c("Country.Region"))
long$Country.Region<- droplevels(long$Country.Region)
colnames(long)<- c("country","day","count")
long$day<-as.numeric(long$day)

########################################################
######  self-determination of the starts parameters. 
# This sometimes breaks or is undeterminated. 
#
#find the parameters for the equation


predictdf<-data.frame() # this df is to rbind extrapolated values and counts for ggplot
i<-1
for (c in country){ # loops around the country selected, runs nls(), and builds the predictdf dataframe
  
  subs<-long[which(long$country == c),]
  outbr_day <- min(which(subs$count >15)) # set a minimum of 20 cases to facilitate convergence
  subs<-subs[which(subs$day > outbr_day),] 
  
  # estimation of start point and growth rate
  SS<-getInitial(count~SSlogis(day,alpha,xmid,scal),data=data.frame(count=subs$count,day=subs$day)) 
  
  # Setting the upper asyntote (lower asyntote is zero)
  a_start<-SS["alpha"]
  
  #logit(subs$count/a_start) # just a debug output
  phi<-coef(lm(logit(subs$count/a_start)~day,data=subs)) # another way of estimating the growth rate
  
  #logistic model - I think is the same as the Richards Logistic Growth model
  m<-nls(count~ a/(1+exp(-(b+g*day))), start=list(a=a_start,b=phi[1],g=phi[2]),data=subs)
  
  summary(m)
  
  #get some estimation of goodness of fit
  cor(subs$count,predict(m))
  
  days<-((outbr_day+1):60) # number of day for plot and extrapolation
  predict<-predict(m, newdata =  data.frame(day = days)) #  extrapolation
  cc<-rep(c, each = 60-outbr_day) # create the factor "country" column
  
  count_temp<-subs$count  # prepare a column with real counts
  length(count_temp) = length(cc) # fill NA where no data is present
  
  predictdf<-(rbind(predictdf,data.frame(count = count_temp,predict,day=days,country=cc))) # assemble the dataframe
  
  i<-i+1
}

sysdate<-Sys.Date() %>% format(format="%B %d %Y")

ggplot(data = predictdf, aes(x=day, y=count, colour=country)) +
  geom_point() +
  scale_y_continuous(breaks = seq(0, 30000, len = 11))+
  ggtitle(paste("Count (dots) and prediction (line) of total cases. Updated", sysdate))+
  geom_line(data = predictdf, aes(x=day, y=predict, colour = country))
