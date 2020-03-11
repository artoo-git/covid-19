
######################################################################################

###                                 Online Data Multiple country

#####################################################################################
# Thanks to Alaan Rathery for the input on ggplots

library(dplyr)
library(reshape2)
library(gtools)
library(ggplot2)

#https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv

#####################

          country = c("Italy","France","UK","Germany","Republic of Korea")

####################

data<-read.csv(file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv",header = TRUE,sep = ",")
data<-data %>% select(2,5:ncol(data))

#subset<-data[which(data$Country.Region == country),]

subset<-data %>% filter_all(all_vars(Country.Region %in% country))
names(subset) <- c("Country.Region",1:(ncol(subset)-1))
long <- melt(subset, id.vars = c("Country.Region"))

long$Country.Region<- droplevels(long$Country.Region)

#############################################
####################
#################### observed daily count plot: It should not look exponential if measures are working
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
  ggtitle(paste(country, ":everyday count of new cases"))

#plot(long$day,long$count, main = paste(country, ":everyday count of new cases"))
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

# #find the parameters for the equation
#  SS<-getInitial(count~SSlogis(day,alpha,xmid,scale),data=data.frame(count=long$count,day=long$day))
#
# #we used a different parametrization
#  K_start<-SS["alpha"]
#  R_start<-1/SS["scale"]
#  N0_start<-SS["alpha"]/(exp(SS["xmid"]/SS["scale"])+1)
#  #the formula for the model
#  log_formula<-formula(count~K*N0*exp(R*day)/(K+N0*(exp(R*day)-1)))
#  #fit the model
#  m<-nls(log_formula,start=list(K=K_start,R=R_start,N0=N0_start), data = long)
#  #estimated parameters
#  summary(m)
#  #get some estimation of goodness of fit
#  cor(long$count,predict(m))
#
# plot(long$day,long$count, main = paste(country, ":Total count of new cases"))
# lines(long$day,predict(m),col="red",lty=2,lwd=3)
#uncomment this if the self-determining function breaks

predictdf<-data.frame()

i<-1
for (c in country){
  
  subs<-long[which(long$country == c),]
  outbr_day <- min(which(subs$count >13))
  subs<-subs[which(subs$day > outbr_day),] # removing the zero counts to facilitate convergence
  
  a_start<-15000# asymptote I havet 100000 as a place where I would def expect this virus to have a plateau
  logit(subs$count/a_start)
  phi<-coef(lm(logit(subs$count/a_start)~day,data=subs))
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

ggplot(data = predictdf, aes(x=day, y=count, colour=country)) +
  geom_point() +
  ggtitle(paste(country, ":Total count of new cases"))+
  geom_line(data = predictdf, aes(x=day, y=predict, colour = country))
