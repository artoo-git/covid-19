######################################################################################

###                                 Online Data 

#####################################################################################

# probably wrong implementation of the Michelis-Menten model of growth 

#https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv

#####################
country = "France" 
####################

par(mfrow=c(2,1))

data<-read.csv(file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv",header = TRUE,sep = ",")

data<-data %>% select(2,5:ncol(data))

subset<-data[which(data$Country.Region == country),]

library(reshape2)

long <- melt(subset, id.vars = c("Country.Region"))


############################################# 
####################
#################### observed daily count plot: It should not look exponential if measures are working
####################
#############################################
colnames(long)<- c("country","date","total")


long$day<-1:nrow(long)

long$count <- 1 #initialize a count variable
i<-1

# calculate  the difference in scores to model the daily increase and not the total count
while (i <= nrow(long)){
  if(long$total[i] > 1){
    long$count[i]<-long$total[i]-long$total[i-1]
  }else{
    long$count[i]<-long$total[i]
  }
  i<-i+1
}




a_start<-33 #param a is the y value when x=0 -> this parameter needs to be eyeballed looking at the scatterplot
b_start<-2*log(2)/a_start #b is the decay rate

m<-nls(count~a*exp(b*day),start=list(a=a_start,b=b_start), data = long)

summary(m)

#get some estimation of goodness of fit
cor(long$count,predict(m))

plot(long$day,long$count, main = paste(country, ":everyday count of new cases"))
lines(long$day,predict(m),col="red",lty=3,lwd=2)

############################################# 
####################
#################### Total count plot: It should deviate from the exponential growth
####################
#############################################

subset<-data[which(data$Country.Region == country),]

library(reshape2)

long <- melt(subset, id.vars = c("Country.Region"))

long<-long[1:nrow(long),]

colnames(long)<- c("country","date","count")


long$day<-1:nrow(long)


a_start<-33 #param a is the y value when x=0
b_start<-2*log(2)/a_start #b is the decay rate

m<-nls(count~a*exp(b*day),start=list(a=a_start,b=b_start), data = long)

summary(m)

#get some estimation of goodness of fit
cor(long$count,predict(m))

plot(long$day,long$count, main = paste(country, ":Total count of new cases"))
lines(long$day,predict(m),col="red",lty=3,lwd=2)

predict(m, newdata =  data.frame(day = 1:49))

