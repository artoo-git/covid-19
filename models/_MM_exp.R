######################################################################################

###                                 Online Data 

#####################################################################################

# probably wrong implementation of the Michelis-Menten model of growth 

#https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv

library(reshape2)
library(gtools)
library(boot)
library(nlstools)

#####################
country = "United Kingdom" 
####################

par(mfrow=c(2,1))

data<-read.csv(file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv",header = TRUE,sep = ",")

data<-data %>% select(1:2,5:ncol(data))
# the csv has inconsistent ways to name UK ( AS USUALLY happens with Great britain, UK, United Kingdom)
levels(data$Province.State) <- c(levels(data$Province.State),"United Kingdom")
data[data=="UK"]<-as.character("United Kingdom") 


# Subsetting
subset<-data %>% filter_all(all_vars(Country.Region %in% country))
subset<-subset %>% filter_all(all_vars(Province.State %in% c(country,"")))
subset<- subset %>% select(2:ncol(data))

#subset<-subset[,1:52]

names(subset) <- c("Country.Region",(1:(ncol(subset)-1)))



long <- melt(subset, id.vars = c("Country.Region"))

colnames(long)<- c("country","day","total")

long[which(long$country=="United Kingdom" & long$day==51),3]<-590
long$day<-as.numeric(long$day)
############################################# 
####################
#################### observed daily count plot: It should not look exponential if measures are working
####################
#############################################






outbr_day <- min(which(long$total >33)) # set a minimum of 20 cases to facilitate convergence
long<-long[which(long$day >= outbr_day),] 
long$day<-1:nrow(long)

long$daily <- 1 #initialize a count variable

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



a_start<-35 #param a is the y value when x=0 -> this parameter needs to be eyeballed looking at the scatterplot
b_start<-2*log(2)/a_start #b is the decay rate

m<-nls(daily~a*exp(b*day),start=list(a=a_start,b=b_start), data = long)

summary(m)

#get some estimation of goodness of fit
cor(long$total,predict(m))

plot(long$day,long$daily, main = paste(country, ":everyday count of new cases"))
lines(long$day,predict(m),col="red",lty=3,lwd=2)

############################################# 
####################
#################### Total count plot: It should deviate from the exponential growth
####################
#############################################

subset<-data[which(data$Country.Region == country),]

library(reshape2)


a_start<-33 #param a is the y value when x=0
b_start<-2*log(2)/a_start #b is the decay rate
long
m<-nls(total~a*exp(b*day),start=list(a=a_start,b=b_start), data = long)

summary(m)

#get some estimation of goodness of fit
cor(long$total,predict(m))

plot(long$day,long$total, main = paste(country, ":Total count of new cases"))
lines(long$day,predict(m),col="red",lty=3,lwd=2)

#n.Iter<-5000 ### 
#bootL<- nlsBoot(m, niter = n.Iter)

predict(m, newdata =  data.frame(day = 1:15))

#hist(bootL$coefboot[,1], breaks = 200, main = paste("boostrap value of extrapolated value of plateau for ", country))
#abline(v=bootL$estiboot[1,1], col = "blue")
#text(bootL$estiboot[1,1], -2, round(bootL$estiboot[1,1],1), srt=0.4, col = "blue")


