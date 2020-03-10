
######################################################################################

###                                 Online Data South Korea

#####################################################################################


#https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv

#####################
country = "Italy" 
####################


data<-read.csv(file = "/home/diego/Windows/time_series_19-covid-Confirmed.csv",header = TRUE,sep = ",")

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


long$day<-1:length(long$date)

long$count <- 1 #initialize a count variable
i<-1

# calculate  the difference in scores to model the daily increase and not the total count
while (i <= length(long$day)){
  if(long$total[i] > 1){
    long$count[i]<-long$total[i]-long$total[i-1]
  }else{
    long$count[i]<-long$total[i]
  }
  i<-i+1
}


plot(long$day,long$count)

# uncomment this if the distribution is just exponential
#a_start<-33 #param a is the y value when x=0
#b_start<-2*log(2)/a_start #b is the decay rate

#m<-nls(count~a*exp(b*day),start=list(a=a_start,b=b_start), data = long)

#summary(m)

#get some estimation of goodness of fit
#cor(long$count,predict(m))


#plot(long$day,long$count)
#lines(long$day,predict(m),col="red",lty=3,lwd=2)


############################################# 
####################
#################### Total count plot: It should compile if the data presents a logistic curve
####################
#############################################

subset<-data[which(data$Country.Region == country),]

library(reshape2)

long <- melt(subset, id.vars = c("Country.Region"))

long<-long[1:nrow(long),]

colnames(long)<- c("country","date","count")


long$day<-1:nrow(long)


#find the parameters for the equation
SS<-getInitial(count~SSlogis(day,alpha,xmid,scale),data=data.frame(count=long$count,day=long$day))

#we used a different parametrization
K_start<-SS["alpha"]
R_start<-1/SS["scale"]
N0_start<-SS["alpha"]/(exp(SS["xmid"]/SS["scale"])+1)
#the formula for the model
log_formula<-formula(count~K*N0*exp(R*day)/(K+N0*(exp(R*day)-1)))
#fit the model
m<-nls(log_formula,start=list(K=K_start,R=R_start,N0=N0_start), data = long)
#estimated parameters
summary(m)
#get some estimation of goodness of fit
cor(long$count,predict(m))

plot(long$day,long$count)
lines(long$day,predict(m),col="red",lty=2,lwd=3)


# uncomment this if the distribution is just exponential
#a_start<-33 #param a is the y value when x=0
#b_start<-2*log(2)/a_start #b is the decay rate

#m<-nls(count~a*exp(b*day),start=list(a=a_start,b=b_start), data = long)

#summary(m)

#get some estimation of goodness of fit
#cor(long$count,predict(m))


#plot(long$day,long$count)
#lines(long$day,predict(m),col="red",lty=3,lwd=2)



