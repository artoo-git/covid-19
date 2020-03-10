
######################################################################################

###                                 Online Data South Korea

#####################################################################################


#https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv

#####################
country = "South Korea" 
####################

par(mfrow=c(2,1))

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


plot(long$day,long$count, main = paste(country, ":everyday count of new cases"))




############################################# 
####################
#################### Total count plot: It should present with a good fir with a non linear logistic model
####################
#############################################

subset<-data[which(data$Country.Region == country),]

library(reshape2)

long <- melt(subset, id.vars = c("Country.Region"))

long<-long[1:nrow(long),]

colnames(long)<- c("country","date","count")


long$day<-1:nrow(long)


##find the parameters for the equation
#SS<-getInitial(count~SSlogis(day,alpha,xmid,scale),data=data.frame(count=long$count,day=long$day))

# #we used a different parametrization
# K_start<-SS["alpha"]
# R_start<-1/SS["scale"]
# N0_start<-SS["alpha"]/(exp(SS["xmid"]/SS["scale"])+1)
# #the formula for the model
# log_formula<-formula(count~K*N0*exp(R*day)/(K+N0*(exp(R*day)-1)))
# #fit the model
# m<-nls(log_formula,start=list(K=K_start,R=R_start,N0=N0_start), data = long)
# #estimated parameters
# summary(m)
# #get some estimation of goodness of fit
# cor(long$count,predict(m))



#plot(long$day,long$count, main = paste(country, ":Total count of new cases"))
#lines(long$day,predict(m),col="red",lty=2,lwd=3)


# uncomment this if the self-determining function breaks
#a_start<-100000 # asymptote I have set 100000 as a place where I would def expect this virus to have a plateau
#
#
#phi<-coef(lm(logit(count/a_start)~day,data=long))
#
#m<-nls(count~a/(1+exp(-(b+g*day))), start=list(a=s_start,b=phi[1],g=phi[2]),data=long)
#
#summary(m)
#
##get some estimation of goodness of fit
#cor(long$count,predict(m))
#
#plot(long$day,long$count, main = paste(country, ":Total count of new cases"))
#lines(long$day,predict(m),col="red",lty=3,lwd=2)
#
#day <-nrow(long)
#coef(m)[1]/(1+exp(-(coef(m)[2]+coef(m)[3]*day)))
