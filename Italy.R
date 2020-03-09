################################

#Online Data Italy

###############################

#https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv



data<-read.csv(file = "/home/diego/Windows/time_series_19-covid-Confirmed.csv",header = TRUE,sep = ",")
data<-data %>% select(2,5:51)

subset<-data[which(data$Country.Region == "Italy"),]

library(reshape2)

long <- melt(subset, id.vars = c("Country.Region"))
colnames(long)<- c("country","date","count")


long$day<-1:length(long$date)

long<-long[29:47,]


plot(long$day,long$count)

#Long %>% ggplot(aes(x = day, y = count))+
#                  geom_point(size=2, shape=1)
#



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

lines(long$day,predict(m),col="red",lty=2,lwd=3)
