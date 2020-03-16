######################################################################################

###                                 Bootstrap values of plateau for Italy

#####################################################################################
#

library(dplyr)
library(reshape2)
library(gtools)
library(ggplot2)
library(boot)
library(nlstools)


#https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv

#####################

country = c("France")

####################

sysdate<-Sys.Date() %>% format(format="%B %d %Y")
#link<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
link<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
data<-read.csv(file = link,header = TRUE,sep = ",")

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


names(subset) <- c("Country.Region",paste("day.",(1:(ncol(subset)-1)),sep = ""))


long <- melt(subset, time.var= subset(2:ncol(subset)),id.vars = c("Country.Region"))
long[51,3] <-2876 # csv is wrong
long[54,3] <-5423 # csv is wrong

long$Country.Region<- droplevels(long$Country.Region)

#############################################
####################
#################### Total count plot: It should present with a good fir with a non linear logistic model
####################
#############################################

colnames(long)<- c("country","day","count")
long$day<-as.numeric(long$day)

########################################################
######  self-determination of the starts parameters. 
# This sometimes breaks or is undeterminated. 
#
#find the parameters for the equation



subs<-long
outbr_day <- min(which(subs$count >10)) # set a minimum of 20 cases to facilitate convergence
subs<-subs[which(subs$day >= outbr_day),] 
subs$day<-1:nrow(subs)
print(paste(subs$day[1],subs$count[1],subs$country[1]))

# setting outbreak day to one. Comment this to see the delay start
outbr_day<-1 
# estimation of start point and growth rate
SS<-getInitial(count~SSlogis(day,alpha,xmid,scal),data=data.frame(count=subs$count,day=subs$day)) 

# Setting the upper asyntote (lower asyntote is zero)
a_start<-SS["alpha"]

#logit(subs$count/a_start) # just a debug output
phi<-coef(lm(logit(subs$count/a_start)~day,data=subs)) # another way of estimating the growth rate

#logistic model - I think is the same as the Richards Logistic Growth model
m<-nls(count~ a/(1+exp(-(b+g*day))), start=list(a=a_start,b=phi[1],g=phi[2]),data=subs)

################### Bootstrap
n.Iter<-5000 ### careful with the iterations, the plotting can be time consuming
bootL<- nlsBoot(m, niter = n.Iter)

png("images/ITplateauD.png", width = 600, height = 600, units = "px")
hist(bootL$coefboot[,1], breaks = 200, main = "boostrap value of extrapolated value of plateau for France")
abline(v=bootL$estiboot[1,1], col = "blue")
text(bootL$estiboot[1,1], -2, round(bootL$estiboot[1,1],1), srt=0.4, col = "blue")
dev.off()

n.Iter<-200 ### careful with the iterations, the plotting can be time consuming
bootL<- nlsBoot(m, niter = n.Iter)
hist(bootL$coefboot[,1], breaks = 200, main = "boostrap value of extrapolated value of plateau for France")

x<-1:(nrow(subs)+5)
Param_Boo<-bootL$coefboot
curveDF <- data.frame(matrix(0,ncol = 3,nrow =n.Iter*length(x)))
for(i in 1:n.Iter){
  for(j in 1:length(x)){
    # Function value
    curveDF[j+(i-1)*n.Iter,1] <- Param_Boo[i,1]/(1+exp(-(Param_Boo[i,2]+Param_Boo[i,3]*x[j])))
    # Bootstrap sample number
    curveDF[j+(i-1)*n.Iter,2] <- i
    # x value
    curveDF[j+(i-1)*n.Iter,3] <- x[j]
  }
}
colnames(curveDF) <- c('count','bsP','day')

span<-1:(nrow(subs)+5)
pred<-round(max(predict(m, newdata = data.frame(day=span),1)))
lowbound<-min(curveDF[which(curveDF$day == max(span)),][1]) %>% round(0)
highbound<-max(curveDF[which(curveDF$day == max(span)),][1]) %>% round(0)

png("images/ITmodelD.png", width = 600, height = 600, units = "px")
ggplot(curveDF, aes(x=day, y=count, group=bsP)) +
  geom_line(color="blue") +
  geom_vline(xintercept = max(subs$day),linetype = "dashed")+
  annotate("text", hjust = 1, x = max(subs$day), y = max(subs$count), label = paste(max(subs$count), ". Count as per", sysdate))+
  annotate("text", hjust = 1, x = max(span), y = pred, label = paste(pred, "estimated in 5 days"))+
  annotate("text", hjust = 1, x = max(span), y = lowbound, label = lowbound)+
  annotate("text", hjust = 1, x = max(span), y = highbound, label = highbound)+
  ggtitle(paste("Projection of Total counts of cases 5 days for France as per",sysdate))
dev.off()
#predict(m, data = data.frame(24))
