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

country = c("Italy")

####################

sysdate<-Sys.Date() %>% format(format="%B %d %Y")
#data<-read.csv(file = "~/Windows/time_series_19-covid-Confirmed.csv",header = TRUE,sep = ",")
link<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
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
long[51,3] <-15113 # csv is wrong
long[55,3]<-27980

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

########### selfstart paramenters for the logistic function 
## this model will be passed on to nlsboot

# Asym is the carrying capacity - Upper plateau
# xmid is the x value at the inflection point of the logistic curve
# scal is a scaling parameter for the x-axis.

## same as getInitials, but I am going to feed this into bootL
logisticModelSS <- nls(count~SSlogis(day, Asym, xmid, scal), data = subs)

n.Iter <-5000
SSboot<- nlsBoot(logisticModelSS, niter = n.Iter)

hist(SSboot$coefboot[,3])

#growth rate parameter k is
k_start<- (-1/SSboot$estiboot[3]) %>% as.numeric

# alpha
# Setting the upper asyntote
# from getInitials this is simply Asym
a_start<-SSboot$estiboot[1] %>% as.numeric()

#b_start (lower plateau in N of cases) as per self-start can be defived from xmid = -bstart/k
b_start<- -SSboot$estiboot[2]*k_start


# midpoint in numner of days
mid<-SSboot$estiboot[2] %>% as.numeric()


##### IDEAL LOGISTIC MODEL ( SEE Italy bootstrap new formula.R for the work in progress)
# 
#Richards logistic curve is the ideal model to fit (see previous epidemiological studies on sars)
# y(t) = α/((1 + β * exp(-k * t))^(1 / m))
#
#   t	= time
#   alpha = upper asymptote
#   beta=growth range
#   k	= growth rate
#   m	= slope of growth


#logistic model - I think is not the same as the Richards Logistic Growth model it is missing the growth range. 
# it is a simple enough logistic growth model and it should "do" with some more error. 

m<-nls(count~ a/(1 + exp(b+k * day)), start=list(
                                                a=a_start, 
                                                k=k_start,
                                                b=b_start
                                              ),data=subs)

# where: 
#   k_startu is the boostrapped growth rate parameter k 
#   a_start is the boostrapped upper asyntote
#   b_start is the boostrapped lower asyntote


################### Bootstrap (the number of rows to bootstrap in this time series is ridicolously small .. )

n.Iter<-5000 ### careful with the iterations, the plotting can be time consuming
bootL<- nlsBoot(m, niter = n.Iter)

png("images/ITplateau.png", width = 600, height = 600, units = "px")
  hist(bootL$coefboot[,1], xlab = paste("Prediction of tot detected cases at plateau -", sysdate), breaks = 200, main = paste("ITALY: Bootstrap extrapolation of total count at plateau (", sysdate, ")"))
  abline(v=bootL$estiboot[1,1], col = "blue")
  text(bootL$estiboot[1,1], -2, round(bootL$estiboot[1,1],1), srt=0.4, col = "blue")
dev.off()

n.Iter<-200 ### careful with the iterations, the plotting can be time consuming
bootL<- nlsBoot(m, niter = n.Iter)
hist(bootL$coefboot[,1], breaks = 200)

#set days ahead for the prediciton
daysAhead<-10

x<-1:(nrow(subs)+daysAhead)
Param_Boo<-bootL$coefboot
curveDF <- data.frame(matrix(0,ncol = 3,nrow =n.Iter*length(x)))
for(i in 1:n.Iter){
  for(j in 1:length(x)){
    # Function value
    curveDF[j+(i-1)*n.Iter,1] <- Param_Boo[i,1]/(1+exp(Param_Boo[i,3]+Param_Boo[i,2]*x[j]))
    # Bootstrap sample number
    curveDF[j+(i-1)*n.Iter,2] <- i
  # x value
    curveDF[j+(i-1)*n.Iter,3] <- x[j]
  }
}
colnames(curveDF) <- c('count','bsP','day')

span<-1:(nrow(subs)+daysAhead)
pred<-round(max(predict(m, newdata = data.frame(day=span),1)))
lowbound<-min(curveDF[which(curveDF$day == max(span)),][1]) %>% round(0)
highbound<-max(curveDF[which(curveDF$day == max(span)),][1]) %>% round(0)

png("images/ITmodel.png", width = 600, height = 600, units = "px")
ggplot(curveDF, aes(x=day, y=count, group=bsP)) +
  xlim(1,(max(subs$day)+daysAhead+5))+
  geom_line(color="blue") +
  geom_vline(xintercept = max(subs$day),linetype = "dashed")+
  annotate("text", hjust = 1.05, vjust= 0, x = max(subs$day), y = max(subs$count), label = paste(max(subs$count)," as per", sysdate))+
  annotate("text", hjust = 0, vjust= 0, x = max(span), y = pred, label = paste(pred))+
  annotate("text", hjust = 0, vjust= 1, x = max(span), y = lowbound, label = lowbound)+
  annotate("text", hjust = 0, vjust= 0, x = max(span), y = highbound, label = highbound)+
  labs( title = paste(country, "- ", daysAhead, " days projection of total counts of cases"),
        subtitle = paste("Bootstrapped CI at ", daysAhead, "days from", sysdate),
        caption = paste("Updated ", sysdate, ". Data source: Johns Hopkins public dataset")
  )
dev.off()

predict(m, newdata = data.frame(day=1:58))
