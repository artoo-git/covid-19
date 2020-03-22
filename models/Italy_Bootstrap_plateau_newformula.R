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
#subs$day<-1:nrow(subs)
print(paste(subs$day[1],subs$count[1],subs$country[1]))
# setting outbreak day to one. Comment this to see the delay start
outbr_day<-min(subs$day)


#logistic model - I think is the same as the Richards Logistic Growth model

# y(t) = α/((1 + β * exp(-k * t))^(1 / m))
#
#   t	= time
#   alpha = upper asymptote
#   beta=growth range
#   k	= growth rate
#   m	= slope of growth


## estimating parameters

# estimation of a starting plateau level of y and midpoint
SS<-getInitial(count~SSlogis(day,Asym,xmid,scale),data=data.frame(count=subs$count,day=subs$day)) 
#logit(subs$count/a_start) # just a debug output


# alpha
# Setting the upper asyntote
a_start<-SS["Asym"] %>%as.numeric()
#K_start<-120000

#beta range of growth
b_start<-SS["Asym"]/(exp(SS["xmid"]/SS["scale"])+1) %>% as.numeric()

# midpoint
mid<-SS["xmid"] %>% as.numeric()
# 
#growth rate parameter
kappa<-1/SS["scale"]

#growth rate parameters <- intercept and slope (FIX ME IF I AM WRONG)
k_r<-coef(lm(logit(subs$count/a_start)~day,data=subs))[2] %>% as.numeric()
k_i<-coef(lm(logit(subs$count/a_start)~day,data=subs))[1] %>% as.numeric()



m<-nls(count~ a/(1 + b_start*exp(-(k_i+k_r* day)))^(1/b_start), start=list(a=a_start,
                                                 k_i=k_i,
                                                 k_r=k_r),data=subs)




cor(subs$count,predict(m))

x<-min(subs$day):(max(subs$day)+20)
y<-subs$count
p<-predict(m, newdata = data.frame(day=x))
length(y)<-length(x)
new<-data.frame(x,y,p)

cor(subs$count,predict(m))

ggplot(new,aes(x,y))+
  geom_point()+
  geom_line(aes(x,p))

predict(m, newdata = data.frame(day=31:65))

################### Bootstrap
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

daysAhead<-5
x<-min(subs$day):(max(subs$day)+daysAhead)
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

span<-x
pred<-round(max(predict(m, newdata = data.frame(day=span),1)))
lowbound<-min(curveDF[which(curveDF$day == max(span)),][1]) %>% round(0)
highbound<-max(curveDF[which(curveDF$day == max(span)),][1]) %>% round(0)

png("images/ITmodel.png", width = 600, height = 600, units = "px")
ggplot(curveDF, aes(x=day, y=count, group=bsP)) +
  geom_line(color="blue") +
  geom_vline(xintercept = max(subs$day),linetype = "dashed")+
  annotate("text", hjust = 1, x = max(subs$day), y = max(subs$count), label = paste(max(subs$count), ". Count as per", sysdate))+
  annotate("text", hjust = 1, x = max(span), y = pred, label = paste(pred, "estimated in ", daysAhead, "days"))+
  annotate("text", hjust = 1, x = max(span), y = lowbound, label = lowbound)+
  annotate("text", hjust = 1, x = max(span), y = highbound, label = highbound)+
  labs( title = paste(country, "- 5 days projection of total counts of cases"),
        subtitle = paste("Bootstrapped CI at 5 days from", sysdate),
        caption = paste("Updated ", sysdate, ". Data source: Johns Hopkins public dataset")
  )
dev.off()

     
