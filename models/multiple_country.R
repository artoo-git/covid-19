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
library(gridExtra)

#https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv

#####################

          country = c("Italy","France","United Kingdom","Spain","Germany")

####################
          

data<-read.csv(file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",header = TRUE,sep = ",")
#data<-read.csv(file = "~/Windows/time_series_19-covid-Confirmed.csv",header = TRUE,sep = ",")

data<-data %>% select(1:2,5:ncol(data))
#subset<-data[which(data$Country.Region == country),]

# the csv has inconsistent ways to name UK ( AS USUALLY happens with Great britain, UK, United Kingdom)
levels(data$Province.State) <- c(levels(data$Province.State),"United Kingdom")
data[data=="UK"]<-as.character("United Kingdom") 


# Subsetting
subset<-data %>% filter_all(all_vars(Country.Region %in% country))
subset<-subset %>% filter_all(all_vars(Province.State %in% c(country,"")))
subset<- subset %>% select(2:ncol(data))

#subset<-subset[,1:52]
  
names(subset) <- c("Country.Region",(1:(ncol(subset)-1)))


long <- melt(subset, time.var= subset(2:ncol(subset)),id.vars = c("Country.Region"))
colnames(long)<- c("country","day","count")

#long<- long[which(long$value != 0),]# select all rows with zero counts for deletion

long$country<-droplevels(long$country)

#currently 13/03/20:10h10 GMT csv is wrong on the totals correcting
#####################################################################
long[which(long$country=="Italy" & long$day==51),3]<-15113          #
long[which(long$country=="United Kingdom" & long$day==51),3]<-590   #
long[which(long$country=="United Kingdom" & long$day==54),3]<-1393   #
long[which(long$country=="United Kingdom" & long$day==58),3]<-3269   #
long[which(long$country=="France" & long$day==51),3]<-2876  
long[which(long$country=="France" & long$day==54),3]<-5423
#
#####################################################################

#############################################
####################
#################### observed daily count plot: It should not look exponential if national counter-measures are working
####################
#############################################
#long[which(long$day==50|long$day==51),]
#colnames(long)<- c("country","day","count")

long$day<-as.numeric(long$day)

long$daily <- 0 #initialize a count variable

i<-1
# calculate  the difference in scores to model the daily increase and not the total count

while (i <= nrow(long)){
  count<-long$count[i]
  day<- long$day[i]
  cntr<-long$country[i]
  if(count > 1 & day >1){
    prev_tot<-long[which(long$country == cntr & long$day == (day-1)),][3]
    long$daily[i]<-as.numeric(count - prev_tot)
  }else{
    long$daily[i]<-as.numeric(long$count[i])
  }
  i<-i+1
}

# daily<-long[which(long$country=="Italy"),][4]
# num<-daily$daily
# den<-c(1,num)[1:54]
# long$growth<-0
# long[which(long$country=="Italy"),][5]<-growth<-num/den
# long[which(long$country=="Italy"),]

sysdate<-Sys.Date() %>% format(format="%B %d %Y")

## plot moved to the tail of the script

#############################################
####################
#################### Total count plot: It should present with a good fir with a non linear logistic model
####################
#############################################

colnames(long)<- c("country","day","count","daily")
long$day<-as.numeric(long$day)

########################################################
######  self-determination of the starts parameters. 
# This sometimes breaks or is undeterminated. 
#
#find the parameters for the equation

 predictdf<-data.frame() # this df is to rbind extrapolated values and counts for ggplot
# c<-"France"
 for (c in country){ # loops around the country selected, runs nls(), and builds the predictdf dataframe
#   
   subs<-long[which(long$country == c),]
   outbr_day <- min(which(subs$count >16)) # set a minimum of 20 cases to facilitate convergence
   subs<-subs[which(subs$day >= outbr_day),] 
   subs$absDay<-subs$day
   subs$day<-1:nrow(subs)
   print(paste(subs$day[1],subs$count[1],subs$country[1]))
#   
#   # setting outbreak day to one. Comment this to see the delay start
   outbr_day<-1 
#   
#   # estimation of start point and growth rate
#   SS<-getInitial(count~SSlogis(day,alpha,xmid,scal),data=data.frame(count=subs$count,day=subs$day)) 
#   
#   # Setting the upper asyntote (lower asyntote is zero)
#   a_start<-SS["alpha"]
#   
#   #logit(subs$count/a_start) # just a debug output
#   phi<-coef(lm(logit(subs$count/a_start)~day,data=subs)) # another way of estimating the growth rate
#   
#   #logistic model - I think is the same as the Richards Logistic Growth model
#   m<-nls(count~ a/(1+exp(-(b+g*day))), start=list(a=a_start,b=phi[1],g=phi[2]),data=subs)
#   
#   summary(m)
#   
#   #get some estimation of goodness of fit
#   cor(subs$count,predict(m))
#   
#   limit<-30 # prediction limit
#   days<-((outbr_day):limit) # number of day for plot and extrapolation
   days<-((outbr_day):nrow(subs)) # number of day for plot and extrapolation
   
   #   
#   predict<-predict(m, newdata =  data.frame(day = days)) #  extrapolation
#   cc<-rep(c, each = limit) # create the factor "country" column
    cc<-rep(c, each = nrow(subs))
#   
   count_temp<-subs$count  # prepare a column with real counts
   length(count_temp) = length(cc) # fill NA where no data is present
#  
   absDay<-subs$absDay
   length(absDay) = length(cc)
#   
#   
#   predictdf<-(rbind(predictdf,data.frame(count = count_temp,predict,day=days,country=cc, absDay=absDay))) # assemble the dataframe
   predictdf<-(rbind(predictdf,data.frame(count = count_temp,day=days,country=cc, absDay=absDay))) # assemble the dataframe
 }
#predict(m, newdata =  data.frame(day = ))

sysdate<-Sys.Date() %>% format(format="%B %d %Y")



###############################################FRANCE DELAY###############
# 
# # find nearest value for italy
# ITcounts<-long[which(long$country =="Italy"),][3]
# lower<-ITcounts[sum(ITcounts <=lastCountFR),]
# higher<-ITcounts[min(which(ITcounts>lastCountFR)),] %>% as.numeric
# 
# if (abs(lower-lastCountFR)>abs(higher-lastCountFR)){
#   eqDayITFR<-long[which(long$country =="Italy" & long$count == higher),][2] %>% as.numeric
# }else{
#   eqDayITFR<-long[which(long$country =="Italy" & long$count == lower),][2] %>% as.numeric
# }
# 
# 
# xlab<-0
# ylabFrance<-max(predictdf[which(predictdf$country  == "Italy"),][1])

###############################################################

##############################################UK DELAY###############
# # find nearest value for italy
# ITcounts<-long[which(long$country =="Italy"),][3] 
# lower<-ITcounts[sum(ITcounts <=lastCountUK),] %>% as.numeric
# higher<-ITcounts[min(which(ITcounts>lastCountUK)),] %>% as.numeric
# 
# if (abs(lower-lastCountUK)>abs(higher-lastCountUK)){
#   eqDayITUK<-long[which(long$country =="Italy" & long$count == higher),][2] %>% as.numeric
# }else{
#   eqDayITUK<-long[which(long$country =="Italy" & long$count == lower),][2] %>% as.numeric
# }
# 
# xlab<-0
# ylabUK<-ylabFrance-12000
###############################################################
lastCountFR<-max(long[which(long$country =="France"),][3]) %>% as.numeric
dayFR<-max(long[which(long$country =="France"),][2]) %>% as.numeric

lastCountUK<-max(long[which(long$country =="United Kingdom"),][3]) %>% as.numeric
dayUK<-max(long[which(long$country =="United Kingdom"),][2]) %>% as.numeric

lastCountIT<-max(long[which(long$country =="Italy"),][3]) %>% as.numeric
dayIT<-max(long[which(long$country =="Italy"),][2]) %>% as.numeric

lastCountES<-max(long[which(long$country =="Spain"),][3]) %>% as.numeric
dayES<-max(long[which(long$country =="Spain"),][2]) %>% as.numeric

lastCountDE<-max(long[which(long$country =="Germany"),][3]) %>% as.numeric
dayDE<-max(long[which(long$country =="Germany"),][2]) %>% as.numeric
################################################################
xFRlockdwn<-predictdf[which(predictdf$absDay==54 & predictdf$country == "France"),][4] %>% as.numeric
yFRlockdwn<-predictdf[which(predictdf$absDay==54 & predictdf$country == "France"),][1] %>% as.numeric

xITlockdwn<-predictdf[which(predictdf$absDay==47 & predictdf$country == "Italy"),][4] %>% as.numeric
yITlockdwn<-predictdf[which(predictdf$absDay==47 & predictdf$country == "Italy"),][1] %>% as.numeric

xESlockdwn<-predictdf[which(predictdf$absDay==52 & predictdf$country == "Spain"),][4] %>% as.numeric
yESlockdwn<-predictdf[which(predictdf$absDay==52 & predictdf$country == "Spain"),][1] %>% as.numeric

xUKlockdwn<-predictdf[which(predictdf$absDay==61 & predictdf$country == "United Kingdom"),][4] %>% as.numeric
yUKlockdwn<-predictdf[which(predictdf$absDay==61 & predictdf$country == "United Kingdom"),][1] %>% as.numeric


###################



png("images/Rplot06.png", width = 800, height = 800, units = "px")

ggplot(data = predictdf, aes(x=absDay, y=count, colour=country, breaks = 10)) +
  geom_point() +
  #geom_line(size = 1)+
  stat_smooth(aes(x=absDay, y=count, colour = country), method = lm , level = 0,formula = y ~ poly(x, 6))+
  
  scale_y_continuous(trans = "log10")+#, breaks = round(seq(0, max(predictdf$predict), len = 10),1))+ # breaks for linear y scale
  #scale_y_continuous(breaks = round(seq(0, max(predictdf$count), len = 10),1))+ # breaks for linear y scale
  #scale_x_continuous(breaks = seq(0, max(predictdf$day), by = 5))+
  xlim(min(predictdf$absDay),65)+
  ####### last count labels
  annotate("text", hjust =0, x=dayFR, y= lastCountFR, size=4, vjust=-0.2,label=lastCountFR)+
  annotate("text", hjust =0, x=dayUK, y= lastCountUK, size=4, vjust=-0.2,label=lastCountUK)+
  annotate("text", hjust =0, x=dayIT, y= lastCountIT, size=4, vjust=-0.2,label=lastCountIT)+
  annotate("text", hjust =0, x=dayES, y= lastCountES, size=4, vjust=-0.2,label=lastCountES)+
  annotate("text", hjust =0, x=dayES, y= lastCountDE, size=4, vjust=-0.2,label=lastCountDE)+
  ####### lockdown segments
  geom_segment(mapping=aes(x=xITlockdwn, xend=xITlockdwn,y=0,yend=yITlockdwn), color = "black", linetype = 9,size = .1)+
  geom_segment(mapping=aes(x=xITlockdwn, xend=Inf,y=yITlockdwn,yend=yITlockdwn), color = "black", linetype = 9,size = .1)+
  
  geom_segment(mapping=aes(x=xFRlockdwn, xend=xFRlockdwn,y=0,yend=yFRlockdwn), color = "black", linetype = 9,size = .1)+
  geom_segment(mapping=aes(x=xFRlockdwn, xend=Inf,y=yFRlockdwn,yend=yFRlockdwn), color = "black", linetype = 9,size = .1)+
  
  geom_segment(mapping=aes(x=xESlockdwn, xend=xESlockdwn,y=0,yend=yESlockdwn), color = "black", linetype = 9,size = .1)+
  geom_segment(mapping=aes(x=xESlockdwn, xend=Inf,y=yESlockdwn,yend=yESlockdwn), color = "black", linetype = 9,size = .1)+
  
  geom_segment(mapping=aes(x=xUKlockdwn, xend=xUKlockdwn,y=0,yend=yUKlockdwn), color = "black", linetype = 9,size = .1)+
  geom_segment(mapping=aes(x=xUKlockdwn, xend=Inf,y=yUKlockdwn,yend=yUKlockdwn), color = "black", linetype = 9,size = .1)+
  
  ####### lockdown counts
  annotate("text", hjust= -0.1, x=xFRlockdwn, y= yFRlockdwn, size=3, vjust=-0.4, label=yFRlockdwn) +
  annotate("text", hjust= -0.1, x=xITlockdwn, y= yITlockdwn, size=3, vjust=-0.4, label=yITlockdwn) +
  annotate("text", hjust= -0.1, x=xESlockdwn, y= yESlockdwn, size=3, vjust=-0.4, label=yESlockdwn) +
  annotate("text", hjust= -0.1, x=xUKlockdwn, y= yUKlockdwn, size=3, vjust=-0.4, label=yUKlockdwn) +
  
  ######lockdown labels
  annotate("text", hjust= 0, x=xITlockdwn, y= 0, size=4, angle=90, vjust=-0.4, label="Italy lockdown - 9 March") +
  annotate("text", hjust= 0, x=xESlockdwn, y= 0, size=4, angle=90, vjust=-0.4, label="Spain lockdown - 14 March") +
  annotate("text", hjust= 0, x=xFRlockdwn, y= 0, size=4, angle=90, vjust=-0.4, label="France lockdown - 16 March") +
  annotate("text", hjust= 0, x=xUKlockdwn, y= 0, size=4, angle=90, vjust=-0.4, label="UK lockdown - 23 March") +
  #annotate("text", hjust= 0, x=xUKlockdwn, y= 0, size=4, angle=90, vjust=-0.4, label="UK lockdown") +
  
  guides(colour = "legend", linetype = "none")+
  
  annotate("text", hjust =0, x=min(predictdf$absDay), y=lastCountIT, vjust=-0.4,size=4, label= paste("UK is 14 days behind Italy and 6 behind France"))+
  
  labs( title = "Cov-19: count of total cases registred by country (dots)",
        subtitle = "(log scale) dotted line shows lockdown dates",
        caption = paste("Updated ", sysdate, ". Data source: Johns Hopkins public dataset")
        )


dev.off()



###
## DAILY INCREASE PLOT
################################################################
ydFRlockdwn<-long[which(long$day==54 & long$country == "France"),][4] %>% as.numeric

ydITlockdwn<-long[which(long$day==47 & long$country == "Italy"),][4] %>% as.numeric

ydESlockdwn<-long[which(long$day==52 & long$country == "Spain"),][4] %>% as.numeric

png("images/daycount.png", width = 800, height = 800, units = "px")
ggplot(data = long[which(long$count>50),], aes(x=day, y=daily, colour=country)) +
  scale_y_continuous(trans = "log10")+#
  geom_point() +
  #geom_line(data = long[which(long$count>50),], aes(x=day, y=daily, colour = country))+
  stat_smooth(aes(x=day, y=daily, colour = country), method = lm, formula = y ~ poly(x, 9,raw =F), se = FALSE)+
  ####### lockdown segments
  geom_segment(mapping=aes(x=xITlockdwn, xend=xITlockdwn,y=0,yend=Inf), color = "black", linetype = 9,size = .1)+
  geom_segment(mapping=aes(x=xFRlockdwn, xend=xFRlockdwn,y=0,yend=Inf), color = "black", linetype = 9,size = .1)+
  geom_segment(mapping=aes(x=xESlockdwn, xend=xESlockdwn,y=0,yend=Inf), color = "black", linetype = 9,size = .1)+
  geom_segment(mapping=aes(x=xUKlockdwn, xend=xUKlockdwn,y=0,yend=Inf), color = "black", linetype = 9,size = .1)+
  ######lockdown labels
  annotate("text", hjust= 0, x=xITlockdwn, y= 0, size=4, angle=90, vjust=-0.4, label="Italy lockdown - 9 March") +
  annotate("text", hjust= 0, x=xESlockdwn, y= 0, size=4, angle=90, vjust=-0.4, label="Spain lockdown - 14 March") +
  annotate("text", hjust= 0, x=xFRlockdwn, y= 0, size=4, angle=90, vjust=-0.4, label="France lockdown - 16 March") +
  annotate("text", hjust= 0, x=xUKlockdwn, y= 0, size=4, angle=90, vjust=-0.4, label="UK lockdown - 23 March") +
  
  #annotate("text", hjust= 0, x=xUKlockdwn, y= 0, size=4, angle=90, vjust=-0.4, label="UK lockdown") +
  labs( title = paste("Polynomial approx. of daily new cases as per: ", sysdate),
        subtitle = "Per-day increase, y is Log.",
        caption = paste("Updated ", sysdate, ". Data source: Johns Hopkins public dataset")
  )

dev.off()

