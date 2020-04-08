require(reshape2)
require(ggplot2)

#####################

country = c("Italy")

####################


######################## BEGIN DATA PARSING for Confirmed cases

sysdate<-Sys.Date() %>% format(format="%B %d %Y")
#data<-read.csv(file = "~/Windows/time_series_19-covid-Confirmed.csv",header = TRUE,sep = ",")
link<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
data<-read.csv(file = link,header = TRUE,sep = ",")

data<-data %>% select(1:2,5:ncol(data))

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


colnames(long)<- c("country","day","count")
long$day<-as.numeric(long$day)



######################## END DATA PARSING for Confirmed cases

sirdDF<-long # appending to final df for analysis

######################## BEGIN DATA PARSING for Recovered cases

link<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
data<-read.csv(file = link,header = TRUE,sep = ",")

data<-data %>% select(1:2,5:ncol(data))

# the csv has inconsistent ways to name UK ( AS USUALLY happens with Great britain, UK, United Kingdom)
levels(data$Province.State) <- c(levels(data$Province.State),"United Kingdom")
data[data=="UK"]<-as.character("United Kingdom") 

# Subsetting
subset<-data %>% filter_all(all_vars(Country.Region %in% country))
subset<-subset %>% filter_all(all_vars(Province.State %in% c(country,"")))
subset<- subset %>% select(2:ncol(data))


names(subset) <- c("Country.Region",paste("day.",(1:(ncol(subset)-1)),sep = ""))


long <- melt(subset, time.var= subset(2:ncol(subset)),id.vars = c("Country.Region"))
long[51,3] <-1258 # csv is wrong
long[55,3]<-5129

long$Country.Region<- droplevels(long$Country.Region)


colnames(long)<- c("country","day","count")
long$day<-as.numeric(long$day)

######################## END DATA PARSING for Recovered cases

sirdDF$recovered<-long$count

######################## BEGIN DATA PARSING for deaths

link<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
data<-read.csv(file = link,header = TRUE,sep = ",")

data<-data %>% select(1:2,5:ncol(data))

# the csv has inconsistent ways to name UK ( AS USUALLY happens with Great britain, UK, United Kingdom)
levels(data$Province.State) <- c(levels(data$Province.State),"United Kingdom")
data[data=="UK"]<-as.character("United Kingdom") 

# Subsetting
subset<-data %>% filter_all(all_vars(Country.Region %in% country))
subset<-subset %>% filter_all(all_vars(Province.State %in% c(country,"")))
subset<- subset %>% select(2:ncol(data))


names(subset) <- c("Country.Region",paste("day.",(1:(ncol(subset)-1)),sep = ""))


long <- melt(subset, time.var= subset(2:ncol(subset)),id.vars = c("Country.Region"))
long[51,3] <-1016 # csv is wrong

long$Country.Region<- droplevels(long$Country.Region)


colnames(long)<- c("country","day","count")
long$day<-as.numeric(long$day)

######################## BEGIN DATA PARSING for deaths

sirdDF$deaths<-long$count





#SIRD Model of Disease Transmission: beta = infection rate, gamma = recovery rate, mu = death rate

S=60000000 # Susceptible;             deltaS = S_t+1 - S_t = -betaI_tS_t
I=15   # Infected;                    deltaI = betaI_tS_t - gammaI_t -muI_t 
R=0    # Recovered;                   deltaR = gammaIt
D=0    # Dead                         deltaD = muI_t
#
#        
#


beta=.0005   # beta  = infection rate
gamma=.05    # gamma = recovery rate
mu=.02       # mu    = death rate
nreps=100
#Create History Dataframe
history = data.frame(time=0, S=S,I=I,R=R,D=D);
#Loop over step function
for(time in 1:nreps){
  newInf = pmin(S, floor(beta*I*S))
  newRec = pmin(I, floor(gamma*I))
  S = S - newInf
  I = I + newInf - newRec
  R = R + newRec
  newDead = pmin(I, floor(mu*I))
  I = I- newDead
  D = D + newDead
  history = rbind(history, data.frame(time, S, I, R, D))
}
#And finally plot
plotdat = melt(history, id.vars = c("time"))
ggplot(data=plotdat)+
  aes(x=time, y=value, color=variable)+
  geom_line(size=2)+
  theme_set(theme_gray(base_size = 15))+
  xlab("Time Step")+ylab("# Indv.")+
  ggtitle(paste("SIRD Epidemic Dynamics\nβ=",beta,"; γ=",gamma,"; μ=",mu,"\n", sep=""))+
  scale_color_manual(name="Disease State", values=c("blue", "orange", "green", "red"))
