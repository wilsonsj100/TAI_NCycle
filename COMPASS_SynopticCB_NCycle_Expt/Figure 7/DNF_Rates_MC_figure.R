
########## Look at distribution of Rates across transects ###########

#load packages 
library(fitdistrplus)
library(pracma)
library(pdqr)
library(lognorm)
library(ggplot2)
library(scales)
library(here)

#read in the data 
dat <- read.csv(here("Figure 7","DNF_Distrib_Rates.csv"))
head(dat)

dat1 <- read.csv(here("Figure 7","DNF_Distrib_Rates_wmydata.csv"))
head(dat1)

mydat <- read.csv(here("Figure 7","DNF_mydata.csv"))
head(mydat)

#subset by the transect location
Upland <-  as.data.frame(subset(dat, Transect == "Upland", select = Location:Units))
Transition <-  as.data.frame(subset(mydat, Transect == "Transition", select = Location:Units))
Wetland <-  as.data.frame(subset(dat, Transect == "Wetland", select = Location:Units))

UpQs <- as.data.frame(quantile(Upland$DNF_Rate, na.rm=TRUE))
WetQs <- as.data.frame(quantile(Wetland$DNF_Rate, na.rm=TRUE))


#plot histograms of these data 
hist(Upland$DNF_Rate)
hist(Wetland$DNF_Rate)

############# let's make box and whiskers of the compiled data and then compare ##############

#need to reorder the transect zones 
dat1$Zone <- factor(dat1$Transect)
dat1 <- subset(dat1, Zone != "Upland_Wet")
dat1$Zone <- factor(dat1$Zone , levels = c("Upland", "Transition", "Wetland"))

mydat$Zone <- factor(mydat$Transect)
mydat <- subset(mydat, Zone != "Upland_Wet")
mydat$Zone <- factor(mydat$Zone , levels = c("Upland", "Transition", "Wetland"))

compass_coolors <- c("#20063B", "#FFBC42", "#419973")

box <- ggplot()+
  geom_boxplot(data=dat1, aes(x=Zone, y=log10(DNF_Rate), fill=Zone), show.legend = FALSE) + 
  geom_point(data=mydat, aes(x=Zone, y=log10(DNF_Rate), shape=Site),  size=3,
             color="black", fill="darkgrey") + 
  theme_classic() + 
  xlab(" ") + ylab("log(Denitrification Rate)") +
  scale_fill_manual(values=compass_coolors) +
  scale_shape_manual(values=c(21, 22, 23,24 )) + 
  scale_x_discrete(labels=c("Upland", "Transition", "Wetland")) +
  theme_bw() +
  theme( legend.key = element_blank(),         # Removes the legend key background + border
    panel.background = element_rect(fill = "white"), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +  # Keeps the plot background )
    theme(panel.background = element_rect(colour = "black", linewidth  =1.2), 
          legend.position = "bottom", 
          legend.title= element_blank(), 
          legend.text = element_text(size = 12),
          axis.text=element_text(size=12), 
          axis.title=element_text(size=12)) +
  guides(shape = guide_legend(nrow = 2))

box

library(ggbreak)
box1 <- ggplot()+
  geom_boxplot(data=dat1, aes(x=Zone, y=DNF_Rate, fill=Zone)) + 
  geom_point(data=mydat, aes(x=Zone, y=DNF_Rate), shape = 21, size=3,
             color="black", fill="darkgrey") + 
  theme_classic() + 
  ylim(0,60000) + 
  #scale_y_cut(breaks=c( 60000), # change your y-axis values here
            #  which=c(1), 
             # scales=c(0.5,4))+
  xlab(" ") + ylab("Denitrification Rate (umoles N m-2 d-1)") +
  scale_fill_manual(values=compass_coolors) +
  scale_x_discrete(labels=c("Upland", "Transition", "Wetland")) +
  theme(panel.background = element_rect(colour = "black", linewidth  =1.2), 
        legend.position = "NONE", 
        legend.title= element_blank(), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) 

box1

#change from in log space to log scale y axis - used in manuscript 

box2 <- ggplot()+
  geom_boxplot(data=dat1, aes(x=Zone, y=DNF_Rate, fill=Zone), show.legend = FALSE) + 
  geom_point(data=mydat, aes(x=Zone, y=DNF_Rate, shape=Site),  size=3,
             color="black", fill="darkgrey") + 
  theme_classic() + 
  xlab(" ") + ylab("Denitrification Rate") +
  scale_fill_manual(values=compass_coolors) +
  scale_shape_manual(values=c(21, 22, 23,24 )) + 
  scale_x_discrete(labels=c("Upland", "Transition", "Wetland")) +
  scale_y_log10(
    labels = function(x) {
      # Convert numbers like 1e5 to expression with 10^5
      parse(text = gsub("e\\+?", " %*% 10^", scales::scientific_format()(x)))
    }
  ) + 
  theme_bw() +
  theme( legend.key = element_blank(),         # Removes the legend key background + border
         panel.background = element_rect(fill = "white"), 
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank()) +  # Keeps the plot background )
  theme(panel.background = element_rect(colour = "black", linewidth  =1.2), 
        legend.position = "bottom", 
        legend.title= element_blank(), 
        legend.text = element_text(size = 12),
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) +
  guides(shape = guide_legend(nrow = 2))

box2

#########################################################################################################

###################### Monte Carlo of Rates Trial ###################################

##MC function for data with less than 800 we need to boost then MC 
MC_less = function(x) {
  
  plot(ecdf(x))
  empdist=ecdf(x)
  ns=1000
  ans = as.numeric(quantile(empdist,runif(ns)))
  hist(ans,pro=T,col='wheat2')
  boost = data.frame(ans)
  boost_eq = data.frame(getParmsLognormForMoments(mean=mean(boost$ans), var=var(boost$ans), 
                                                  sigmaOrig = sqrt(var(boost$ans))))
  y= rlnorm(1000000,boost_eq$mu,boost_eq$sigma)
  hist(y)
  hist(log10(y))
  return(y)
}


#log normally distributed 
##upland 
pre_Upland = data.frame(getParmsLognormForMoments(
  mean=mean(Upland$DNF_Rate, na.rm=TRUE), var=var(Upland$DNF_Rate, na.rm=TRUE),
  sigmaOrig = sqrt(var(Upland$DNF_Rate, na.rm=TRUE))))
Upland_DNF = MC_less(Upland$DNF_Rate)

post_Upland = data.frame(t(estimateParmsLognormFromSample(Upland_DNF))) #estimate posterior
row.names(post_Upland)=c("Upland")

#wetland 
pre_Wetland = data.frame(getParmsLognormForMoments(
  mean=mean(Wetland$DNF_Rate, na.rm=TRUE), var=var(Wetland$DNF_Rate, na.rm=TRUE),
  sigmaOrig = sqrt(var(Wetland$DNF_Rate, na.rm=TRUE))))
Wetland_DNF = MC_less(Wetland$DNF_Rate)

post_Wetland = data.frame(t(estimateParmsLognormFromSample(Wetland_DNF))) #estimate posterior
row.names(post_Upland)=c("Wetland")

#my transition data 
pre_Transition = data.frame(getParmsLognormForMoments(
  mean=mean(Transition$DNF_Rate, na.rm=TRUE), var=var(Transition$DNF_Rate, na.rm=TRUE),
  sigmaOrig = sqrt(var(Transition$DNF_Rate, na.rm=TRUE))))
Transition_DNF = MC_less(Transition$DNF_Rate)

post_Transition = data.frame(t(estimateParmsLognormFromSample(Transition_DNF))) #estimate posterior
row.names(post_Transition)=c("Transition")

# parameter calculations if we need them: 
#set std. error equations
std.error <- function (x, na.rm=FALSE){
  sqrt(var(x, na.rm = na.rm) / sum(!is.na(x)))}

std.error1 <- function (x, na.rm=FALSE){
  sqrt(sd(x, na.rm = na.rm) / sum(!is.na(x)))} #gives much lower std. error

param_func = function(x){
  y = data.frame(mean(x),std(x),median(x),IQR(x),
                 quantile(x,probs=c(0.25)), quantile(x,probs=c(0.75)),
                 quantile(x,probs=c(0.1)),quantile(x,probs=c(0.9)))
  colnames(y) = c("Mean","STDV","Median","IQR", "25%", "75%", "10%","90%")
  return(y)
  
}


#now plot density diagrams of the upland and wetland 
Upland_DNF_dat <- as.data.frame(Upland_DNF)
Wetland_DNF_dat <- as.data.frame(Wetland_DNF)
Transition_DNF_dat <- as.data.frame(Transition_DNF)
head(Upland_DNF_dat)
###########################################################################################################


################ Denisty Figure  ####################
DNF_dens = 
  ggplot()+
  geom_density(data = Upland_DNF_dat,aes(Upland_DNF,fill="Upland",
                                         color="Upland"),alpha=0.5,linewidth=2)+
  geom_density(data = Wetland_DNF_dat,aes(Wetland_DNF,fill="Wetland",
                                          color="Wetland"),alpha=0.5,linewidth=2)+
  geom_density(data = Transition_DNF_dat,aes(Transition_DNF,fill="Transition",
                                             color="Transition"),alpha=0.5,linewidth=2)+
  xlab(expression(paste(Rate ( d^-1))))+
  ylab(expression(Density))+
  #scale_y_continuous(expand = c(0,0), labels = scales::scientific)+
  xlim(0, 50000)+
  theme_bw()+
  theme(axis.title =element_text(size=12),
        axis.text.x.bottom  = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) +
  #aspect.ratio = 1)+
  ggtitle(" ") + 
  scale_fill_manual(values = c('Upland'='#20063B', 'Wetland'='#419973', 'Transition'='#FFBC42'))+
  scale_color_manual(values = c('Upland'='#20063B',
                                'Wetland'='#419973', 'Transition'='#FFBC42'),guide = "none")  
DNF_dens


#log space
log_DNF_dens = 
  ggplot()+
  geom_density(data = Upland_DNF_dat,aes(log(Upland_DNF),fill="Upland",
                                           color="Upland"),alpha=0.5,linewidth=2)+

  geom_density(data = Transition_DNF_dat,aes(log(Transition_DNF),fill="Transition",
                                          color="Transition"),alpha=0.5,linewidth=2)+
  geom_density(data = Wetland_DNF_dat,aes(log(Wetland_DNF),fill="Wetland",
                                          color="Wetland"),alpha=0.5,linewidth=2)+
  #xlab(expression(paste(Log (Denitrification Rate))))+
  xlab("log(Denitrification Rate)") +
  ylab(expression(Density))+
  #scale_y_continuous(expand = c(0,0), labels = scales::scientific)+
  xlim(7, 18)+
  theme_bw()+
  theme(axis.title =element_text(size=12),
        axis.text.x.bottom  = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) +
  #aspect.ratio = 1)+
  ggtitle(" ") + 
  scale_fill_manual(values = c('Upland'='#20063B', 'Wetland'='#419973', 'Transition'='#FFBC42'))+
  scale_color_manual(values = c('Upland'='#20063B',
                                'Wetland'='#419973', 'Transition'='#FFBC42'),guide = "none")  
log_DNF_dens

###########################################################################################################
