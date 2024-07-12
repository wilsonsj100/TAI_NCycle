
########## Look at distribution of Rates across the transect ###########

#load packages 
library(fitdistrplus)
library(pracma)
library(pdqr)
library(lognorm)
library(ggplot2)
library(scales)

#read in the data 
dat <- read.csv("DNF_Distrib_Rates.csv")
head(dat)

dat1 <- read.csv("DNF_Distrib_Rates_wmydata.csv")
head(dat1)

mydat <- read.csv("DNF_mydata.csv")
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
  geom_boxplot(data=dat1, aes(x=Zone, y=log10(DNF_Rate), fill=Zone)) + 
  geom_point(data=mydat, aes(x=Zone, y=log10(DNF_Rate)), shape = 21, size=3,
             color="black", fill="darkgrey") + 
  theme_classic() + 
  xlab(" ") + ylab("log( Denitrification Rate )") +
  scale_fill_manual(values=compass_coolors) +
  scale_x_discrete(labels=c("Upland", "Transition", "Wetland")) +
    theme(panel.background = element_rect(colour = "black", linewidth  =1.2), 
          legend.position = "NONE", 
          legend.title= element_blank(), 
          axis.text=element_text(size=12), 
          axis.title=element_text(size=12)) 

box

library(ggbreak)
box1 <- ggplot()+
  geom_boxplot(data=dat1, aes(x=Zone, y=DNF_Rate, fill=Zone)) + 
  geom_point(data=mydat, aes(x=Zone, y=DNF_Rate), shape = 21, size=3,
             color="black", fill="darkgrey") + 
  theme_classic() + 
  #ylim(0,65000) + 
  scale_y_cut(breaks=c( 60000), # change your y-axis values here
              which=c(1), 
              scales=c(0.5,4))+
  xlab(" ") + ylab("Denitrification Rate (nmoles N g-1 hr-1)") +
  scale_fill_manual(values=compass_coolors) +
  scale_x_discrete(labels=c("Upland", "Transition", "Wetland")) +
  theme(panel.background = element_rect(colour = "black", linewidth  =1.2), 
        legend.position = "NONE", 
        legend.title= element_blank(), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) 

box1
#########################################################################################################
