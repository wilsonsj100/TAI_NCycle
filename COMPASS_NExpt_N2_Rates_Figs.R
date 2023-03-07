##################################
################## INFO #######################
####### COMPASS Synoptic CB 
####### Nitrogen Experiment 2022
####### Rate Calculation & Data Visualization
####### Stephanie J. Wilson 
####### Edited: 10-04-2022
##############################################


############### SET UP #######################
#packages
library(ggplot2)
library(data.table)
library(tidyr)
library(dplyr)
library(ggpubr)
library(car)

#set wd for data read in 
setwd("S:/Biogeochemistry/People/Wilson (Steph)/Compass N Cycle Experiment/Full Incub. Experiment/Data/IRMS Data/Processed Data")

#read in data
dat <- read.csv("COMPASS_NExpt_N2_Data_Final.csv")
head(dat)

#reset wd for figure & file outputs 
setwd("S:/Biogeochemistry/People/Wilson (Steph)/Compass N Cycle Experiment/Full Incub. Experiment/Data/IRMS Data/Rates & Figures")

compass_coolors1 <- c("#777469","#CFB863", "#3B5D32") 
compass_coolors2 <- c("#4D4847","#1C3738", "#000F08") 
compass_coolors2a <- c("#000F08","#1C3738", "#8BAAAD") 
compass_coolors2b <- c("#00562E","#3A7173", "#9BB6B8") 
compass_coolors3 <- c("#020402","#B3B752", "#2C3F27") 

##############################################

######### Sample dataframe set up ############

#change column names 
colnames(dat) <- c('ID', 'Hours','N2_29','N2_30', "d15N")
head(dat)

dat$ID <- as.character(dat$ID)

#make new sample id columns from the ID 
dat1 <- dat %>% separate(ID, 
                c("Site", "Zone","Replicate", "Time_Point", "Analytical_Rep"))

head(dat1)

dat1$N2N = (dat$N2_29 + (2  *dat$N2_30))
head(dat1)

##############################################

######### Subset by site & zones #############

#Subset out GCrew Samples: 
G1 <- subset(dat1, Site=="Gcrew")
head(G1)

G1UP <- subset(G1, Zone=="UP")
G1TR <- subset(G1, Zone=="TR")
G1WC <- subset(G1, Zone=="WC")

#Subset out MSM Samples: 
M1 <- subset(dat1, Site=="MSM")
head(M1)

M1UP <- subset(M1, Zone=="UP")
M1TR <- subset(M1, Zone=="TR")
M1WC <- subset(M1, Zone=="WC")

#Subset out GWI Samples: 
GW1 <- subset(dat1, Site=="GWI")
head(GW1)

GW1UP <- subset(GW1, Zone=="UP")
GW1TR <- subset(GW1, Zone=="TR")
GW1WC <- subset(GW1, Zone=="WC")

#Subset out GCrew Samples: 
S1 <- subset(dat1, Site=="SWH")
head(S1)

S1UP <- subset(S1, Zone=="UP")
S1TR <- subset(S1, Zone=="TR")
S1WC <- subset(S1, Zone=="WC")
##############################################

####### Plot N2 over time & calc rates: GCREW #######
#GCrew N2 Production Figures 
Pg1up <- ggplot(G1UP, aes(x=Hours, y=N2N, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles N2-N") +
  ggtitle("(a) GCReW Upland") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        legend.position = c(0.15, 0.8), 
        legend.title= element_blank()) +
 # theme(plot.title = element_text(margin = margin(t = 10, b = -20))) + 
  ylim(0,100) +
  scale_color_manual(values=c("black",'gray38','gray68'))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
Pg1up

Pg1tr <- ggplot(G1TR, aes(x=Hours, y=N2N, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles N2-N") + 
  ggtitle("(b) GCReW Transition") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        legend.position = c(0.15, 0.8), 
        legend.title= element_blank()) +  
  ylim(0,100) +
  xlim(0,10) + 
  scale_color_manual(values=c("black",'gray38','gray68'))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
Pg1tr

Pg1wc <- ggplot(G1WC, aes(x=Hours, y=N2N, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles N2-N") + 
  ggtitle("(c) GCReW Wetland") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        legend.position = c(0.15, 0.8), 
        legend.title= element_blank()) +  
  ylim(0,100) +
  xlim(0,10) +
  scale_color_manual(values=c("black",'gray38','gray68'))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
Pg1wc

#plot all three figures in a line
GcrewRaw <- ggarrange(Pg1up, Pg1tr, Pg1wc, ncol=3, nrow=1)

pdf("GCrew_Raw_N2N_overtime.pdf", width=12,height=4)
GcrewRaw
dev.off()

#Now calculate GCrew Lm for slope of these three lines 
G1TRa <- subset(G1TR, Replicate=="CoreA")
G1TRb <- subset(G1TR, Replicate=="CoreB")
G1TRc <- subset(G1TR, Replicate=="CoreC")

G1UPa <- subset(G1UP, Replicate=="CoreA")
G1UPb <- subset(G1UP, Replicate=="CoreB")
G1UPc <- subset(G1UP, Replicate=="CoreC")

G1WCa <- subset(G1WC, Replicate=="CoreA")
G1WCb <- subset(G1WC, Replicate=="CoreB")
G1WCc <- subset(G1WC, Replicate=="CoreC")

lmGTRa <- lm(N2N ~ Hours, data=G1TRa)
lmGTRb <- lm(N2N ~ Hours, data=G1TRb)
lmGTRc <- lm(N2N ~ Hours, data=G1TRc)

lmGUPa <- lm(N2N ~ Hours, data=G1UPa)
lmGUPb <- lm(N2N ~ Hours, data=G1UPb)
lmGUPc <- lm(N2N ~ Hours, data=G1UPc)

lmGWCa <- lm(N2N ~ Hours, data=G1WCa)
lmGWCb <- lm(N2N ~ Hours, data=G1WCb)
lmGWCc <- lm(N2N ~ Hours, data=G1WCc)

#extract slopes: 
Gslopes <- as.data.frame(c(summary(lmGTRa)$coefficients[2,1], 
                           summary(lmGTRb)$coefficients[2,1],
                           summary(lmGTRc)$coefficients[2,1], 
                           summary(lmGUPa)$coefficients[2,1], 
                           summary(lmGUPb)$coefficients[2,1],
                           summary(lmGUPc)$coefficients[2,1], 
                           summary(lmGWCa)$coefficients[2,1], 
                           summary(lmGWCb)$coefficients[2,1],
                           summary(lmGWCc)$coefficients[2,1]))
colnames(Gslopes) <- c('N2_Rate_g_hr')
head(Gslopes)



########################################################

####### Plot N2 over time & calc rates: MSM #######
#GCrew N2 Production Figures 
Pm1up <- ggplot(M1UP, aes(x=Hours, y=N2N, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles N2-N") +
  ggtitle("(d) MoneyStump Swamp Upland") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        legend.position = c(0.15, 0.8), 
        legend.title= element_blank()) +
  # theme(plot.title = element_text(margin = margin(t = 10, b = -20))) + 
  ylim(0,150) +
  scale_color_manual(values=c("black",'gray38','gray68'))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
Pm1up

Pm1tr <- ggplot(M1TR, aes(x=Hours, y=N2N, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles N2-N") + 
  ggtitle("(e) MoneyStump Swamp Transition") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        legend.position = c(0.15, 0.8), 
        legend.title= element_blank()) +  
  ylim(0,150) +
  xlim(0,10) + 
  scale_color_manual(values=c("black",'gray38','gray68'))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
Pm1tr

Pm1wc <- ggplot(M1WC, aes(x=Hours, y=N2N, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles N2-N") + 
  ggtitle("(f) MoneyStump Swamp Wetland") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        legend.position = c(0.15, 0.8), 
        legend.title= element_blank()) +  
  ylim(0,150) +
  xlim(0,10) +
  scale_color_manual(values=c("black",'gray38','gray68'))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
Pm1wc

#plot all three figures in a line
MSMRaw <- ggarrange(Pm1up, Pm1tr, Pm1wc, ncol=3, nrow=1)

pdf("MSM_Raw_N2N_overtime.pdf", width=12,height=4)
MSMRaw
dev.off()

#Now calculate GCrew Lm for slope of these three lines 
M1UPa <- subset(M1UP, Replicate=="CoreA")
M1UPb <- subset(M1UP, Replicate=="CoreB")
M1UPc <- subset(M1UP, Replicate=="CoreC")

M1TRa <- subset(M1TR, Replicate=="CoreA")
M1TRb <- subset(M1TR, Replicate=="CoreB")
M1TRc <- subset(M1TR, Replicate=="CoreC")

M1WCa <- subset(M1WC, Replicate=="CoreA")
M1WCb <- subset(M1WC, Replicate=="CoreB")
M1WCc <- subset(M1WC, Replicate=="CoreC")

lmMUPa <- lm(N2N ~ Hours, data=M1UPa)
lmMUPb <- lm(N2N ~ Hours, data=M1UPb)
lmMUPc <- lm(N2N ~ Hours, data=M1UPc)

lmMTRa <- lm(N2N ~ Hours, data=M1TRa)
lmMTRb <- lm(N2N ~ Hours, data=M1TRb)
lmMTRc <- lm(N2N ~ Hours, data=M1TRc)

lmMWCa <- lm(N2N ~ Hours, data=M1WCa)
lmMWCb <- lm(N2N ~ Hours, data=M1WCb)
lmMWCc <- lm(N2N ~ Hours, data=M1WCc)

#extract slopes: 
Mslopes <- as.data.frame(c(summary(lmMTRa)$coefficients[2,1], 
                           summary(lmMTRb)$coefficients[2,1],
                           summary(lmMTRc)$coefficients[2,1], 
                           summary(lmMUPa)$coefficients[2,1], 
                           summary(lmMUPb)$coefficients[2,1],
                           summary(lmMUPc)$coefficients[2,1], 
                           summary(lmMWCa)$coefficients[2,1], 
                           summary(lmMWCb)$coefficients[2,1],
                           summary(lmMWCc)$coefficients[2,1]))
colnames(Mslopes) <- c('N2_Rate_g_hr')
head(Mslopes)

########################################################

####### Plot N2 over time & calc rates: GWI #######
#GCrew N2 Production Figures 
Pgw1up <- ggplot(GW1UP, aes(x=Hours, y=N2N, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles N2-N") +
  ggtitle("(g) Goodwin Islands Upland") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        legend.position = c(0.15, 0.8), 
        legend.title= element_blank()) +
  # theme(plot.title = element_text(margin = margin(t = 10, b = -20))) + 
  ylim(0,100) +
  scale_color_manual(values=c("black",'gray38','gray68'))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
Pgw1up

Pgw1tr <- ggplot(GW1TR, aes(x=Hours, y=N2N, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles N2-N") + 
  ggtitle("(h) Goodwin Islands Transition") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        legend.position = c(0.15, 0.8), 
        legend.title= element_blank()) +  
  ylim(0,100) +
  xlim(0,10) + 
  scale_color_manual(values=c("black",'gray38','gray68'))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
Pgw1tr

Pgw1wc <- ggplot(GW1WC, aes(x=Hours, y=N2N, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles N2-N") + 
  ggtitle("(i) Goodwin Islands Wetland") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        legend.position = c(0.15, 0.8), 
        legend.title= element_blank()) +  
  ylim(0,100) +
  xlim(0,10) +
  scale_color_manual(values=c("black",'gray38','gray68'))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
Pgw1wc

#plot all three figures in a line
GWIRaw <- ggarrange(Pgw1up, Pgw1tr, Pgw1wc, ncol=3, nrow=1)

pdf("GWI_Raw_N2N_overtime.pdf", width=12,height=4)
GWIRaw
dev.off()

#Now calculate GWI Lm for slope of these three lines 
Gw1UPa <- subset(GW1UP, Replicate=="CoreA")
Gw1UPb <- subset(GW1UP, Replicate=="CoreB")
Gw1UPc <- subset(GW1UP, Replicate=="CoreC")

Gw1TRa <- subset(GW1TR, Replicate=="CoreA")
Gw1TRb <- subset(GW1TR, Replicate=="CoreB")
Gw1TRc <- subset(GW1TR, Replicate=="CoreC")

Gw1WCa <- subset(GW1WC, Replicate=="CoreA")
Gw1WCb <- subset(GW1WC, Replicate=="CoreB")
Gw1WCc <- subset(GW1WC, Replicate=="CoreC")

lmGwUPa <- lm(N2N ~ Hours, data=Gw1UPa)
lmGwUPb <- lm(N2N ~ Hours, data=Gw1UPb)
lmGwUPc <- lm(N2N ~ Hours, data=Gw1UPc)

lmGwTRa <- lm(N2N ~ Hours, data=Gw1TRa)
lmGwTRb <- lm(N2N ~ Hours, data=Gw1TRb)
lmGwTRc <- lm(N2N ~ Hours, data=Gw1TRc)

lmGwWCa <- lm(N2N ~ Hours, data=Gw1WCa)
lmGwWCb <- lm(N2N ~ Hours, data=Gw1WCb)
lmGwWCc <- lm(N2N ~ Hours, data=Gw1WCc)

#extract slopes: 
Gwslopes <- as.data.frame(c(summary(lmGwTRa)$coefficients[2,1], 
                            summary(lmGwTRb)$coefficients[2,1],
                            summary(lmGwTRc)$coefficients[2,1],
                            summary(lmGwUPa)$coefficients[2,1], 
                           summary(lmGwUPb)$coefficients[2,1],
                           summary(lmGwUPc)$coefficients[2,1], 
                           summary(lmGwWCa)$coefficients[2,1], 
                           summary(lmGwWCb)$coefficients[2,1],
                           summary(lmGwWCc)$coefficients[2,1]))
colnames(Gwslopes) <- c('N2_Rate_g_hr')
head(Gwslopes)

########################################################

####### Plot N2 over time & calc rates: SWH #######
#GCrew N2 Production Figures 
Ps1up <- ggplot(S1UP, aes(x=Hours, y=N2N, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles N2-N") +
  ggtitle("(j) Sweet Hall Marsh Upland") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        legend.position = c(0.15, 0.8), 
        legend.title= element_blank()) +
  # theme(plot.title = element_text(margin = margin(t = 10, b = -20))) + 
  ylim(0,100) +
  scale_color_manual(values=c("black",'gray38','gray68'))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
Ps1up

Ps1tr <- ggplot(S1TR, aes(x=Hours, y=N2N, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles N2-N") + 
  ggtitle("(k) Sweet Hall Marsh Transition") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        legend.position = c(0.15, 0.8), 
        legend.title= element_blank()) +  
  ylim(0,100) +
  xlim(0,10) + 
  scale_color_manual(values=c("black",'gray38','gray68'))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
Ps1tr

Ps1wc <- ggplot(S1WC, aes(x=Hours, y=N2N, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles N2-N") + 
  ggtitle("(l) Sweet Hall Marsh Wetland") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        legend.position = c(0.15, 0.8), 
        legend.title= element_blank()) +  
  ylim(0,100) +
  xlim(0,10) +
  scale_color_manual(values=c("black",'gray38','gray68'))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
Ps1wc

#plot all three figures in a line
SWHRaw <- ggarrange(Ps1up, Ps1tr, Ps1wc, ncol=3, nrow=1)

pdf("SWH_Raw_N2N_overtime.pdf", width=12,height=4)
SWHRaw
dev.off()

#Now calculate GCrew Lm for slope of these three lines 
S1UPa <- subset(S1UP, Replicate=="CoreA")
S1UPb <- subset(S1UP, Replicate=="CoreB")
S1UPc <- subset(S1UP, Replicate=="CoreC")

S1TRa <- subset(S1TR, Replicate=="CoreA")
S1TRb <- subset(S1TR, Replicate=="CoreB")
S1TRc <- subset(S1TR, Replicate=="CoreC")

S1WCa <- subset(S1WC, Replicate=="CoreA")
S1WCb <- subset(S1WC, Replicate=="CoreB")
S1WCc <- subset(S1WC, Replicate=="CoreC")

lmSUPa <- lm(N2N ~ Hours, data=S1UPa)
lmSUPb <- lm(N2N ~ Hours, data=S1UPb)
lmSUPc <- lm(N2N ~ Hours, data=S1UPc)

lmSTRa <- lm(N2N ~ Hours, data=S1TRa)
lmSTRb <- lm(N2N ~ Hours, data=S1TRb)
lmSTRc <- lm(N2N ~ Hours, data=S1TRc)

lmSWCa <- lm(N2N ~ Hours, data=S1WCa)
lmSWCb <- lm(N2N ~ Hours, data=S1WCb)
lmSWCc <- lm(N2N ~ Hours, data=S1WCc)

#extract slopes: 
Sslopes <- as.data.frame(c(summary(lmSTRa)$coefficients[2,1], 
                           summary(lmSTRb)$coefficients[2,1],
                           summary(lmSTRc)$coefficients[2,1], 
                           summary(lmSUPa)$coefficients[2,1], 
                           summary(lmSUPb)$coefficients[2,1],
                           summary(lmSUPc)$coefficients[2,1], 
                           summary(lmSWCa)$coefficients[2,1], 
                           summary(lmSWCb)$coefficients[2,1],
                           summary(lmSWCc)$coefficients[2,1]))
colnames(Sslopes) <- c('N2_Rate_g_hr')
head(Sslopes)

########################################################

############ Plot all N2-N overtime in one graph ########################

#plot all three figures in a line
N2RatesRaw <- ggarrange(Pg1up, Pg1tr, Pg1wc, Pm1up, Pm1tr, Pm1wc,
                      Pgw1up, Pgw1tr, Pgw1wc, Ps1up, Ps1tr, Ps1wc,
                      ncol=3, nrow=4, common.legend = TRUE, legend="bottom")

pdf("Raw_N2N_overtime.pdf", width=10,height=12)
N2RatesRaw
dev.off()

############################################################

################ Make new data frame ######################
Gavg <- G1 %>% 
  group_by(Site, Zone, Replicate) %>% 
  summarise(mean = mean(N2N, na.rm=TRUE))
head(Gavg)

GRates <- as.data.frame(cbind(Gavg$Site, Gavg$Zone, Gavg$Replicate, Gslopes$N2_Rate_g_hr))
colnames(GRates) <- c("Site", "Zone", "Replicate", 'N2_Rate_g_hr')
GRates$N2_Rate_g_hr <- as.numeric(GRates$N2_Rate_g_hr)
head(GRates)

Mavg <- M1 %>% 
  group_by(Site, Zone, Replicate) %>% 
  summarise(mean = mean(N2N, na.rm=TRUE))
head(Mavg)

MRates <- as.data.frame(cbind(Mavg$Site, Mavg$Zone, Mavg$Replicate, Mslopes$N2_Rate_g_hr))
colnames(MRates) <- c("Site", "Zone", "Replicate", 'N2_Rate_g_hr')
MRates$N2_Rate_g_hr <- as.numeric(MRates$N2_Rate_g_hr)
head(MRates)

GWavg <- GW1 %>% 
  group_by(Site, Zone, Replicate) %>% 
  summarise(mean = mean(N2N, na.rm=TRUE))
head(GWavg)

GWRates <- as.data.frame(cbind(GWavg$Site, GWavg$Zone, GWavg$Replicate, Gwslopes$N2_Rate_g_hr))
colnames(GWRates) <- c("Site", "Zone", "Replicate", 'N2_Rate_g_hr')
GWRates$N2_Rate_g_hr <- as.numeric(GWRates$N2_Rate_g_hr)
head(GWRates)

Savg <- S1 %>% 
  group_by(Site, Zone, Replicate) %>% 
  summarise(mean = mean(N2N, na.rm=TRUE))
head(Savg)

SRates <- as.data.frame(cbind(Savg$Site, Savg$Zone, Savg$Replicate, Sslopes$N2_Rate_g_hr))
colnames(SRates) <- c("Site", "Zone", "Replicate", 'N2_Rate_g_hr')
SRates$N2_Rate_g_hr <- as.numeric(SRates$N2_Rate_g_hr)
head(SRates)

Rates <- as.data.frame(rbind(GRates, MRates, GWRates, SRates))
head(Rates)

Rates$N2_Rate_g_hr <- as.numeric(Rates$N2_Rate_g_hr)

write.csv(Rates, file="DNF_Rates_RCalc.csv")

#############################################

############### Check Data Normality & Variances ##################
hist(Rates$N2_Rate_g_hr)    

Rates$LogN2 <- log(Rates$N2_Rate_g_hr)
hist(Rates$LogN2)

Rates$sqrt <- sqrt(Rates$N2_Rate_g_hr)
hist(Rates$sqrt)


### Try without log adjust 
qqnorm(Rates$N2_Rate_g_hr) #the data can be negative technically so they can be normal 
qqline(Rates$N2_Rate_g_hr)

library(ggpubr)
ggqqplot(Rates$N2_Rate_g_hr)

shapiro.test(Rates$N2_Rate_g_hr) #pval less than 0.5 so not normal 

ks.test(Rates$N2_Rate_g_hr, "pnorm") #pval is less than 0.5 so the data are not normal 

#All 
#do a variance test to check homogeneity of variance 
bartlett.test(N2_Rate_g_hr ~ Zone, data = Rates)               # p=0.2363
# Levene's test with one independent variable
leveneTest(N2_Rate_g_hr ~ Zone, data = Rates)                 #  p=0.4497

#GCReW
#do a variance test to check homogeneity of variance 
bartlett.test(N2_Rate_g_hr ~ Zone, data = GRates)               # p=0.0151
# Levene's test with one independent variable
leveneTest(N2_Rate_g_hr ~ Zone, data = GRates)                 #  p=0.1622

#MSM
#do a variance test to check homogeneity of variance 
bartlett.test(N2_Rate_g_hr ~ Zone, data = MRates)        # p=0.3105
# Levene's test with one independent variable
leveneTest(N2_Rate_g_hr ~ Zone, data = MRates)          #  p=0.4738

#GWI 
#do a variance test to check homogeneity of variance 
bartlett.test(N2_Rate_g_hr ~ Zone, data = GWRates)             # p=0.01284
# Levene's test with one independent variable
leveneTest(N2_Rate_g_hr ~ Zone, data = GWRates)               # p=0.5543

#SWH
#do a variance test to check homogeneity of variance 
bartlett.test(N2_Rate_g_hr ~ Zone, data = SRates)            # p=0.0293
# Levene's test with one independent variable
leveneTest(N2_Rate_g_hr ~ Zone, data = SRates)               # p=0.4334




########## check with log transformed data  ############
qqnorm(Rates$LogN2) #technically the data cannot be zero so they are log normal
qqline(Rates$LogN2)

library("ggpubr")
ggqqplot(Rates$LogN2)
ggdensity(Rates$LogN2, 
          main = "Density plot of Denitrification Rates",
          xlab = "Denitrification Rate")

#GCReW
GRates$LogN2 <- log(GRates$N2_Rate_g_hr)
#do a variance test to check homogeneity of variance 
bartlett.test(LogN2 ~ Zone, data = GRates)               # p=0.1085
# Levene's test with one independent variable
leveneTest(LogN2 ~ Zone, data = GRates)                 #  p=0.5892

#MSM
MRates$LogN2 <- log(MRates$N2_Rate_g_hr)
#do a variance test to check homogeneity of variance 
bartlett.test(LogN2 ~ Zone, data = MRates)        # p=0.1151
# Levene's test with one independent variable
leveneTest(LogN2 ~ Zone, data = MRates)          #  p=0.4863

#GWI 
GWRates$LogN2 <- log(GWRates$N2_Rate_g_hr)
#do a variance test to check homogeneity of variance 
bartlett.test(LogN2 ~ Zone, data = GWRates)             # p=0.5727
# Levene's test with one independent variable
leveneTest(LogN2 ~ Zone, data = GWRates)               # p=0.6806

#SWH
SRates$LogN2 <- log(SRates$N2_Rate_g_hr)
#do a variance test to check homogeneity of variance 
bartlett.test(LogN2 ~ Zone, data = SRates)            # p=0.009387
# Levene's test with one independent variable
leveneTest(LogN2 ~ Zone, data = SRates)               # p=0.3488

#######################################################

### I think we need to do a kruskal wallace test for non-parametric data 
# because the rate data are not normal 

############### ANOVA to test Zone with non transformed data ####################

#All data 
#kruskal wallis test on non-parametric data
kruskal.test(N2_Rate_g_hr ~ Zone, data = Rates)     # pval = 0.0001653 significant 

pairwise.wilcox.test(Rates$N2_Rate_g_hr, Rates$Zone,
                     p.adjust.method = "BH")


#GCrew
Gres <- aov(N2_Rate_g_hr ~ Zone, data = GRates)
summary(Gres)                                         # Zone: p=0.0011
TukeyHSD(Gres)                                        # UP-TR Sig; WC-TR Sig; WC-UP not
tukey.plot.test<-TukeyHSD(Gres)
plot(tukey.plot.test, las = 1)

#kruskal wallis test on non-parametric data
kruskal.test(N2_Rate_g_hr ~ Zone, data = GRates)     # pval = 0.03899 significant 

pairwise.wilcox.test(GRates$N2_Rate_g_hr, GRates$Zone,
                     p.adjust.method = "BH")

#MSM
Mres <- aov(N2_Rate_g_hr ~ Zone, data = MRates)
summary(Mres)                                         # Zone: p=0.00178
TukeyHSD(Mres)                                        # UP-TR Sig; WC-TR Sig; WC-UP not
tukey.plot.test<-TukeyHSD(Gres)
plot(tukey.plot.test, las = 1)

#kruskal wallis test on non-parametric data
kruskal.test(N2_Rate_g_hr ~ Zone, data = MRates)     # pval = 0.06646 not significant 

pairwise.wilcox.test(MRates$N2_Rate_g_hr, MRates$Zone,
                     p.adjust.method = "BH")

#GWI
GWres <- aov(N2_Rate_g_hr ~ Zone, data = GWRates)
summary(GWres)                                         # Zone: p=0.00199
TukeyHSD(GWres)                                        # UP-TR Sig; WC-TR NOT; WC-UP sig
tukey.plot.test<-TukeyHSD(GWres)
plot(tukey.plot.test, las = 1)

#kruskal wallis test on non-parametric data
kruskal.test(N2_Rate_g_hr ~ Zone, data = GWRates)     # pval = 0.05091 not significant 

pairwise.wilcox.test(GWRates$N2_Rate_g_hr, GWRates$Zone,
                     p.adjust.method = "BH")

#SWH
Sres <- aov(N2_Rate_g_hr ~ Zone, data = SRates)
summary(Sres)                                         # Zone: p=0.0189
TukeyHSD(Sres)                                        # UP-TR Not; WC-TR Sig; WC-UP not
tukey.plot.test<-TukeyHSD(Sres)
plot(tukey.plot.test, las = 1)

#kruskal wallis test on non-parametric data
kruskal.test(N2_Rate_g_hr ~ Zone, data = SRates)     # pval = 0.02732  significant 

pairwise.wilcox.test(SRates$N2_Rate_g_hr, SRates$Zone,
                     p.adjust.method = "BH")


######################################################

############### ANOVA to test Zone with log transformed data ####################

#GCrew
Gres <- aov(LogN2 ~ Zone, data = GRates)
summary(Gres)                                         # Zone: p=0.0222
TukeyHSD(Gres)                                        # UP-TR Sig; WC-TR not; WC-UP not
tukey.plot.test<-TukeyHSD(Gres)
plot(tukey.plot.test, las = 1)

#kruskal wallis test on non-parametric data
kruskal.test(LogN2 ~ Zone, data = GRates)

#MSM
Mres <- aov(LogN2 ~ Zone, data = MRates)
summary(Mres)                                         # Zone: p=0.0786
TukeyHSD(Mres)                                        # UP-TR not; WC-TR not; WC-UP not
tukey.plot.test<-TukeyHSD(Gres)
plot(tukey.plot.test, las = 1)

#GWI
GWres <- aov(LogN2 ~ Zone, data = GWRates)
summary(GWres)                                         # Zone: p=2.56 x10-5
TukeyHSD(GWres)                                        # UP-TR Sig; WC-TR NOT; WC-UP sig
tukey.plot.test<-TukeyHSD(GWres)
plot(tukey.plot.test, las = 1)

#SWH
Sres <- aov(LogN2 ~ Zone, data = SRates)
summary(Sres)                                         # Zone: p=0.04
TukeyHSD(Sres)                                        # UP-TR Not; WC-TR Sig; WC-UP not
tukey.plot.test<-TukeyHSD(Sres)
plot(tukey.plot.test, las = 1)


######################################################

################ Summarize Rates ######################
RatesAvg <- Rates %>% 
  group_by(Site, Zone) %>% 
  summarise(mean = as.numeric(mean(N2_Rate_g_hr, na.rm=TRUE)),
            std = sd(N2_Rate_g_hr, na.rm=TRUE), 
            se = as.numeric(std/(sqrt(3))))
RatesAvg <- as.data.frame(RatesAvg)
head(RatesAvg)

RatesAvg$Zone <- factor(RatesAvg$Zone , levels = c("UP", "TR", "WC"))

#######################################################

############### Make individual Bar Plots ################
G2 <- subset(RatesAvg, Site=="Gcrew")
G2$cld <- c("b", "a", "a")
G2$cldy <- G2$mean + G2$se
head(G2)

Gavg <- ggplot(data=G2, aes(x=Zone, y=mean, fill=Zone)) +
  geom_bar(stat="identity", color="black")+
  labs(y=expression("nmoles N"[2]*"-N g"^-1*" h"^-1)) +
  xlab(" ") + 
  ggtitle("(a) GCReW") +
  geom_text(aes(label = cld, y = cldy), vjust = -0.5) +
  annotate("text", x=1.2, y=25, label= "Zone: p-value = 0.001") +
  ylim(0,25)+
  theme_classic() +
  scale_fill_manual(values=compass_coolors2a) + 
  scale_x_discrete(labels=c("Upland", "Transition", "Wetland")) +
  theme(panel.background = element_rect(colour = "black", size=1.2), 
        legend.position = "NONE", 
        legend.title= element_blank(), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.05, size=1,
                position=position_dodge(.9))
Gavg


M2 <- subset(RatesAvg, Site=="MSM")
M2$cld <- c("b", "a", "a")
M2$cldy <- M2$mean + M2$se
head(M2)

Mavg <- ggplot(data=M2, aes(x=Zone, y=mean, fill=Zone)) +
  geom_bar(stat="identity", color="black")+
  labs(y=expression("nmoles N"[2]*"-N g"^-1*" h"^-1)) +
  xlab(" ") + 
  ylim(0,25)+
  geom_text(aes(label = cld, y = cldy), vjust = -0.5) +
  annotate("text", x=1.2, y=25, label= "Zone: p-value = 0.002") +
  ggtitle("(b) Money Stump Swamp") +
  theme_classic() +
  scale_fill_manual(values=compass_coolors2a) +
  scale_x_discrete(labels=c("Upland", "Transition", "Wetland")) +
  theme(panel.background = element_rect(colour = "black", size=1.2), 
        legend.position = "NONE",  
        legend.title= element_blank(), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.05, size=1,
                position=position_dodge(.9))
Mavg

  
GW2 <- subset(RatesAvg, Site=="GWI")
GW2$cld <- c("b", "a", "b")
GW2$cldy <- GW2$mean + GW2$se
head(GW2)

GWavg <- ggplot(data=GW2, aes(x=Zone, y=mean, fill=Zone)) +
  geom_bar(stat="identity", color="black")+
  labs(y=expression("nmoles N"[2]*"-N g"^-1*" h"^-1)) +
  xlab(" ") + 
  ylim(0,25)+
  ggtitle("(c) Goodwin Islands") +
  geom_text(aes(label = cld, y = cldy), vjust = -0.5) +
  annotate("text", x=1.2, y=25, label= "Zone: p-value = 0.002") +
  theme_classic() +
  scale_fill_manual(values=compass_coolors2a) +  
  scale_x_discrete(labels=c("Upland", "Transition", "Wetland")) +
  theme(panel.background = element_rect(colour = "black", size=1.2), 
        legend.position = "NONE", 
        legend.title= element_blank(), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.05, size=1,
                position=position_dodge(.9))
GWavg

S2 <- subset(RatesAvg, Site=="SWH")
S2$cld <- c("ab", "a", "a")
S2$cldy <- S2$mean + S2$se
head(S2)

Savg <- ggplot(data=S2, aes(x=Zone, y=mean, fill=Zone)) +
  geom_bar(stat="identity", color="black")+
  labs(y=expression("nmoles N"[2]*"-N g"^-1*" h"^-1)) +
  xlab(" ") + 
  ylim(0,25)+
  ggtitle("(d) Sweet Hall Marsh") +
  geom_text(aes(label = cld, y = cldy), vjust = -0.5) +
  annotate("text", x=1.2, y=25, label= "Zone: p-value = 0.02") +
  theme_classic() +
  scale_fill_manual(values=compass_coolors2a) +
  scale_x_discrete(labels=c("Upland", "Transition", "Wetland")) +
  theme(panel.background = element_rect(colour = "black", size=1.2), 
        legend.position = "NONE", 
        legend.title= element_blank(), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.05, size=1,
                position=position_dodge(.9))
Savg


All_Avg <- ggarrange(Gavg, Mavg, GWavg, Savg, ncol=2, nrow=2)

pdf("Avg_DNF_N2N_Rates1.pdf", width=8,height=8)
All_Avg
dev.off()

ggsave(plot = All_Avg, "DNF_20221107.png", h = 8, w = 8, type = "cairo-png")

#########################################################

### boxplots ####
Rates$Zone <- factor(Rates$Zone , levels = c("UP", "TR", "WC"))
Allbox <- ggboxplot(Rates, x = "Zone", y = "N2_Rate_g_hr", 
                  color = "Black", fill="Site") + 
  labs(y=expression("nmoles N"[2]*"-N g"^-1*" h"^-1)) +
  xlab(" ") + 
  ylim(0,25)+
  ggtitle("All DNF") +
  #geom_text(aes(label = cld, y = cldy), vjust = -0.5) +
  annotate("text", x=3, y=25, label= "Zone: p-value = 0.02") +
  theme_classic() +
 # scale_fill_manual(values=compass_coolors2b) +
  scale_x_discrete(labels=c("Upland", "Transition", "Wetland")) +
  theme(panel.background = element_rect(colour = "black", size=1.2), 
        legend.position = "bottom", 
        legend.title= element_blank(), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) 
Allbox

head(GRates)
GRates$Zone <- factor(GRates$Zone , levels = c("UP", "TR", "WC"))

Gbox <- ggboxplot(GRates, x = "Zone", y = "N2_Rate_g_hr", 
          color = "Black", fill="Zone") + 
          labs(y=expression("nmoles N"[2]*"-N g"^-1*" h"^-1)) +
          xlab(" ") + 
          ylim(0,25)+
          ggtitle("(d) Sweet Hall Marsh") +
          #geom_text(aes(label = cld, y = cldy), vjust = -0.5) +
          annotate("text", x=3, y=25, label= "Zone: p-value = 0.02") +
          theme_classic() +
          scale_fill_manual(values=compass_coolors2b) +
          scale_x_discrete(labels=c("Upland", "Transition", "Wetland")) +
          theme(panel.background = element_rect(colour = "black", size=1.2), 
            legend.position = "NONE", 
            legend.title= element_blank(), 
            axis.text=element_text(size=12), 
            axis.title=element_text(size=12)) 
Gbox

head(MRates)
MRates$Zone <- factor(MRates$Zone , levels = c("UP", "TR", "WC"))

Mbox <- ggboxplot(MRates, x = "Zone", y = "N2_Rate_g_hr", 
                  color = "Black", fill="Zone") + 
  labs(y=expression("nmoles N"[2]*"-N g"^-1*" h"^-1)) +
  xlab(" ") + 
  ylim(0,25)+
  ggtitle("(d) Sweet Hall Marsh") +
  #geom_text(aes(label = cld, y = cldy), vjust = -0.5) +
  annotate("text", x=3, y=25, label= "Zone: p-value = 0.02") +
  theme_classic() +
  scale_fill_manual(values=compass_coolors2b) +
  scale_x_discrete(labels=c("Upland", "Transition", "Wetland")) +
  theme(panel.background = element_rect(colour = "black", size=1.2), 
        legend.position = "NONE", 
        legend.title= element_blank(), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) 
Mbox





############




#####



