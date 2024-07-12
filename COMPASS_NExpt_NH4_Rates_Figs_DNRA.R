##################################
################## INFO #######################
####### COMPASS Synoptic CB 
####### Nitrogen Experiment 2022
####### Rate Calculation & Data Visualization
####### Stephanie J. Wilson 
####### Edited: 10-04-2022
##############################################

#DNRA

############### SET UP #######################
#packages
library(ggplot2)
library(data.table)
library(tidyr)
library(dplyr)
library(ggpubr)
library(car)

#read in data
dat <- read.csv("COMPASS_NExpt_NH4_Sample_Data_Final.csv")
head(dat)

compass_coolors <- c("#419973", "#BF8638", "#9255D4")
compass_coolors1 <- c("#20063B", "#FFBC42", "#419973")
compass_coolors2 <- c("#20063B", "#FFBC42", "#419973")
##############################################

######### Sample dataframe set up ############

#change column names 
colnames(dat) <- c('ID', 'Hours','NH4_nmole')
head(dat)

dat$ID <- as.character(dat$ID)

#make new sample id columns from the ID 
dat1 <- dat %>% separate(ID, 
                         c("Site", "Zone","Replicate", "Time_Point"))

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

####### Plot NH4 over time & calc rates: GCREW #######
#GCrew NH4 Production Figures 
Pg1up <- ggplot(G1UP, aes(x=Hours, y=NH4_nmole, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles NH4") +
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

Pg1tr <- ggplot(G1TR, aes(x=Hours, y=NH4_nmole, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles NH4") + 
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

Pg1wc <- ggplot(G1WC, aes(x=Hours, y=NH4_nmole, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles NH4") + 
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

pdf("GCrew_Raw_NH4_overtime.pdf", width=12,height=4)
GcrewRaw
dev.off()

#Now calculate GCrew Lm for slope of these three lines 
G1TRa <- subset(G1TR, Replicate=="A")
G1TRb <- subset(G1TR, Replicate=="B")
G1TRc <- subset(G1TR, Replicate=="C")

G1UPa <- subset(G1UP, Replicate=="A")
G1UPb <- subset(G1UP, Replicate=="B")
G1UPc <- subset(G1UP, Replicate=="C")

G1WCa <- subset(G1WC, Replicate=="A")
G1WCb <- subset(G1WC, Replicate=="B")
G1WCc <- subset(G1WC, Replicate=="C")

lmGTRa <- lm(NH4_nmole ~ Hours, data=G1TRa)
lmGTRb <- lm(NH4_nmole ~ Hours, data=G1TRb)
lmGTRc <- lm(NH4_nmole ~ Hours, data=G1TRc)

lmGUPa <- lm(NH4_nmole ~ Hours, data=G1UPa)
lmGUPb <- lm(NH4_nmole ~ Hours, data=G1UPb)
lmGUPc <- lm(NH4_nmole ~ Hours, data=G1UPc)

lmGWCa <- lm(NH4_nmole ~ Hours, data=G1WCa)
lmGWCb <- lm(NH4_nmole ~ Hours, data=G1WCb)
lmGWCc <- lm(NH4_nmole ~ Hours, data=G1WCc)

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
colnames(Gslopes) <- c('NH4_Rate_g_hr')
head(Gslopes)



########################################################

####### Plot N2 over time & calc rates: MSM #######
#GCrew N2 Production Figures 
Pm1up <- ggplot(M1UP, aes(x=Hours, y=NH4_nmole, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles NH4") +
  ggtitle("(d) MoneyStump Swamp Upland") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        legend.position = c(0.15, 0.8), 
        legend.title= element_blank()) +
  # theme(plot.title = element_text(margin = margin(t = 10, b = -20))) + 
  ylim(0,100) +
  scale_color_manual(values=c("black",'gray38','gray68'))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
Pm1up

Pm1tr <- ggplot(M1TR, aes(x=Hours, y=NH4_nmole, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles NH4") + 
  ggtitle("(e) MoneyStump Swamp Transition") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        legend.position = c(0.15, 0.8), 
        legend.title= element_blank()) +  
  ylim(0,100) +
  xlim(0,10) + 
  scale_color_manual(values=c("black",'gray38','gray68'))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
Pm1tr

Pm1wc <- ggplot(M1WC, aes(x=Hours, y=NH4_nmole, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles NH4") + 
  ggtitle("(f) MoneyStump Swamp Wetland") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        legend.position = c(0.15, 0.8), 
        legend.title= element_blank()) +  
  ylim(0,100) +
  xlim(0,10) +
  scale_color_manual(values=c("black",'gray38','gray68'))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
Pm1wc

#plot all three figures in a line
MSMRaw <- ggarrange(Pm1up, Pm1tr, Pm1wc, ncol=3, nrow=1)

pdf("MSM_Raw_NH4_overtime.pdf", width=12,height=4)
MSMRaw
dev.off()

#Now calculate GCrew Lm for slope of these three lines 
M1UPa <- subset(M1UP, Replicate=="A")
M1UPb <- subset(M1UP, Replicate=="B")
M1UPc <- subset(M1UP, Replicate=="C")

M1TRa <- subset(M1TR, Replicate=="A")
M1TRb <- subset(M1TR, Replicate=="B")
M1TRc <- subset(M1TR, Replicate=="C")

M1WCa <- subset(M1WC, Replicate=="A")
M1WCb <- subset(M1WC, Replicate=="B")
M1WCc <- subset(M1WC, Replicate=="C")

lmMUPa <- lm(NH4_nmole ~ Hours, data=M1UPa)
lmMUPb <- lm(NH4_nmole ~ Hours, data=M1UPb)
lmMUPc <- lm(NH4_nmole ~ Hours, data=M1UPc)

lmMTRa <- lm(NH4_nmole ~ Hours, data=M1TRa)
lmMTRb <- lm(NH4_nmole ~ Hours, data=M1TRb)
lmMTRc <- lm(NH4_nmole ~ Hours, data=M1TRc)

lmMWCa <- lm(NH4_nmole ~ Hours, data=M1WCa)
lmMWCb <- lm(NH4_nmole ~ Hours, data=M1WCb)
lmMWCc <- lm(NH4_nmole ~ Hours, data=M1WCc)

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
colnames(Mslopes) <- c('NH4_Rate_g_hr')
head(Mslopes)

########################################################

####### Plot N2 over time & calc rates: GWI #######
#GCrew N2 Production Figures 
Pgw1up <- ggplot(GW1UP, aes(x=Hours, y=NH4_nmole, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles NH4") +
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

Pgw1tr <- ggplot(GW1TR, aes(x=Hours, y=NH4_nmole, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles NH4") + 
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

Pgw1wc <- ggplot(GW1WC, aes(x=Hours, y=NH4_nmole, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles NH4") + 
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

pdf("GWI_Raw_NH4_overtime.pdf", width=12,height=4)
GWIRaw
dev.off()

#Now calculate GWI Lm for slope of these three lines 
Gw1UPa <- subset(GW1UP, Replicate=="A")
Gw1UPb <- subset(GW1UP, Replicate=="B")
Gw1UPc <- subset(GW1UP, Replicate=="C")

Gw1TRa <- subset(GW1TR, Replicate=="A")
Gw1TRb <- subset(GW1TR, Replicate=="B")
Gw1TRc <- subset(GW1TR, Replicate=="C")

Gw1WCa <- subset(GW1WC, Replicate=="A")
Gw1WCb <- subset(GW1WC, Replicate=="B")
Gw1WCc <- subset(GW1WC, Replicate=="C")

lmGwUPa <- lm(NH4_nmole ~ Hours, data=Gw1UPa)
lmGwUPb <- lm(NH4_nmole ~ Hours, data=Gw1UPb)
lmGwUPc <- lm(NH4_nmole ~ Hours, data=Gw1UPc)

lmGwTRa <- lm(NH4_nmole ~ Hours, data=Gw1TRa)
lmGwTRb <- lm(NH4_nmole ~ Hours, data=Gw1TRb)
lmGwTRc <- lm(NH4_nmole ~ Hours, data=Gw1TRc)

lmGwWCa <- lm(NH4_nmole ~ Hours, data=Gw1WCa)
lmGwWCb <- lm(NH4_nmole ~ Hours, data=Gw1WCb)
lmGwWCc <- lm(NH4_nmole ~ Hours, data=Gw1WCc)

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
colnames(Gwslopes) <- c('NH4_Rate_g_hr')
head(Gwslopes)

########################################################

####### Plot N2 over time & calc rates: SWH #######
#GCrew N2 Production Figures 
Ps1up <- ggplot(S1UP, aes(x=Hours, y=NH4_nmole, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles NH4") +
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

Ps1tr <- ggplot(S1TR, aes(x=Hours, y=NH4_nmole, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles NH4") + 
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

Ps1wc <- ggplot(S1WC, aes(x=Hours, y=NH4_nmole, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles NH4") + 
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

pdf("SWH_Raw_NH4_overtime.pdf", width=12,height=4)
SWHRaw
dev.off()

#Now calculate GCrew Lm for slope of these three lines 
S1UPa <- subset(S1UP, Replicate=="A")
S1UPb <- subset(S1UP, Replicate=="B")
S1UPc <- subset(S1UP, Replicate=="C")

S1TRa <- subset(S1TR, Replicate=="A")
S1TRb <- subset(S1TR, Replicate=="B")
S1TRc <- subset(S1TR, Replicate=="C")

S1WCa <- subset(S1WC, Replicate=="A")
S1WCb <- subset(S1WC, Replicate=="B")
S1WCc <- subset(S1WC, Replicate=="C")

lmSUPa <- lm(NH4_nmole ~ Hours, data=S1UPa)
lmSUPb <- lm(NH4_nmole ~ Hours, data=S1UPb)
lmSUPc <- lm(NH4_nmole ~ Hours, data=S1UPc)

lmSTRa <- lm(NH4_nmole ~ Hours, data=S1TRa)
lmSTRb <- lm(NH4_nmole ~ Hours, data=S1TRb)
lmSTRc <- lm(NH4_nmole ~ Hours, data=S1TRc)

lmSWCa <- lm(NH4_nmole ~ Hours, data=S1WCa)
lmSWCb <- lm(NH4_nmole ~ Hours, data=S1WCb)
lmSWCc <- lm(NH4_nmole ~ Hours, data=S1WCc)

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
colnames(Sslopes) <- c('NH4_Rate_g_hr')
head(Sslopes)

########################################################

############ Plot all N2-N overtime in one graph ########################

#plot all three figures in a line
N2RatesRaw <- ggarrange(Pg1up, Pg1tr, Pg1wc, Pm1up, Pm1tr, Pm1wc,
                        Pgw1up, Pgw1tr, Pgw1wc, Ps1up, Ps1tr, Ps1wc,
                        ncol=3, nrow=4, common.legend = TRUE, legend="bottom")

pdf("Raw_NH4-N_overtime.pdf", width=10,height=12)
N2RatesRaw
dev.off()

############################################################

################ Make new data frame ######################
Gavg <- G1 %>% 
  group_by(Site, Zone, Replicate) %>% 
  dplyr::summarize(mean = mean(NH4_nmole, na.rm=TRUE))
head(Gavg)

GRates <- as.data.frame(cbind(Gavg$Site, Gavg$Zone, Gavg$Replicate, Gslopes$NH4_Rate_g_hr))
colnames(GRates) <- c("Site", "Zone", "Replicate", 'NH4_Rate_g_hr')
GRates$NH4_Rate_g_hr <- as.numeric(GRates$NH4_Rate_g_hr)
head(GRates)

Mavg <- M1 %>% 
  group_by(Site, Zone, Replicate) %>% 
  dplyr::summarize(mean = mean(NH4_nmole, na.rm=TRUE))
head(Mavg)

MRates <- as.data.frame(cbind(Mavg$Site, Mavg$Zone, Mavg$Replicate, Mslopes$NH4_Rate_g_hr))
colnames(MRates) <- c("Site", "Zone", "Replicate", 'NH4_Rate_g_hr')
MRates$NH4_Rate_g_hr <- as.numeric(MRates$NH4_Rate_g_hr)
head(MRates)

GWavg <- GW1 %>% 
  group_by(Site, Zone, Replicate) %>% 
  dplyr::summarize(mean = mean(NH4_nmole, na.rm=TRUE))
head(GWavg)

GWRates <- as.data.frame(cbind(GWavg$Site, GWavg$Zone, GWavg$Replicate, Gwslopes$NH4_Rate_g_hr))
colnames(GWRates) <- c("Site", "Zone", "Replicate", 'NH4_Rate_g_hr')
GWRates$NH4_Rate_g_hr <- as.numeric(GWRates$NH4_Rate_g_hr)
head(GWRates)

Savg <- S1 %>% 
  group_by(Site, Zone, Replicate) %>% 
  dplyr::summarize(mean = mean(NH4_nmole, na.rm=TRUE))
head(Savg)

SRates <- as.data.frame(cbind(Savg$Site, Savg$Zone, Savg$Replicate, Sslopes$NH4_Rate_g_hr))
colnames(SRates) <- c("Site", "Zone", "Replicate", 'NH4_Rate_g_hr')
SRates$NH4_Rate_g_hr <- as.numeric(SRates$NH4_Rate_g_hr)
head(SRates)

Rates <- as.data.frame(rbind(GRates, MRates, GWRates, SRates))
head(Rates)

Rates$NH4_Rate_g_hr <- as.numeric(Rates$NH4_Rate_g_hr)

write.csv(Rates, file="DNRA_Rates_RCalc.csv")

#############################################

############### Check Data Normality & Variances ##################
hist(Rates$NH4_Rate_g_hr)     
#hist(Rates$LogN2)

### Try without log adjust 
qqnorm(Rates$NH4_Rate_g_hr) #technically the data cannot be zero so they are log normal, but look pretty normal here.... 
qqline(Rates$NH4_Rate_g_hr)

#All 
#do a variance test to check homogeneity of variance 
bartlett.test(NH4_Rate_g_hr ~ Zone, data = Rates)               # p=0.0001
# Levene's test with one independent variable
leveneTest(NH4_Rate_g_hr ~ Zone, data = Rates)                 #  p=0.0001

#GCReW
#do a variance test to check homogeneity of variance 
bartlett.test(NH4_Rate_g_hr ~ Zone, data = GRates)               # p=0.1344
# Levene's test with one independent variable
leveneTest(NH4_Rate_g_hr ~ Zone, data = GRates)                 #  p=0.6738

#MSM
#do a variance test to check homogeneity of variance 
bartlett.test(NH4_Rate_g_hr ~ Zone, data = MRates)        # p=0.5953
# Levene's test with one independent variable
leveneTest(NH4_Rate_g_hr ~ Zone, data = MRates)          #  p=0.7765

#GWI 
#do a variance test to check homogeneity of variance 
bartlett.test(NH4_Rate_g_hr ~ Zone, data = GWRates)             # p=0.308
# Levene's test with one independent variable
leveneTest(NH4_Rate_g_hr ~ Zone, data = GWRates)               # p=0.7195

#SWH
#do a variance test to check homogeneity of variance 
bartlett.test(NH4_Rate_g_hr ~ Zone, data = SRates)            # p=0.1517
# Levene's test with one independent variable
leveneTest(NH4_Rate_g_hr ~ Zone, data = SRates)               # p=0.3921

############################################################################################

################ Summarize Rates ######################
RatesAvg <- Rates %>% 
  group_by(Site, Zone) %>% 
  dplyr::summarize(mean = as.numeric(mean(NH4_Rate_g_hr, na.rm=TRUE)),
            std = sd(NH4_Rate_g_hr, na.rm=TRUE), 
            se = as.numeric(std/(sqrt(3))))
RatesAvg <- as.data.frame(RatesAvg)
head(RatesAvg)

RatesAvg$Zone <- factor(RatesAvg$Zone , levels = c("UP", "TR", "WC"))

write.csv(RatesAvg, file="Avg_DNRA_Rates_RCalc.csv")

#######################################################

############### Make individual Bar Plots ################
G2 <- subset(RatesAvg, Site=="Gcrew")
G2$cld <- c("b", "a", "a")
G2$cldy <- G2$mean + G2$se
head(G2)

Gavg <- ggplot(data=G2, aes(x=Zone, y=mean, fill=Zone)) +
  geom_bar(stat="identity", color="black")+
  labs(y=expression("nmoles NH "[4]*"-N g"^-1*" h"^-1)) +
  xlab(" ") + 
  ggtitle("(a) GCReW") +
  #geom_text(aes(label = cld, y = cldy), vjust = -0.5) +
  #annotate("text", x=1.2, y=25, label= "Zone: p-value = 0.08") +
  ylim(-1,25)+
  theme_classic() +
  scale_fill_manual(values=c(compass_coolors2)) + 
  scale_x_discrete(labels=c("Upland", "Transition", "Wetland")) +
  theme(panel.background = element_rect(colour = "black", size=1.2), 
        legend.position = "NONE", 
        legend.title= element_blank(), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, size=1,
                position=position_dodge(.9))
Gavg


M2 <- subset(RatesAvg, Site=="MSM")
M2$cld <- c("b", "a", "a")
M2$cldy <- M2$mean + M2$se
head(M2)

Mavg <- ggplot(data=M2, aes(x=Zone, y=mean, fill=Zone)) +
  geom_bar(stat="identity", color="black")+
  labs(y=expression("nmoles NH "[4]*"-N g"^-1*" h"^-1)) +
  xlab(" ") + 
  ylim(0,25)+
  #geom_text(aes(label = cld, y = cldy), vjust = -0.5) +
  #annotate("text", x=1.2, y=25, label= "Zone: p-value = 0.5") +
  ggtitle("(b) Moneystump Swamp") +
  theme_classic() +
  scale_fill_manual(values=c(compass_coolors2)) +   
  scale_x_discrete(labels=c("Upland", "Transition", "Wetland")) +
  theme(panel.background = element_rect(colour = "black", size=1.2), 
        legend.position = "NONE",  
        legend.title= element_blank(), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, size=1,
                position=position_dodge(.9))
Mavg


GW2 <- subset(RatesAvg, Site=="GWI")
GW2$cld <- c("a", "b", "c")
GW2$cldy <- GW2$mean + GW2$se
head(GW2)

GWavg <- ggplot(data=GW2, aes(x=Zone, y=mean, fill=Zone)) +
  geom_bar(stat="identity", color="black")+
  labs(y=expression("nmoles NH "[4]*"-N g"^-1*" h"^-1)) +
  xlab(" ") + 
  ylim(0,25)+
  ggtitle("(c) Goodwin Islands") +
  #geom_text(aes(label = cld, y = cldy), vjust = -0.5) +
  #annotate("text", x=1.2, y=25, label= "  Zone: p-value = 0.0003") +
  theme_classic() +
  scale_fill_manual(values=c(compass_coolors2)) +  
  scale_x_discrete(labels=c("Upland", "Transition", "Wetland")) +
  theme(panel.background = element_rect(colour = "black", size=1.2), 
        legend.position = "NONE", 
        legend.title= element_blank(), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, size=1,
                position=position_dodge(.9))
GWavg

S2 <- subset(RatesAvg, Site=="SWH")
S2$cld <- c("a", "b", "c")
S2$cldy <- S2$mean + S2$se
head(S2)

Savg <- ggplot(data=S2, aes(x=Zone, y=mean, fill=Zone)) +
  geom_bar(stat="identity", color="black")+
  labs(y=expression("nmoles NH "[4]*"-N g"^-1*" h"^-1)) +
  xlab(" ") + 
  ylim(0,25)+
  ggtitle("(d) Sweet Hall Marsh") +
  #geom_text(aes(label = cld, y = cldy), vjust = -0.5) +
  #annotate("text", x=1.2, y=25, label= "  Zone: p-value = 0.0008") +
  theme_classic() +
  scale_fill_manual(values=c(compass_coolors2)) + 
  scale_x_discrete(labels=c("Upland", "Transition", "Wetland")) +
  theme(panel.background = element_rect(colour = "black", size=1.2), 
        legend.position = "NONE", 
        legend.title= element_blank(), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, size=1,
                position=position_dodge(.9))
Savg


All_Avg <- ggarrange(Gavg, Mavg, GWavg, Savg, ncol=2, nrow=2)

pdf("Avg_DNRA_NH4_Rates1.pdf", width=8,height=8)
All_Avg
dev.off()

#########################################################

############### Make individual Bar Plots - ylim 0-10 ################
G2 <- subset(RatesAvg, Site=="Gcrew")
G2$cld <- c("b", "a", "a")
G2$cldy <- G2$mean + G2$se
head(G2)

Gavg1 <- ggplot(data=G2, aes(x=Zone, y=mean, fill=Zone)) +
  geom_bar(stat="identity", color="black")+
  labs(y=expression("nmoles NH "[4]*"-N g"^-1*" h"^-1)) +
  xlab(" ") + 
  ggtitle("(a) GCReW") +
  #geom_text(aes(label = cld, y = cldy), vjust = -0.5) +
  #annotate("text", x=1.2, y=25, label= "Zone: p-value = 0.08") +
  ylim(-1,10)+
  theme_classic() +
  scale_fill_manual(values=c(compass_coolors1)) + 
  scale_x_discrete(labels=c("Upland", "Transition", "Wetland")) +
  theme(panel.background = element_rect(colour = "black", size=1.2), 
        legend.position = "NONE", 
        legend.title= element_blank(), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, size=1,
                position=position_dodge(.9))
Gavg1


M2 <- subset(RatesAvg, Site=="MSM")
M2$cld <- c("b", "a", "a")
M2$cldy <- M2$mean + M2$se
head(M2)

Mavg1 <- ggplot(data=M2, aes(x=Zone, y=mean, fill=Zone)) +
  geom_bar(stat="identity", color="black")+
  labs(y=expression("nmoles NH "[4]*"-N g"^-1*" h"^-1)) +
  xlab(" ") + 
  ylim(-1,10)+
  #geom_text(aes(label = cld, y = cldy), vjust = -0.5) +
  #annotate("text", x=1.2, y=25, label= "Zone: p-value = 0.5") +
  ggtitle("(b) MoneyStump Swamp") +
  theme_classic() +
  scale_fill_manual(values=c(compass_coolors1)) +   
  scale_x_discrete(labels=c("Upland", "Transition", "Wetland")) +
  theme(panel.background = element_rect(colour = "black", size=1.2), 
        legend.position = "NONE",  
        legend.title= element_blank(), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, size=1,
                position=position_dodge(.9))
Mavg1


GW2 <- subset(RatesAvg, Site=="GWI")
GW2$cld <- c("a", "b", "c")
GW2$cldy <- GW2$mean + GW2$se
head(GW2)

GWavg1 <- ggplot(data=GW2, aes(x=Zone, y=mean, fill=Zone)) +
  geom_bar(stat="identity", color="black")+
  labs(y=expression("nmoles NH "[4]*"-N g"^-1*" h"^-1)) +
  xlab(" ") + 
  ylim(-1,10)+
  ggtitle("(c) Goodwin Islands") +
  #geom_text(aes(label = cld, y = cldy), vjust = -0.5) +
  #annotate("text", x=1.2, y=25, label= "  Zone: p-value = 0.0003") +
  theme_classic() +
  scale_fill_manual(values=c(compass_coolors1)) +  
  scale_x_discrete(labels=c("Upland", "Transition", "Wetland")) +
  theme(panel.background = element_rect(colour = "black", size=1.2), 
        legend.position = "NONE", 
        legend.title= element_blank(), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, size=1,
                position=position_dodge(.9))
GWavg1

S2 <- subset(RatesAvg, Site=="SWH")
S2$cld <- c("a", "b", "c")
S2$cldy <- S2$mean + S2$se
head(S2)

Savg1 <- ggplot(data=S2, aes(x=Zone, y=mean, fill=Zone)) +
  geom_bar(stat="identity", color="black")+
  labs(y=expression("nmoles NH "[4]*"-N g"^-1*" h"^-1)) +
  xlab(" ") + 
  ylim(-1,10)+
  ggtitle("(d) Sweet Hall Marsh") +
  #geom_text(aes(label = cld, y = cldy), vjust = -0.5) +
  #annotate("text", x=1.2, y=25, label= "  Zone: p-value = 0.0008") +
  theme_classic() +
  scale_fill_manual(values=c(compass_coolors1)) + 
  scale_x_discrete(labels=c("Upland", "Transition", "Wetland")) +
  theme(panel.background = element_rect(colour = "black", size=1.2), 
        legend.position = "NONE", 
        legend.title= element_blank(), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, size=1,
                position=position_dodge(.9))
Savg1


All_Avg1 <- ggarrange(Gavg1, Mavg1, GWavg1, Savg1, ncol=2, nrow=2)

pdf("Avg_DNRA_NH4_Rates2.pdf", width=8,height=8)
All_Avg1
dev.off()

#########################################################

### boxplots ####
Rates$Zone <- factor(Rates$Zone , levels = c("UP", "TR", "WC"))
Allbox <- ggboxplot(Rates, x = "Zone", y = "NH4_Rate_g_hr", 
                    color = "Black", fill="Site") + 
  labs(y=expression("nmoles N"[2]*"-N g"^-1*" h"^-1)) +
  xlab(" ") + 
  ylim(0,25)+
  ggtitle("All DNRA") +
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

Gbox <- ggboxplot(GRates, x = "Zone", y = "NH4_Rate_g_hr", 
                  color = "Black", fill="Zone") + 
  labs(y=expression("nmoles N g"^-1*" h"^-1*" ")) +
  xlab(" ") + 
  ylim(0,25)+
  ggtitle("(a) GCReW") +
  #annotate("text", x=1, y=5, label= "a") +
  #annotate("text", x=2, y=25, label= "b") +
  #annotate("text", x=3, y=5, label= "a") +
  theme_classic() +
  scale_fill_manual(values=compass_coolors) +
  scale_x_discrete(labels=c("Upland", "Transition", "Wetland")) +
  theme(panel.background = element_rect(colour = "black", size=1.2), 
        legend.position = "NONE", 
        legend.title= element_blank(), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) 
Gbox

head(MRates)
MRates$Zone <- factor(MRates$Zone , levels = c("UP", "TR", "WC"))

Mbox <- ggboxplot(MRates, x = "Zone", y = "NH4_Rate_g_hr", 
                  color = "Black", fill="Zone") + 
  labs(y=expression("nmoles N g"^-1*" h"^-1*" ")) +
  xlab(" ") + 
  ylim(0,25)+
  ggtitle("(b) Moneystump Swamp") +
  #annotate("text", x=1, y=5, label= "a") +
  #annotate("text", x=2, y=5, label= "a") +
 # annotate("text", x=3, y=5, label= "a") +
  theme_classic() +
  scale_fill_manual(values=compass_coolors) +
  scale_x_discrete(labels=c("Upland", "Transition", "Wetland")) +
  theme(panel.background = element_rect(colour = "black", size=1.2), 
        legend.position = "NONE", 
        legend.title= element_blank(), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) 
Mbox


head(GWRates)
GWRates$Zone <- factor(GWRates$Zone , levels = c("UP", "TR", "WC"))

GWbox <- ggboxplot(GWRates, x = "Zone", y = "NH4_Rate_g_hr", 
                   color = "Black", fill="Zone") + 
  labs(y=expression("nmoles N g"^-1*" h"^-1*" ")) +
  xlab(" ") + 
  ylim(0,25)+
  ggtitle("(b) Goodwin Islands") +
  #annotate("text", x=1, y=2, label= "a") +
  #annotate("text", x=2, y=7, label= "a") +
  #annotate("text", x=3, y=4, label= "a") +
  theme_classic() +
  scale_fill_manual(values=compass_coolors) +
  scale_x_discrete(labels=c("Upland", "Transition", "Wetland")) +
  theme(panel.background = element_rect(colour = "black", size=1.2), 
        legend.position = "NONE", 
        legend.title= element_blank(), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) 
GWbox

head(SRates)
SRates$Zone <- factor(SRates$Zone , levels = c("UP", "TR", "WC"))

Sbox <- ggboxplot(SRates, x = "Zone", y = "NH4_Rate_g_hr", 
                  color = "Black", fill="Zone") + 
  labs(y=expression("nmoles N g"^-1*" h"^-1*" ")) +
  xlab(" ") + 
  ylim(0,25)+
  ggtitle("(b) Sweet Hall Marsh") +
  #annotate("text", x=1, y=2, label= "a") +
 # annotate("text", x=2, y=7, label= "a") +
  #annotate("text", x=3, y=5, label= "a") +
  theme_classic() +
  scale_fill_manual(values=compass_coolors) +
  scale_x_discrete(labels=c("Upland", "Transition", "Wetland")) +
  theme(panel.background = element_rect(colour = "black", size=1.2), 
        legend.position = "NONE", 
        legend.title= element_blank(), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) 
Sbox

All_box <- ggarrange(Gbox, Mbox, GWbox, Sbox, ncol=2, nrow=2)

############
