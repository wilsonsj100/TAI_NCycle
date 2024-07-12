##################################
################## INFO #######################
####### COMPASS Synoptic CB 
####### Nitrogen Experiment 2022
####### Rate Calculation & Data Visualization
####### Stephanie J. Wilson 
####### Edited: 10-04-2022
##############################################

################# THIS IS FOR ANAMMOX SO ONLY THE 29N2 production #############

############### SET UP #######################
#packages
library(ggplot2)
library(data.table)
library(tidyr)
library(dplyr)
library(ggpubr)
library(car)

#read in data
dat <- read.csv("COMPASS_NExpt_N2_Data_Final.csv")
head(dat)

compass_coolors <- c("#419973", "#BF8638", "#9255D4")
compass_coolors2 <- c("#20063B", "#FFBC42", "#419973")

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

####### Plot 29N2 over time & calc rates: GCREW #######
#GCrew 29N2 Production Figures 
Pg1up <- ggplot(G1UP, aes(x=Hours, y=N2_29, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles N2-N") +
  ggtitle("(a) GCReW Upland") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        legend.position = c(0.15, 0.8), 
        legend.title= element_blank()) +
  # theme(plot.title = element_text(margin = margin(t = 10, b = -20))) + 
  ylim(0,5) +
  scale_color_manual(values=c("black",'gray38','gray68'))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
Pg1up

Pg1tr <- ggplot(G1TR, aes(x=Hours, y=N2_29, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles N2-N") + 
  ggtitle("(b) GCReW Transition") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        legend.position = c(0.15, 0.8), 
        legend.title= element_blank()) +  
  ylim(0,5) +
  xlim(0,10) + 
  scale_color_manual(values=c("black",'gray38','gray68'))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
Pg1tr

Pg1wc <- ggplot(G1WC, aes(x=Hours, y=N2_29, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles N2-N") + 
  ggtitle("(c) GCReW Wetland") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        legend.position = c(0.15, 0.8), 
        legend.title= element_blank()) +  
  ylim(0,5) +
  xlim(0,10) +
  scale_color_manual(values=c("black",'gray38','gray68'))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
Pg1wc

#plot all three figures in a line
GcrewRaw <- ggarrange(Pg1up, Pg1tr, Pg1wc, ncol=3, nrow=1)

pdf("GCrew_Raw_29N2N_overtime.pdf", width=12,height=4)
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

lmGTRa <- lm(N2_29 ~ Hours, data=G1TRa)
lmGTRb <- lm(N2_29 ~ Hours, data=G1TRb)
lmGTRc <- lm(N2_29 ~ Hours, data=G1TRc)

lmGUPa <- lm(N2_29 ~ Hours, data=G1UPa)
lmGUPb <- lm(N2_29 ~ Hours, data=G1UPb)
lmGUPc <- lm(N2_29 ~ Hours, data=G1UPc)

lmGWCa <- lm(N2_29 ~ Hours, data=G1WCa)
lmGWCb <- lm(N2_29 ~ Hours, data=G1WCb)
lmGWCc <- lm(N2_29 ~ Hours, data=G1WCc)

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
colnames(Gslopes) <- c('29N2_Rate_g_hr')
head(Gslopes)



########################################################

####### Plot N2 over time & calc rates: MSM #######
#GCrew N2 Production Figures 
Pm1up <- ggplot(M1UP, aes(x=Hours, y=N2_29, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles N2-N") +
  ggtitle("(d) MoneyStump Swamp Upland") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        legend.position = c(0.15, 0.8), 
        legend.title= element_blank()) +
  # theme(plot.title = element_text(margin = margin(t = 10, b = -20))) + 
  ylim(0,5) +
  scale_color_manual(values=c("black",'gray38','gray68'))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
Pm1up

Pm1tr <- ggplot(M1TR, aes(x=Hours, y=N2_29, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles N2-N") + 
  ggtitle("(e) MoneyStump Swamp Transition") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        legend.position = c(0.15, 0.8), 
        legend.title= element_blank()) +  
  ylim(0,5) +
  xlim(0,10) + 
  scale_color_manual(values=c("black",'gray38','gray68'))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
Pm1tr

Pm1wc <- ggplot(M1WC, aes(x=Hours, y=N2_29, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles N2-N") + 
  ggtitle("(f) MoneyStump Swamp Wetland") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        legend.position = c(0.15, 0.8), 
        legend.title= element_blank()) +  
  ylim(0,5) +
  xlim(0,10) +
  scale_color_manual(values=c("black",'gray38','gray68'))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
Pm1wc

#plot all three figures in a line
MSMRaw <- ggarrange(Pm1up, Pm1tr, Pm1wc, ncol=3, nrow=1)

pdf("MSM_Raw_29N2N_overtime.pdf", width=12,height=4)
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

lmMUPa <- lm(N2_29 ~ Hours, data=M1UPa)
lmMUPb <- lm(N2_29 ~ Hours, data=M1UPb)
lmMUPc <- lm(N2_29 ~ Hours, data=M1UPc)

lmMTRa <- lm(N2_29 ~ Hours, data=M1TRa)
lmMTRb <- lm(N2_29 ~ Hours, data=M1TRb)
lmMTRc <- lm(N2_29 ~ Hours, data=M1TRc)

lmMWCa <- lm(N2_29 ~ Hours, data=M1WCa)
lmMWCb <- lm(N2_29 ~ Hours, data=M1WCb)
lmMWCc <- lm(N2_29 ~ Hours, data=M1WCc)

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
colnames(Mslopes) <- c('29N2_Rate_g_hr')
head(Mslopes)

########################################################

####### Plot N2 over time & calc rates: GWI #######
#GCrew N2 Production Figures 
Pgw1up <- ggplot(GW1UP, aes(x=Hours, y=N2_29, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles N2-N") +
  ggtitle("(g) Goodwin Islands Upland") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        legend.position = c(0.15, 0.8), 
        legend.title= element_blank()) +
  # theme(plot.title = element_text(margin = margin(t = 10, b = -20))) + 
  ylim(0,5) +
  scale_color_manual(values=c("black",'gray38','gray68'))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
Pgw1up

Pgw1tr <- ggplot(GW1TR, aes(x=Hours, y=N2_29, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles N2-N") + 
  ggtitle("(h) Goodwin Islands Transition") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        legend.position = c(0.15, 0.8), 
        legend.title= element_blank()) +  
  ylim(0,5) +
  xlim(0,10) + 
  scale_color_manual(values=c("black",'gray38','gray68'))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
Pgw1tr

Pgw1wc <- ggplot(GW1WC, aes(x=Hours, y=N2_29, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles N2-N") + 
  ggtitle("(i) Goodwin Islands Wetland") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        legend.position = c(0.15, 0.8), 
        legend.title= element_blank()) +  
  ylim(0,5) +
  xlim(0,10) +
  scale_color_manual(values=c("black",'gray38','gray68'))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
Pgw1wc

#plot all three figures in a line
GWIRaw <- ggarrange(Pgw1up, Pgw1tr, Pgw1wc, ncol=3, nrow=1)

pdf("GWI_Raw_29N2N_overtime.pdf", width=12,height=4)
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

lmGwUPa <- lm(N2_29 ~ Hours, data=Gw1UPa)
lmGwUPb <- lm(N2_29 ~ Hours, data=Gw1UPb)
lmGwUPc <- lm(N2_29 ~ Hours, data=Gw1UPc)

lmGwTRa <- lm(N2_29 ~ Hours, data=Gw1TRa)
lmGwTRb <- lm(N2_29 ~ Hours, data=Gw1TRb)
lmGwTRc <- lm(N2_29 ~ Hours, data=Gw1TRc)

lmGwWCa <- lm(N2_29 ~ Hours, data=Gw1WCa)
lmGwWCb <- lm(N2_29 ~ Hours, data=Gw1WCb)
lmGwWCc <- lm(N2_29 ~ Hours, data=Gw1WCc)

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
colnames(Gwslopes) <- c('29N2_Rate_g_hr')
head(Gwslopes)

########################################################

####### Plot N2 over time & calc rates: SWH #######
#GCrew N2 Production Figures 
Ps1up <- ggplot(S1UP, aes(x=Hours, y=N2_29, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles N2-N") +
  ggtitle("(j) Sweet Hall Marsh Upland") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        legend.position = c(0.15, 0.8), 
        legend.title= element_blank()) +
  # theme(plot.title = element_text(margin = margin(t = 10, b = -20))) + 
  ylim(0,5) +
  scale_color_manual(values=c("black",'gray38','gray68'))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
Ps1up

Ps1tr <- ggplot(S1TR, aes(x=Hours, y=N2_29, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles N2-N") + 
  ggtitle("(k) Sweet Hall Marsh Transition") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        legend.position = c(0.15, 0.8), 
        legend.title= element_blank()) +  
  ylim(0,5) +
  xlim(0,10) + 
  scale_color_manual(values=c("black",'gray38','gray68'))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
Ps1tr

Ps1wc <- ggplot(S1WC, aes(x=Hours, y=N2_29, col=Replicate)) +
  geom_point(size=4) + 
  ylab("nmoles N2-N") + 
  ggtitle("(l) Sweet Hall Marsh Wetland") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        legend.position = c(0.15, 0.8), 
        legend.title= element_blank()) +  
  ylim(0,5) +
  xlim(0,10) +
  scale_color_manual(values=c("black",'gray38','gray68'))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
Ps1wc

#plot all three figures in a line
SWHRaw <- ggarrange(Ps1up, Ps1tr, Ps1wc, ncol=3, nrow=1)

pdf("SWH_Raw_29N2N_overtime.pdf", width=12,height=4)
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

lmSUPa <- lm(N2_29 ~ Hours, data=S1UPa)
lmSUPb <- lm(N2_29 ~ Hours, data=S1UPb)
lmSUPc <- lm(N2_29 ~ Hours, data=S1UPc)

lmSTRa <- lm(N2_29 ~ Hours, data=S1TRa)
lmSTRb <- lm(N2_29 ~ Hours, data=S1TRb)
lmSTRc <- lm(N2_29 ~ Hours, data=S1TRc)

lmSWCa <- lm(N2_29 ~ Hours, data=S1WCa)
lmSWCb <- lm(N2_29 ~ Hours, data=S1WCb)
lmSWCc <- lm(N2_29 ~ Hours, data=S1WCc)

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
colnames(Sslopes) <- c('29N2_Rate_g_hr')
head(Sslopes)

########################################################

############ Plot all N2-N overtime in one graph ########################

#plot all three figures in a line
N2_29RatesRaw <- ggarrange(Pg1up, Pg1tr, Pg1wc, Pm1up, Pm1tr, Pm1wc,
                        Pgw1up, Pgw1tr, Pgw1wc, Ps1up, Ps1tr, Ps1wc,
                        ncol=3, nrow=4, common.legend = TRUE, legend="bottom")

pdf("Raw_29N2N_overtime.pdf", width=10,height=12)
N2_29RatesRaw
dev.off()

############################################################

################ Make new data frame ######################
Gavg <- G1 %>% 
  group_by(Site, Zone, Replicate) %>% 
  summarise(mean = mean(N2_29, na.rm=TRUE))
head(Gavg)

GRates <- as.data.frame(cbind(Gavg$Site, Gavg$Zone, Gavg$Replicate, Gslopes$'29N2_Rate_g_hr'))
colnames(GRates) <- c("Site", "Zone", "Replicate", '29N2_Rate_g_hr')
GRates$'29N2_Rate_g_hr' <- as.numeric(GRates$'29N2_Rate_g_hr')
head(GRates)

Mavg <- M1 %>% 
  group_by(Site, Zone, Replicate) %>% 
  summarise(mean = mean(N2_29, na.rm=TRUE))
head(Mavg)

MRates <- as.data.frame(cbind(Mavg$Site, Mavg$Zone, Mavg$Replicate, Mslopes$'29N2_Rate_g_hr'))
colnames(MRates) <- c("Site", "Zone", "Replicate", '29N2_Rate_g_hr')
MRates$'29N2_Rate_g_hr' <- as.numeric(MRates$'29N2_Rate_g_hr')
head(MRates)

GWavg <- GW1 %>% 
  group_by(Site, Zone, Replicate) %>% 
  summarise(mean = mean(N2_29, na.rm=TRUE))
head(GWavg)

GWRates <- as.data.frame(cbind(GWavg$Site, GWavg$Zone, GWavg$Replicate, Gwslopes$'29N2_Rate_g_hr'))
colnames(GWRates) <- c("Site", "Zone", "Replicate", '29N2_Rate_g_hr')
GWRates$'29N2_Rate_g_hr' <- as.numeric(GWRates$'29N2_Rate_g_hr')
head(GWRates)

Savg <- S1 %>% 
  group_by(Site, Zone, Replicate) %>% 
  summarise(mean = mean(N2N, na.rm=TRUE))
head(Savg)

SRates <- as.data.frame(cbind(Savg$Site, Savg$Zone, Savg$Replicate, Sslopes$'29N2_Rate_g_hr'))
colnames(SRates) <- c("Site", "Zone", "Replicate", '29N2_Rate_g_hr')
SRates$'29N2_Rate_g_hr' <- as.numeric(SRates$'29N2_Rate_g_hr')
head(SRates)

Rates <- as.data.frame(rbind(GRates, MRates, GWRates, SRates))
head(Rates)

Rates$AMX_g_hr <- as.numeric(Rates$'29N2_Rate_g_hr')

write.csv(Rates, file="AMX_Rates_RCalc.csv")

#############################################

head(Rates)

################ Summarize Rates ######################

RatesAvg <- Rates %>% 
  group_by(Site, Zone) %>% 
  summarise(mean = as.numeric(mean(AMX_g_hr, na.rm=TRUE)),
            std = sd(AMX_g_hr, na.rm=TRUE), 
            se = as.numeric(std/(sqrt(3))))

RatesAvg <- as.data.frame(RatesAvg)
head(RatesAvg)

RatesAvg$Zone <- factor(RatesAvg$Zone , levels = c("UP", "TR", "WC"))

write.csv(RatesAvg, file="Avg_AMX_Rates_RCalc.csv")

#######################################################
