######################### INFO #########################
#N Cycling across TAIs 

#Data vis and stats 

#author: Stephanie J. Wilson

#######################################################

######################### SET UP ######################
#packages
library(dplyr)
library(stringr)
library(vegan)
library(ggpubr)

#read in data 
dat <- read.csv("NExpt_All_Data.csv")
head(dat)

dat <- dat %>%
  mutate(Zone = factor(Zone, levels=c("UP", "TR", "WC")))

dat <- dat %>%
  mutate(Site = factor(Site, levels=c("Gcrew",  "MSM", "GWI","SWH")))

levels(dat$Site)

wc <- dat %>%  
  filter(str_detect(Zone, "WC"))
head(wc)

trup <- dat %>%  
  filter(str_detect(Zone, "UP|TR"))
head(trup)

#set colors
compass_coolors <- c("#419973", "#BF8638", "#9255D4")
compass_coolors2 <- c("#20063B", "#FFBC42", "#419973")
#######################################################

######################  SUPP FIG IN MANUSCRIPT: PCA of DATA #################################
head(dat)

#rename columns for arrows 
names <- as.list(colnames(dat1))

dat1 <- dat %>%
  rename(
    Denitrification = DNF_Rate_g_hr,
    DNRA = DNRA_Rate_g_hr, 
    'Soil Moisture' = Percent_Water_WetSoil,
    'Soil Organic Matter' = Percent_OM_WetSoil
  )



dat.pca <- prcomp(dat1[,c(5:9)],
                   center = TRUE,
                   scale. = TRUE)

#include all numeric values
#remove one line with an NA 
#dat <- dat[-c(2),]
#head(dat)
#dat.pca1 <- prcomp(dat[,c(5:9,32:39)],
  #                center = TRUE,
 #                 scale. = TRUE)
#dat.pca <- prcomp(~dat$DNF_Rate_g_hr+dat$DNRA_Rate_g_hr+dat$pH+,
 #                 center = TRUE,
  #                scale. = TRUE, 
   #               na.action = na.omit)
#summary(dat.pca1)

# loading library
library(ggfortify)
library(ggrepel)
dat.pca.plot <- autoplot(dat.pca,
                          data = dat1,
                          colour = 'Zone', 
                          shape = 'Site', size = 5,  frame=T, 
                         loadings = TRUE,
                         loadings.colour = 'black',
                         loadings.label = TRUE,
                         loadings.label.size = 5,       # smaller labels
                         loadings.arrow.size = 0.1,
                         loadings.label.colour = 'black')

dat.pca.plot + 
  scale_shape_manual(values = c(16,17,15,18)) + 
  scale_fill_manual(values = compass_coolors2) + 
  scale_color_manual(values = compass_coolors2) + 
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2), 
        legend.title= element_blank(), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) 

#try to move labels out 
dat.pca.plot <- autoplot(dat.pca,
                         data = dat1,
                         colour = 'Zone', 
                         shape = 'Site', size = 5,  
                         frame = TRUE,
                         loadings = TRUE,
                         loadings.colour = 'black',
                         loadings.label = FALSE,       # turn off default labels
                         loadings.arrow.size = 1)

# Extract loadings
loadings_df <- as.data.frame(dat.pca$rotation[, 1:2])
loadings_df$PC2 <- loadings_df$PC2 * 0.4  # try 0.5–0.8 until balance looks right
loadings_df$varname <- rownames(loadings_df)

# Add repelled labels
dat.pca.plot +
  geom_text_repel(data = loadings_df,
                  aes(x = PC1, y = PC2, label = varname),
                  size = 5,
                  color = 'black',
                  box.padding = 0.01,        # default is 0.25
                  point.padding = 0,     # default is 0.5
                  max.overlaps = Inf,       # show all labels
                  #force = 0.1, 
                  min.segment.length = 0.1,
                  segment.color = NA)+ #,
                  #force_pull = 0.1) + # avoid overly long lines
  scale_shape_manual(values = c(16,17,15,18)) + 
  scale_fill_manual(values = compass_coolors2) + 
  scale_color_manual(values = compass_coolors2) + 
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2), 
        legend.title= element_blank(), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) #+ coord_cartesian(ylim = c(-0.3, 0.3))


#determine the importance of different vars to axes
#install.packages(c("FactoMineR", "factoextra"))

library("FactoMineR")
library("factoextra")

res.pca <- PCA(dat[,c(5:9)], graph = TRUE)

var <- get_pca_var(res.pca)
var

# Contributions to the principal components
head(var$contrib)


#cap plot
#capscale(formula, data, distance = "euclidean", sqrt.dist = FALSE,
       #  comm = NULL, add = FALSE,  dfun = vegdist, metaMDSdist = FALSE,
     #    na.action = na.fail, subset = NULL, ...)

#######################################################

#####
##DNF 
#####

################### All Rates vs. Soil Moisture - not in manuscript, but could be supplemental ############

SM <- ggplot(dat, aes(x=Percent_Water_WetSoil, y=DNF_Rate_g_hr, col=Zone, shape=Site)) +
  geom_point(size=4) + 
  ylab("DNF Rate") + 
  xlab("Percent Water (%)") +
  ggtitle(" ") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        #legend.position = c(0.15, 0.8), 
        legend.title= element_blank()) #+  
SM
#####################################################################

######## IN MANUSCRIPT - DNF vs. Soil Moisture #############################

#correlations
#lm1 <- lm(trup$DNF_Rate_g_hr ~ trup$Percent_Water_WetSoil)
#summary(lm1)   #R2 = 0.38   #p-val = 0.00083
cor( trup$Percent_Water_WetSoil, trup$DNF_Rate_g_hr , method='pearson')  #0.636033

#lm2 <- lm(wc$DNF_Rate_g_hr ~ wc$Percent_Water_WetSoil)
#summary(lm2)   #R2 = 0.84   #p-val = 1.898e-05
cor( wc$Percent_Water_WetSoil, wc$DNF_Rate_g_hr , method='pearson')  #-0.9227725

SM1 <- ggplot(trup, aes(x=Percent_Water_WetSoil, y=DNF_Rate_g_hr))+ # , col=Zone, shape=Site)) +
  geom_point(size=4) + 
  ylim(0,23) +
  labs(y=expression("Denitrification Rate (nmoles N "[2]*"-N g"^-1*" h"^-1*")")) +
  xlab("Percent Water (%)") +
  ggtitle("A") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        #legend.position = c(0.15, 0.8), 
        #legend.title= element_blank(),  
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) +
  geom_smooth(method='lm', formula= y~x, color="black", se=FALSE)+
  annotate(geom="text", x=15, y=22, label="r = 0.64",
           color="black", fontface="italic")

SM1 

SM2 <- ggplot(wc, aes(x=Percent_Water_WetSoil, y=DNF_Rate_g_hr))+ #, col=Zone, shape=Site)) +
  geom_point(size=4) + 
  ylim(0,23) + 
  labs(y=expression("Denitrification Rate (nmoles N "[2]*"-N g"^-1*" h"^-1*")")) + 
  xlab("Percent Water (%)") +
  ggtitle("B") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        #legend.position = c(0.15, 0.8), 
        legend.title= element_blank(),  
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) +
  geom_smooth(method='lm', formula= y~x, color="black", se=FALSE)+
  annotate(geom="text", x=85, y=22, label= "r = -0.92",
           color="black") #+  
SM2


All_SM <- ggarrange(SM1, SM2, ncol=2, nrow=1)
All_SM

########################################################

############## All DNF Rates vs. KCl DIN - Not a strong relationship at all R2=0.04 ####################

DIN <- ggplot(dat, aes(x=Soil.KCl.DIN..uM., y=DNF_Rate_g_hr, col=Zone, shape=Site)) +
  geom_point(size=4) + 
  ylab("DNF Rate") + 
  xlab("Soil Kcl") +
  ggtitle(" ") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        #legend.position = c(0.15, 0.8), 
        legend.title= element_blank()) #+  
DIN

dat$Percent_OM_WetSoil

lm1 <- lm(dat$DNF_Rate_g_hr~dat$Soil.KCl.DIN..uM.)
summary(lm1)
###########################################################

################# IN MANUSCRIPT - DNF vs. OM ###########################

#correlations
#om1 <- lm(trup$DNF_Rate_g_hr ~ trup$Percent_OM_WetSoil)
#summary(om1)   #R2 = 0.37  #p-val = 0.00095
cor( trup$Percent_OM_WetSoil, trup$DNF_Rate_g_hr , method='pearson')  #0.6308466

#om2 <- lm(wc$DNF_Rate_g_hr ~ wc$Percent_OM_WetSoil)
#summary(om2)   #R2 = 0.74  #p-val = 0.00021
cor( wc$Percent_OM_WetSoil, wc$DNF_Rate_g_hr , method='pearson')  #-0.8730773


## OM
OM1 <- ggplot(trup, aes(x=Percent_OM_WetSoil, y=DNF_Rate_g_hr))+ #, col=Zone, shape=Site)) +
  geom_point(size=4) + 
  ylim(0,23)+
  labs(y=expression("Denitrification Rate (nmoles N "[2]*"-N g"^-1*" h"^-1*")"))  + 
  xlab("Soil Organic Material (%)") +
  ggtitle("A") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        #legend.position = c(0.15, 0.8), 
        legend.title= element_blank(), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) +
  geom_smooth(method='lm', formula= y~x, color="black", se=FALSE)+
  annotate(geom="text", x=30, y=22, label="r = 0.63",
           color="black", fontface="italic")
OM1

OM2 <- ggplot(wc, aes(x=Percent_OM_WetSoil, y=DNF_Rate_g_hr))+ #, col=Zone, shape=Site)) +
  geom_point(size=4) + 
  ylim(0,23)+
  labs(y=expression("Denitrification Rate (nmoles N "[2]*"-N g"^-1*" h"^-1*")"))  + 
  xlab("Soil Organic Material (%)") +
  ggtitle("B") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        #legend.position = c(0.15, 0.8), 
        legend.title= element_blank(), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) +
  geom_smooth(method='lm', formula= y~x, color="black", se=FALSE)+
  annotate(geom="text", x=85, y=22, label="r = -0.87",
           color="black", fontface="italic")
OM2


All_OM <- ggarrange(OM1, OM2, ncol=2, nrow=1)
All_OM

############################################################################################


#####
##DNRA
#####

################### All Rates vs. Soil Moisture - not in manuscript, but could be supplemental ############

SMd <- ggplot(dat, aes(x=Percent_Water_WetSoil, y=DNRA_Rate_g_hr, col=Zone, shape=Site)) +
  geom_point(size=4) + 
  ylab("DNRA Rate") + 
  xlab("Percent Water (%)") +
  ggtitle(" ") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        #legend.position = c(0.15, 0.8), 
        legend.title= element_blank()) #+  
SMd
#####################################################################

######## SUPP FIG IN MANUSCRIPT - DNRA vs. Soil Moisture #############################

#correlations
#lm1d <- lm(trup$DNRA_Rate_g_hr ~ trup$Percent_Water_WetSoil)
#summary(lm1d)   #R2 =  -0.009003  #p-val = 0.3823
cor( trup$Percent_Water_WetSoil, trup$DNRA_Rate_g_hr , method='pearson')  #-0.1867269

#lm2d <- lm(wc$DNRA_Rate_g_hr ~ wc$Percent_Water_WetSoil)
#summary(lm2d)   #R2 = -0.09299    #p-val = 0.8052
cor( wc$Percent_Water_WetSoil, wc$DNRA_Rate_g_hr , method='pearson')  # 0.07981622


SM1d <- ggplot(trup, aes(x=Percent_Water_WetSoil, y=DNRA_Rate_g_hr))+ # , col=Zone, shape=Site)) +
  geom_point(size=4) + 
  labs(y=expression("DNRA Rate (nmoles N "[2]*"-N g"^-1*" h"^-1*")")) +
  xlab("Percent Water (%)") +
  ggtitle("A") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        #legend.position = c(0.15, 0.8), 
        #legend.title= element_blank(),  
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) +
  geom_smooth(method='lm', formula= y~x, color="black", se=FALSE)+
  annotate(geom="text", x=20, y=6, label="r = -0.19",
           color="black", fontface="italic")

SM1d 

SM2d <- ggplot(wc, aes(x=Percent_Water_WetSoil, y=DNRA_Rate_g_hr))+ #, col=Zone, shape=Site)) +
  geom_point(size=4) + 
  labs(y=expression("DNRA Rate (nmoles N "[2]*"-N g"^-1*" h"^-1*")")) + 
  xlab("Percent Water (%)") +
  ggtitle("B") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        #legend.position = c(0.15, 0.8), 
        legend.title= element_blank(),  
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) +
  geom_smooth(method='lm', formula= y~x, color="black", se=FALSE)+
  annotate(geom="text", x=85, y=6, label= "r = 0.08",
           color="black", fontface="italic") #+  
SM2d

All_SMd <- ggarrange(SM1d, SM2d, ncol=2, nrow=1)
All_SMd

########################################################

############## All DNRA Rates vs. KCl DIN - Not a strong relationship at all R2=0.04 ####################

DINd <- ggplot(dat, aes(x=Soil.KCl.DIN..uM., y=DNRA_Rate_g_hr, col=Zone, shape=Site)) +
  geom_point(size=4) + 
  ylab("DNF Rate") + 
  xlab("Soil Kcl") +
  ggtitle(" ") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        #legend.position = c(0.15, 0.8), 
        legend.title= element_blank()) #+  
DINd

lm1d <- lm(dat$DNRA_Rate_g_hr~dat$Soil.KCl.DIN..uM.)
summary(lm1d)
###########################################################

################### DNRA vs. Soil OM:DIN or NOx - not in manuscript ############

SMNO3d <- ggplot(wc, aes(x=WetOM_SoilDIN, y=DNRA_Rate_g_hr, col=Zone, shape=Site)) +
  geom_point(size=4) + 
  ylab("DNRA Rate") + 
  xlab("Percent Water (%)") +
  ggtitle(" ") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        #legend.position = c(0.15, 0.8), 
        legend.title= element_blank()) #+  
SMNO3d

#dat$WetOM_SoilNOx
#####################################################################


################# Potentially IN MANUSCRIPT - DNF vs. OM ###########################

#correlations
#om1d <- lm(trup$DNRA_Rate_g_hr ~ trup$Percent_OM_WetSoil)
#summary(om1d)   #R2 = -0.005752  #p-val =  0.3615
cor( trup$Percent_OM_WetSoil, trup$DNRA_Rate_g_hr , method='pearson')  #-0.1948746

#om2d <- lm(wc$DNRA_Rate_g_hr ~ wc$Percent_OM_WetSoil)
#summary(om2d)   #R2 = -0.09931   #p-val = 0.9383
cor( wc$Percent_OM_WetSoil, wc$DNRA_Rate_g_hr , method='pearson')  #0.02510639


## OM
OM1d <- ggplot(trup, aes(x=Percent_OM_WetSoil, y=DNRA_Rate_g_hr))+ #, col=Zone, shape=Site)) +
  geom_point(size=4) + 
  labs(y=expression("DNRA Rate (nmoles N "[2]*"-N g"^-1*" h"^-1*")"))  + 
  xlab("Soil Organic Material (%)") +
  ggtitle("A") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        #legend.position = c(0.15, 0.8), 
        legend.title= element_blank(), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) +
  geom_smooth(method='lm', formula= y~x, color="black", se=FALSE)+
  annotate(geom="text", x=30, y=6.5, label="r = -0.19",
           color="black", fontface="italic")
OM1d

OM2d <- ggplot(wc, aes(x=Percent_OM_WetSoil, y=DNRA_Rate_g_hr))+ #, col=Zone, shape=Site)) +
  geom_point(size=4) + 
  labs(y=expression("DNRA Rate (nmoles N "[2]*"-N g"^-1*" h"^-1*")"))  + 
  xlab("Soil Organic Material (%)") +
  ggtitle("B") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        #legend.position = c(0.15, 0.8), 
        legend.title= element_blank(), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) +
  geom_smooth(method='lm', formula= y~x, color="black", se=FALSE)+
  annotate(geom="text", x=85, y=6.5, label="r = 0.025",
           color="black")
OM2d

All_OMd <- ggarrange(OM1d, OM2d, ncol=2, nrow=1)
All_OMd

############################################################################################


###### 
# TOtal NO3 reduction
#######

################## Ratio vs. Total N Reduction Figure ######################################
#read in Ratio and Total N reduction data 
rat <- read.csv("DNF_DNRA_Ratio_Percents.csv")
head(rat)

rat$WetOM_SoilDIN <- dat$WetOM_SoilDIN
rat$WetOM_SoilNOx <- dat$WetOM_SoilNOx 

rat <- rat[-c(1,2),]

cor(rat$Total_N_Reduction, rat$DNF..DNRA, method='pearson')  #0.2740518

Ratio <- ggplot(rat, aes(x=Total_N_Reduction, y=DNF..DNRA ))+ # , col=Zone, shape=Site)) +
  geom_point(size=4) + 
  labs(x=expression("Total Nitrate Reduction Rate (nmoles N "[2]*"-N g"^-1*" h"^-1*")")) +
  ylab("DNF : DNRA") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        #legend.position = c(0.15, 0.8), 
        #legend.title= element_blank(),  
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) 
Ratio 


Ratio <- ggplot(rat, aes(x=WetOM_SoilNOx, y=DNF..DNRA ))+ # , col=Zone, shape=Site)) +
  geom_point(size=4) + 
  labs(x=expression("Soil Organic Content : Soil KCl NOx")) +
  ylab("DNF : DNRA") +
  theme_classic() + 
  theme(panel.background = element_rect(colour = "black", size=1.2, fill=NA), 
        #legend.position = c(0.15, 0.8), 
        #legend.title= element_blank(),  
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) + 
  #geom_smooth(method="lm") 
Ratio 

#exp.model <-lm(DNF..DNRA ~ exp(WetOM_SoilNOx), rat)
#looks exponential, but because the data includes numbers <1 I can't do an expt. model 

############################################################################################


################## CORR PLOT - Need to change labels #####################################
head(dat)
corr <- dat[ ,c(5:9, 32:35, 37:39) ]
head(corr)

colnames(corr) <- c("Denitrification Rate (g/hr)", "DNRA Rate (g/hr)", "Soil pH", "% Water", 
                    "% Organic Matter", "Extractable Sulfate (mM)", "Extractable Chloride (mM)", 
                    "Soil Salinity (psu)", "Soil Conductivity (μS/cm)", "Extractable Ammonium (μM)", 
                    "Extractable NOx (μM)", "Extractable DIN (μM)")

library("Hmisc")
res1 <- rcorr(as.matrix(corr))
res1

mydata.coeff = res1$r
mydata.p = res1$P


library(corrplot)
corrplot(mydata.coeff, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


#Just TR and UP
head(trup)
corr1 <- trup[ ,c(5:9, 32:35, 37:39) ]
head(corr1)

res2 <- rcorr(as.matrix(corr1))
res2

mydata.coeff1 = res2$r
mydata.p1 = res2$P


corrplot(mydata.coeff1, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


#just wetland
head(wc)
corr2 <- wc[ ,c(5:9, 32:35, 37:39) ]
head(corr2)

res3 <- rcorr(as.matrix(corr2))
res3

mydata.coeff2 = res3$r
mydata.p2 = res3$P


corrplot(mydata.coeff2, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

##########################################################################


