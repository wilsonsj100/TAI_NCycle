
#################################################### SET UP ####################################################
setwd("S:/Biogeochemistry/People/Wilson (Steph)/Compass N Cycle Experiment/Full Incub. Experiment/TAI_NCycle/COMPASS-FME Supplemental Data")

#set colors 
compass_coolors <- c("#20063B", "#FFBC42", "#419973")
#################################################################################################################

##################################### Soil Moisture @ 10 cm - 2022 ###################################
#let's start with the soil moisture data from 2022 

#now read in the data we need 
MSM22U <- read.csv("MSM_UP_20220901-20220930_L1_v1-0.csv")
GWI22U <- read.csv("GWI_UP_20220901-20220930_L1_v1-0.csv")
GCW22U <- read.csv("GCW_UP_20220901-20220930_L1_v1-0.csv")

MSM22T <- read.csv("MSM_TR_20220901-20220930_L1_v1-0.csv")
GWI22T <- read.csv("GWI_TR_20220901-20220930_L1_v1-0.csv")
#GCW22T <- read.csv("GCW_TR_20220930-20220930_L1_v1-0.csv")

MSM22W <- read.csv("MSM_W_20220901-20220930_L1_v1-0.csv")
GWI22W <- read.csv("GWI_W_20220901-20220930_L1_v1-0.csv")
GCW22W <- read.csv("GCW_W_20220901-20220930_L1_v1-0.csv")
head(MSM22W)

VWC22raw <- rbind(MSM22U, GWI22U, GCW22U, MSM22T, GWI22T,  MSM22W, GWI22W, GCW22W)
head(VWC22raw)

#subset by the soil moisture at 10cm 
VWC22 <-  as.data.frame(subset(VWC22raw, research_name == "soil_vwc_10cm", select = Site:F_OOS))

#remove any data rows that are out of bounds 
VWC22 <- as.data.frame(subset(VWC22, !F_OOB == "1"))
head(VWC22)


#make new columns for transect location based on design_link
#pull the sample ID and separate it by the underscores 
#IDs <- data.frame(do.call('rbind', strsplit(as.character(VWC22$design_link),'-',fixed=TRUE)))
#colnames(IDs) <- c("Analysis_Type" , "ID_No","Site", "Zone")
#head(IDs)

#rejoin them to the dataframe
#alldat <- cbind(IDs, VWC22)
alldat <- VWC22
head(alldat)

alldat$Site <- as.factor(alldat$Site)
levels(alldat$Site)

alldat$Zone <- as.factor(alldat$Plot)
levels(alldat$Zone) <- list(Upland = "UP", Transition = "TR", Wetland = "W")
levels(alldat$Zone)

#msm <- subset(alldat, Site == "MSM")
#msm <- msm[msm$Zone %in% c('Upland', 'Transition'), ]
#levels(msm$Zone)

#plot as a bar plot across the sites 
box <- ggplot()+
  geom_boxplot(data=alldat, aes(x=Zone, y=Value, fill=Zone)) + 
  facet_wrap(~Site, ncol = 2) + 
  #facet_grid(~factor(Site, levels=c("MSM", "GWI"))) +
  theme_classic() + 
  labs(title= "2022", x=" ", y= expression(paste("Soil VWC (m"^3*"/ m"^3*")"))) +
  scale_fill_manual(values=compass_coolors) +
  theme(panel.background = element_rect(colour = "black", linewidth  =1.2), 
        legend.position = "NONE", 
        legend.title= element_blank(), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12), 
        strip.text.x = element_text(size = 12)) 

box

#################################################################################################################

##################################### Soil Moisture @ 10 cm - 2023 ###################################
#let's start with the soil moisture data from 2022 

#now read in the data we need 
MSM23U <- read.csv("MSM_UP_20230901-20230930_L1_v1-0.csv")
GWI23U <- read.csv("GWI_UP_20230901-20230930_L1_v1-0.csv")
GCW23U <- read.csv("GCW_UP_20230901-20230930_L1_v1-0.csv")
SWH23U <- read.csv("SWH_SWAMP_20230901-20230930_L1_v1-0.csv")

MSM23T <- read.csv("MSM_TR_20220901-20220930_L1_v1-0.csv")
GWI23T <- read.csv("GWI_TR_20230901-20230930_L1_v1-0.csv")
GCW23T <- read.csv("GCW_TR_20230901-20230930_L1_v1-0.csv")
SWH23T <- read.csv("SWH_TR_20230901-20230930_L1_v1-0.csv")

MSM23W <- read.csv("MSM_W_20230901-20230930_L1_v1-0.csv")
GWI23W <- read.csv("GWI_W_20230901-20230930_L1_v1-0.csv")
GCW23W <- read.csv("GCW_W_20230901-20230930_L1_v1-0.csv")
SWH23W <- read.csv("SWH_W_20230901-20230930_L1_v1-0.csv")
head(MSM23W)

VWC23raw <- rbind(MSM23U, GWI23U, GCW23U, MSM23T, GWI23T, GCW23T, MSM23W, GWI23W, GCW23W, SWH23U, SWH23T, SWH23W)
head(VWC23raw)

#subset by the soil moisture at 10cm 
VWC23 <-  as.data.frame(subset(VWC23raw, research_name == c("soil_vwc_10cm", "soil_vwc_15cm"), select = Site:F_OOS))

#remove any data rows that are out of bounds 
VWC23 <- as.data.frame(subset(VWC23, !F_OOB == "1"))
head(VWC23)

#make new columns for transect location based on design_link
#pull the sample ID and separate it by the underscores 
#IDs <- data.frame(do.call('rbind', strsplit(as.character(VWC23$design_link),'-',fixed=TRUE)))
#colnames(IDs) <- c("Analysis_Type" , "ID_No","Site", "Zone")
#head(IDs)

#rejoin them to the dataframe
alldat1 <- VWC23
head(alldat1)

alldat1$Site <- as.factor(alldat1$Site)
levels(alldat1$Site)


alldat1$Zone <- as.factor(alldat1$Plot)
levels(alldat1$Zone)
levels(alldat1$Zone) <- list(Upland = "UP", Transition = "TR", Wetland = "W", Upland = "SWAMP")
levels(alldat1$Zone)

#plot as a bar plot across the sites 
box1 <- ggplot()+
  geom_boxplot(data=alldat1, aes(x=Zone, y=Value, fill=Zone)) + 
  facet_wrap(~Site, ncol = 2) + 
  #facet_grid(~factor(Site, levels=c("SWH", "GCW","MSM", "GWI"))) +
  theme_classic() + 
  labs(title= "2023", x=" ", y= expression(paste("Soil VWC (m"^3*"/ m"^3*")"))) +
  scale_fill_manual(values=compass_coolors) +
  #scale_x_discrete(labels=c("Upland", "Transition", "Wetland")) +
  theme(panel.background = element_rect(colour = "black", linewidth  =1.2), 
        legend.position = "NONE", 
        legend.title= element_blank(), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12), 
        strip.text.x = element_text(size = 12)) 

box1


#################################################################################################################

