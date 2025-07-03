###########################
## Compass N Trial N2 Calc
###########################

######################### SET UP ##########################
#read in raw data 
library(here)

raw <- read.csv(here("IRMS Data/Raw IRMS Data Files", "COMPASS_Synoptic_NExpt_250uL_loop_raw1.csv"))
head(raw)

#pull out only the columns that you need 

raw1 <- as.data.frame(raw$Identifier.1)
raw1 <- cbind(raw1, raw$Peak.Nr, raw$Area.28, raw$Area.29, raw$Area.30, raw$d.15N.14N)
colnames(raw1) <- c("Sample Name", "Peak #", "Area 28", "Area 29", "Area 30", "Del Value")


raw2 <- raw1[which(raw1$`Peak #`==5), ]

ID2 <- substr(raw2$`Sample Name`, 0, 3)

raw2 <- cbind(raw2, ID2)
head(raw2)

STDs <- raw2[which(raw2$`ID2` == "STD"), ]
STDs <- STDs[order(STDs$`Area 28`),]

head(STDs)
#you need to check the order of your stds to make sure there was nothing weird 

library(dplyr)
require(dplyr)

#this should allow you to take out any samples that were standards or blanks 
# **** this only works if your blanks start with the word blank and your air stds start wtih the word air
samples <- raw2 %>% filter(raw2$ID2 != "STD")
samples <- samples %>% filter(samples$ID2 != "Bla")

head(samples)
##########################################################

############## Next steps ##########
#Exetainer (vessel) headspace volume (uL) = Total volume - slurry volume  
# **** 1mL / 1g soil + 1mL water+ 0.1mL 15NO3 + 0.5mL ZnCl = 2.6***
#12 - 2.6 = 9.4mL * 1000 = 9400 uL 
hdspc <- 9400

#GC Sample Loop size (uL) ****MAY NEED TO CHANGE****
sampleloop <- 250 

#Mole fractions of 14,15N and 28,29,30N2 
#SHOULD NOT EVER NEED TO CHANGE THESE 
N14 <- 0.996337
N15 <- 0.003663
N28 <- 0.992687418
N29 <- 0.007299165
N30 <- (0.003663*0.003663)
ratioN29N28 <- (N29/N28)
ratioN30N28 <- (N30/N28)

#Enter P in atm ****MAY NEED TO CHANGE****
P <- 1 

#P in kPa 
PkPa <- (101.325*P)

#Ideal Gas Law 
#n (moles)
nmoles <- 1
#R (J/mol K)
R <- 8.3145

#Enter T in deg C ***MAY NEED TO CHANGE THIS *****
Temp <- 21 
TempK <- (273+Temp)

#Volume total in Liters/mole or uL/umole 
Vtot <- ((nmoles * R * TempK)/PkPa)
###################################################


############### Air Standards ###############
#input the size of your air standards (uL air)

#Take out top two rows that are blanks 
STDs <- STDs[-c(1:2), ]
head(STDs)

uLair <- c(5,5,10,10,20,20,30, 30)

#leave this next section be
STDs <- cbind(STDs, uLair)
STDs <- as.data.frame(STDs)
head(STDs)

uLN2 <- (STDs$uLair * 0.78)
STDs <- cbind(STDs, uLN2)

massN2moles <- (((STDs$uLN2 / hdspc) * sampleloop) / Vtot) *1000
STDs <- cbind(STDs, massN2moles)

areaN28 <- STDs$`Area 28`
areaN29 <- STDs$`Area 29`
areaN30 <- STDs$`Area 30`

Totalarea <- (STDs$`Area 28` + STDs$`Area 29` + STDs$`Area 30`)
STDs <- cbind(STDs, Totalarea)

mass28nmoles <- ((N14 ^ 2) * massN2moles)
STDs <- cbind(STDs, mass28nmoles)

mass29nmoles <- ((2 * (N14 * N15)) * massN2moles)
STDs <- cbind(STDs, mass29nmoles)

mass30nmoles <- ((N15 ^ 2) * massN2moles)
STDs <- cbind(STDs, mass30nmoles)

totalN2mass <- (mass28nmoles + mass29nmoles + mass30nmoles)
STDs <- cbind(STDs, totalN2mass)

#this next column should all equal zero 
totalmasscheck <- (totalN2mass - massN2moles)
STDs <- cbind(STDs, totalmasscheck)
head(STDs)

N29N28mass <- (mass29nmoles / mass28nmoles)
STDs <- cbind(STDs, N29N28mass)
AvgN29N28 <- (mean(STDs$N29N28mass))
AvgN29N28

N30N28mass <- (mass30nmoles / mass28nmoles)
STDs <- cbind(STDs, N30N28mass)
AvgN30N28 <- (mean(STDs$N30N28mass))
AvgN30N28

N29N28area <- (areaN29 / areaN28)
STDs <- cbind(STDs, N29N28area)

STDs <- cbind(STDs, ratioN30N28)

N30N28area <- (ratioN30N28 * areaN28)
STDs <- cbind(STDs, N30N28area)

STDs <- cbind(STDs, mass30nmoles)

#Argon areas - don't think you should have to change (mass 40 + 36)
#Argon <- c(0.020649991, 0.15188884, 0.029820488,  0.069320648, 0.146066591, 0.04145506, 0)
#STDs <- cbind(STDs, Argon)

###28 STD Curve 
par(mfrow=c(1,1))
# basic straight line of fit
fit <- lm(STDs$mass28nmoles ~ STDs$`Area 28`)
co <- coef(fit)

#plot points and line 
plot(STDs$`Area 28`, STDs$mass28nmoles, xlab = "28 area", ylab = "28 mass nmoles N2", pch=16, col=1)
abline(fit, col="red", lwd=2)
a <- summary(fit)
a
Slope28 = a$coefficients[2,1]
Slope28

###29 STD Curve 
# basic straight line of fit
fit <- lm(STDs$mass29nmoles ~ STDs$`Area 29`)
co <- coef(fit)

#plot points and line 
plot(STDs$`Area 29`, STDs$mass29nmoles, xlab = "29 area", ylab = "29 mass nmoles N2", pch=16, col=1)
abline(fit, col="blue", lwd=2)
a <- summary(fit)
a
Slope29 = a$coefficients[2,1]
Slope29

###30 STD Curve 
# basic straight line of fit
fit <- lm(STDs$mass30nmoles ~ STDs$N30N28area)
co <- coef(fit)

#plot points and line 
plot(STDs$N30N28area, STDs$mass30nmoles, xlab = "N2 only 30 area", ylab = "30 mass nmoles N2", pch=16, col=1)
abline(fit, col="green", lwd=2)
a <- summary(fit)
a
Slope30 = a$coefficients[2,1]
Slope30

###Total N2 Curve 
# basic straight line of fit
fit <- lm(STDs$totalN2mass ~ STDs$Totalarea)
co <- coef(fit)

#plot points and line 
plot(STDs$Totalarea, STDs$totalN2mass, xlab = "Total Area", ylab = "Total N2 mass nmoles", pch=16, col=1)
abline(fit, col="purple", lwd=2)
a <- summary(fit)
a
Slopetotal = a$coefficients[2,1]
Slopetotal


Slopes <- c(Slope28, Slope29, Slope30, Slopetotal)
head(Slopes)
##########################################################

############# Sample Table Set up ###############
#we determiend our samples df earlier 
head(samples)

#can also add Total Argon area (36+40) here if you have it 

#can also add wet sed (g) mass here
no.samples <- length(samples$`Sample Name`)  #gives you the number of samples you have 
samplemass <- (rep(1, no.samples))
samples <- cbind(samples, samplemass)

#need to add atom % excess of added 15N substrate 
# in our lab this is usually 99.9 
atom15Nexcess <- (rep(99.9, no.samples))
spikeconc <- (rep(1, no.samples))  #in mmol
spikevol <- (rep(0.1, no.samples))   #in mL

mass15Nadded <- (1000 * spikeconc)*spikevol

samples <- cbind(samples, atom15Nexcess, spikeconc, spikevol, mass15Nadded)

#Nox Box 
porewaterNO3 <- (rep(0, no.samples))   #in uM
porewatervol <- (rep(0, no.samples))    #in mL 

massambient <- (porewaterNO3 * porewatervol)
samples <- cbind(samples, porewaterNO3, porewatervol, massambient)

tracerdilratio <- ((samples$atom15Nexcess * samples$mass15Nadded) / (samples$mass15Nadded + samples$massambient))
Fn <- (tracerdilratio/100)
Fn1 <- (Fn ^ -1)
Fn2 <- (Fn ^ -2)

samples <- cbind(samples, tracerdilratio, Fn, Fn1, Fn2)

head(samples)
###############################################

################# Calculations ###########
#start calcluations 
rawN28mass <- (samples$`Area 28` * Slope28)
rawN29mass <- (samples$`Area 29` * Slope29)
rawN30mass <- (samples$`Area 30` * Slope30)
samples <- cbind(samples, rawN28mass, rawN29mass, rawN30mass)

#The next few steps assume that the 28 mass is all background 
blank29mass <- (AvgN29N28 * samples$rawN28mass)
blank30mass <- (AvgN30N28 * samples$rawN28mass)
samples <- cbind(samples, blank29mass, blank30mass)

correct29mass <- (samples$rawN29mass - samples$blank29mass)
correct30mass <- (samples$rawN30mass - samples$blank30mass)
samples <- cbind(samples, correct29mass, correct30mass)

#continuing based on 28 as background which is sometimes okay 

#next step takes corrected 29 and 30N2 per gram of wet sediment used in the incubation 
correct29perg <- (samples$correct29mass/samples$samplemass)
correct30perg <- (samples$correct30mass/samples$samplemass)
samples <- cbind(samples, correct29perg, correct30perg)

#next calculate mole fraction of 15N using delta value 

MF15N <- ( ( (samples$`Del Value`/1000+1) *0.0036765) / (1 + ( (samples$`Del Value`/1000+1) *0.0036765)))
samples <- cbind(samples, MF15N)

MFexcess <- (samples$MF15N - 0.0036630329)
samples <- cbind(samples, MFexcess)

N2Nmassnmoles <- ((samples$`Area 28` + samples$`Area 29` + samples$`Area 30`) * Slopetotal)
samples <- cbind(samples, N2Nmassnmoles)


#preferred and calculated from Mole Fraction: 
#not dependent on 28 bkgd assumption 
excess29Nperg <- (((2*(samples$MF15N)*(1-samples$MF15N))*samples$N2Nmassnmoles) - ((2*(N15)*(1-N15))*samples$N2Nmassnmoles)) / samples$samplemass
samples <- cbind(samples, excess29Nperg)



#Just repull you values to the end for 29 and 30 
#here we identify that this is the excess mass 29 and 30 per gram of sediment per sample loop 

excessN29pergperlp <- samples$excess29Nperg   # in nmoles 
excessN30pergperlp <- samples$correct30perg   # in nmoles

samples <- cbind(samples, excessN29pergperlp, excessN30pergperlp)


#### Tracer diluion corrected Anammox 29 nmoles g sed-1 per sample loop 
tracerdilcorrectedN29 <- (samples$Fn1 * ((samples$excessN29pergperlp + (2* (1-samples$Fn1)) *samples$excessN30pergperlp)))

#### Tracer diluion corrected Anammox 30 nmoles g sed-1 per sample loop 
tracerdilcorrectedN30 <- (samples$excessN30pergperlp * samples$Fn2)

samples <- cbind(samples, tracerdilcorrectedN29, tracerdilcorrectedN30)

#### Corrected final values for 29N2 and 30N2 g-1 

corrected29N2 <- (samples$tracerdilcorrectedN29 * (hdspc/sampleloop))

corrected30N2 <- (samples$tracerdilcorrectedN30 * (hdspc/sampleloop))

samples <- cbind(samples, corrected29N2, corrected30N2)

output <- cbind(samples$`Sample Name`, samples$`Del Value`, samples$`Area 28`, samples$`Area 29`, samples$`Area 30`, samples$corrected29N2, samples$corrected29N2)
colnames(output) <- c("Sample Name", "Del Value", "28N2 Area", "29N2 Area", "30N2 Area", "Corrected 29N2 g-1", "Corrected 30N2 g-1")

finaloutput <- samples$`Sample Name`
finaloutput <- as.data.frame(finaloutput)
finaloutput<- cbind(finaloutput, samples$corrected29N2, samples$corrected30N2)
colnames(finaloutput) <- c("Sample Name", "Corrected 29N2 g-1", "Corrected 30N2 g-1")
finaloutput <- as.data.frame(finaloutput)
head(finaloutput)
######################################################################################


#################### Write out Processed Data Files ##############

write.csv(samples, file = here("Processed Data","COMPASS_NExpt_N2Calc_FullOutput_SWH1.csv"))  #this will give you everything 

#write.csv(output, file="output.csv")  #this is if you want the samples, areas, del value, and corrected 29 & 30 N2
write.csv(finaloutput, file = here("Processed Data","COMPASS_NExpt_N2Calc_Output_SHW1.csv"))  #this will give you just the sample names and the corrected 29 & 30

#######################################################################


################# Graphs ###########
library(ggplot2)

#simple bar graph 29 N2 
ggplot(finaloutput, aes(x=finaloutput$`Sample Name`, y=finaloutput$`Corrected 29N2 g-1`)) + 
  geom_bar(stat = "identity") + theme_classic() + labs(title = "29N2 Raw Data", x="Sample Names", y="Corrected 29N2 g sedment-1") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#simple bar graph 30 N2 
ggplot(finaloutput, aes(x=finaloutput$`Sample Name`, y=finaloutput$`Corrected 30N2 g-1`)) + 
  geom_bar(stat = "identity") + theme_classic()+ labs(title = "30N2 Raw Data", x="Sample Names", y="Corrected 30N2 g sedment-1") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

############



