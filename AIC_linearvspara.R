####AKAIKE INFORMATION CRITERION (AIC) TEST CODE####

####PROJECT SUMMARY####
#We conducted linear regression analyses by fitting both linear and quadratic models to the data, 
#comparing element/Ca ratios against carbonate parameters. This was done to identify any significant
#relationships between the element/Ca ratios against carbonate parameters and to understand the 
#nature of the relationship. Scatterplots were generated using the ggplot2, ggtext, and Rmisc 
#packages and multiple plots were combined for organisms that displayed five or more significant 
#relationships between both linear and quadratic regressions. We then used an AIC test to choose
#the better fit between the linear and quadratic models.

####SECTION 1: IMPORTING AND CLEANING UP DATA####
setwd('~/Desktop/Biomineralization/Data')
#importing csv file spreadsheet containing all data
mydata = read.csv("Updated_Compiled Sheet_Trace Elem_Ries_CR_Liu_d11B_Correlation.csv")  
#LIST OF ORGANISMS TESTED
#conch
#limpet
#periwinkle
#whelk
#coralline red alga
#temperate urchin
#tropical urchin
#serpulid worm
#temperate coral
#american oyster
#bay scallop
#blue mussel
#hard clam (quahog)
#soft clam
#american lobster
#blue crab
#gulf shrimp
#Halimeda alga

#creating a subset of just data from specified organisms
data <- subset(mydata, Organism == "american oyster")

#List of carbonate system parameters
CRate<-data$CalcRate
carbion<-data$carbion
Alk<-data$Alk
pH<-data$pH
pHcf<-data$pH_cf

#List of Elements
Li<-data$Li_Ca
B<-data$B_Ca
Mg<-data$Mg_Ca
Zn<-data$Zn_Ca
Sr<-data$Sr_Ca
Cd<-data$Cd_Ca
Ba<-data$Ba_Ca
U<-data$U_Ca
Na<-data$Na_Ca

####SECTION 2: CALCULATING AIC AND FINDING BEST FIT MODEL####
####CRate####
##Li/Ca##
linear = lm(Li~CRate) #creating linear model
parabolic = lm(Li~CRate+ I(CRate^2)) #creating quadratic model
fits = AIC(linear, parabolic) #creates array with df and AIC
bestfit=which.min(fits[,2]) #[,2] pulls values from second column of array and returns lowest AIC
bestfit
CRate_Li_bestfit = rownames(fits)[bestfit] #saves which model is associated with the lowest AIC value
fits #display AIC values of both models (to check)
summary(linear) #shows F-statistic, p-value, and adjusted R-squared values
summary(parabolic)

##B/Ca##
linear = lm(B~CRate)
parabolic = lm(B~CRate+ I(CRate^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
CRate_B_bestfit = rownames(fits)[bestfit]
fits
summary(linear)

##Mg/Ca##
linear = lm(Mg~CRate)
parabolic = lm(Mg~CRate+ I(CRate^2))
fits = AIC(linear, parabolic) 
bestfit=which.min(fits[,2]) 
bestfit
CRate_Mg_bestfit = rownames(fits)[bestfit] 
fits

##Zn/Ca##
linear = lm(Zn~CRate)
parabolic = lm(Zn~CRate+ I(CRate^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
CRate_Zn_bestfit = rownames(fits)[bestfit]
fits
summary(linear)
summary(parabolic)

##Cd/Ca##
linear = lm(Cd~CRate)
parabolic = lm(Cd~CRate+ I(CRate^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
CRate_Cd_bestfit = rownames(fits)[bestfit]
fits
summary(linear)
summary(parabolic)

##Sr/Ca##
linear = lm(Sr~CRate)
parabolic = lm(Sr~CRate+ I(CRate^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
CRate_Sr_bestfit = rownames(fits)[bestfit]
fits
summary(linear)
summary(parabolic)

##Ba/Ca##
linear = lm(Ba~CRate)
parabolic = lm(Ba~CRate+ I(CRate^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
CRate_Ba_bestfit = rownames(fits)[bestfit]
fits
summary(linear)
summary(parabolic)

##U/Ca##
linear = lm(U~CRate)
parabolic = lm(U~CRate+ I(CRate^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
CRate_U_bestfit = rownames(fits)[bestfit]
fits
summary(linear)
summary(parabolic)

##Na/Ca##
linear = lm(Na~CRate)
parabolic = lm(Na~CRate+ I(CRate^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
CRate_Na_bestfit = rownames(fits)[bestfit]
fits

####Carbion####
##Li/Ca##
linear = lm(Li~carbion)
parabolic = lm(Li~carbion+ I(carbion^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
carbion_Li_bestfit = rownames(fits)[bestfit]
fits
summary(linear)
summary(parabolic)

##B/Ca##
linear = lm(B~carbion)
parabolic = lm(B~carbion+ I(carbion^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
carbion_B_bestfit = rownames(fits)[bestfit]
fits
summary(linear)
summary(parabolic)

##Mg/Ca##
linear = lm(Mg~carbion)
parabolic = lm(Mg~carbion+ I(carbion^2))
fits = AIC(linear, parabolic) 
bestfit=which.min(fits[,2]) 
bestfit
carbion_Mg_bestfit = rownames(fits)[bestfit]
fits
summary(parabolic)

##Zn/Ca##
linear = lm(Zn~carbion)
parabolic = lm(Zn~carbion+ I(carbion^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
carbion_Zn_bestfit = rownames(fits)[bestfit]
fits
summary(linear)
summary(parabolic)

##Cd/Ca##
linear = lm(Cd~carbion)
parabolic = lm(Cd~carbion+ I(carbion^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
carbion_Cd_bestfit = rownames(fits)[bestfit]
fits

##Sr/Ca##
linear = lm(Sr~carbion)
parabolic = lm(Sr~carbion+ I(carbion^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
carbion_Sr_bestfit = rownames(fits)[bestfit]
fits
summary(parabolic)
summary(linear)

##Ba/Ca##
linear = lm(Ba~carbion)
parabolic = lm(Ba~carbion+ I(carbion^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
carbion_Ba_bestfit = rownames(fits)[bestfit]
fits
summary(linear)
summary(parabolic)

##U/Ca##
linear = lm(U~carbion)
parabolic = lm(U~carbion+ I(carbion^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
carbion_U_bestfit = rownames(fits)[bestfit]
fits

##Na/Ca##
linear = lm(Na~carbion)
parabolic = lm(Na~carbion+ I(carbion^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
carbion_Na_bestfit = rownames(fits)[bestfit]
fits

####Alkalinity####
##Li/Ca##
linear = lm(Li~Alk)
parabolic = lm(Li~Alk+ I(Alk^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
Alk_Li_bestfit = rownames(fits)[bestfit]
fits
summary(linear)
summary(parabolic)

##B/Ca##
linear = lm(B~Alk)
parabolic = lm(B~Alk+ I(Alk^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
Alk_B_bestfit = rownames(fits)[bestfit]
fits
summary(linear)
summary(parabolic)

##Mg/Ca##
linear = lm(Mg~Alk)
parabolic = lm(Mg~Alk+ I(Alk^2))
fits = AIC(linear, parabolic) 
bestfit=which.min(fits[,2]) 
bestfit
Alk_Mg_bestfit = rownames(fits)[bestfit] 
fits
summary(linear)
summary(parabolic)

##Zn/Ca##
linear = lm(Zn~Alk)
parabolic = lm(Zn~Alk+ I(Alk^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
Alk_Zn_bestfit = rownames(fits)[bestfit]
fits
summary(linear)
summary(parabolic)


##Cd/Ca##
linear = lm(Cd~Alk)
parabolic = lm(Cd~Alk+ I(Alk^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
Alk_Cd_bestfit = rownames(fits)[bestfit]
fits

##Sr/Ca##
linear = lm(Sr~Alk)
parabolic = lm(Sr~Alk+ I(Alk^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
Alk_Sr_bestfit = rownames(fits)[bestfit]
fits
summary(linear)
summary(parabolic)

##Ba/Ca##
linear = lm(Ba~Alk)
parabolic = lm(Ba~Alk+ I(Alk^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
Alk_Ba_bestfit = rownames(fits)[bestfit]
fits
summary(parabolic)
summary(linear)

##U/Ca##
linear = lm(U~Alk)
parabolic = lm(U~Alk+ I(Alk^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
Alk_U_bestfit = rownames(fits)[bestfit]
fits

##Na/Ca##
linear = lm(Na~Alk)
parabolic = lm(Na~Alk+ I(Alk^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
Alk_Na_bestfit = rownames(fits)[bestfit]
fits
summary(linear)
summary(parabolic)

####Seawater pH####
##Li/Ca##
linear = lm(Li~pH)
parabolic = lm(Li~pH+ I(pH^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
pH_Li_bestfit = rownames(fits)[bestfit]
fits
summary(linear)

##B/Ca##
linear = lm(B~pH)
parabolic = lm(B~pH+ I(pH^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
pH_B_bestfit = rownames(fits)[bestfit]
fits
summary(linear)
summary(parabolic)

##Mg/Ca##
linear = lm(Mg~pH)
parabolic = lm(Mg~pH+ I(pH^2))
fits = AIC(linear, parabolic) 
bestfit=which.min(fits[,2])
bestfit
pH_Mg_bestfit = rownames(fits)[bestfit]
fits

##Zn/Ca##
linear = lm(Zn~pH)
parabolic = lm(Zn~pH+ I(pH^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
pH_Zn_bestfit = rownames(fits)[bestfit]
fits
summary(linear)
summary(parabolic)

##Cd/Ca##
linear = lm(Cd~pH)
parabolic = lm(Cd~pH+ I(pH^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
pH_Cd_bestfit = rownames(fits)[bestfit]
fits

##Sr/Ca##
linear = lm(Sr~pH)
parabolic = lm(Sr~pH+ I(pH^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
pH_Sr_bestfit = rownames(fits)[bestfit]
fits

##Ba/Ca##
linear = lm(Ba~pH)
parabolic = lm(Ba~pH+ I(pH^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
pH_Ba_bestfit = rownames(fits)[bestfit]
fits
summary(linear)
summary(parabolic)


##U/Ca##
linear = lm(U~pH)
parabolic = lm(U~pH+ I(pH^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
pH_U_bestfit = rownames(fits)[bestfit]
fits

##Na/Ca##
linear = lm(Na~pH)
parabolic = lm(Na~pH+ I(pH^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
pH_Na_bestfit = rownames(fits)[bestfit]
fits
summary(linear)

####Boron-derived pH####
##Li/Ca##
linear = lm(Li~pHcf)
parabolic = lm(Li~pHcf+ I(pHcf^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
pHcf_Li_bestfit = rownames(fits)[bestfit]
fits
summary(parabolic)
summary(linear)

##B/Ca##
linear = lm(B~pHcf)
parabolic = lm(B~pHcf+ I(pHcf^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
pHcf_B_bestfit = rownames(fits)[bestfit]
fits
summary(parabolic)
summary(linear)

##Mg/Ca##
linear = lm(Mg~pHcf)
parabolic = lm(Mg~pHcf+ I(pHcf^2))
fits = AIC(linear, parabolic) 
bestfit=which.min(fits[,2])
bestfit
pHcf_Mg_bestfit = rownames(fits)[bestfit]
fits
summary(linear)

##Zn/Ca##
linear = lm(Zn~pHcf)
parabolic = lm(Zn~pHcf+ I(pHcf^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
pHcf_Zn_bestfit = rownames(fits)[bestfit]
fits
summary(parabolic)

##Cd/Ca##
linear = lm(Cd~pHcf)
parabolic = lm(Cd~pHcf+ I(pHcf^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
pHcf_Cd_bestfit = rownames(fits)[bestfit]
fits
summary(parabolic)

##Sr/Ca##
linear = lm(Sr~pHcf)
parabolic = lm(Sr~pHcf+ I(pHcf^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
pHcf_Sr_bestfit = rownames(fits)[bestfit]
fits
summary(parabolic)
summary(linear)

##Ba/Ca##
linear = lm(Ba~pHcf)
parabolic = lm(Ba~pHcf+ I(pHcf^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
pHcf_Ba_bestfit = rownames(fits)[bestfit]
fits
summary(parabolic)
summary(linear)

##U/Ca##
linear = lm(U~pHcf)
parabolic = lm(U~pHcf+ I(pHcf^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
pHcf_U_bestfit = rownames(fits)[bestfit]
fits
summary(linear)
summary(parabolic)

##Na/Ca##
linear = lm(Na~pHcf)
parabolic = lm(Na~pHcf+ I(pHcf^2))
fits = AIC(linear, parabolic)
bestfit=which.min(fits[,2])
bestfit
pHcf_Na_bestfit = rownames(fits)[bestfit]
fits
