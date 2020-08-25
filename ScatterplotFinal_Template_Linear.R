####FINAL SCATTERPLOT W/ LINEAR REGREESIONS CODE####

####PROJECT SUMMARY####
#We conducted linear regression analyses by fitting both linear and quadratic models to the data, 
#comparing element/Ca ratios against carbonate parameters. This was done to identify any significant
#relationships between the element/Ca ratios against carbonate parameters and to understand the 
#nature of the relationship. Scatterplots were generated using the ggplot2, ggtext, and Rmisc 
#packages and multiple plots were combined for organisms that displayed five or more significant 
#relationships between both linear and quadratic regressions. We then used an AIC test to choose
#the better fit between the linear and quadratic models.

####INSTALL AND LOAD REQUIRED PACKAGES####
install.packages("ggplot2")
install.packages("ggtext")
install.packages("Rmisc")

library(ggplot2)
library(ggtext)
library(Rmisc)

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
subset_data <- subset(mydata, Organism == "coralline red alga") 

####SECTION 2: PLOTTING SCATTERPLOTS####
####Li/Ca####
#CalcRate
#plotting X/Ca against carbonate parameter with points, linear regression line, and 
#removing axis elements except for y-axis scale. All X/Ca vs CalcRate scatterplots are 
#leftmost plots in combined figure and require y-axis scale.
Li_calc <- ggplot(data = subset_data, aes(x= CalcRate, y= Li_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),axis.title.y = element_blank(),
        axis.text.y = element_text(size = 16, face = "bold",color = "black"))

#creating model for linear regression
lmModel <-lm(Li_Ca ~ CalcRate, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared #extracting adjusted R-squared value from linear model
fs <- summary(lmModel)$fstatistic #extracting F-statistic from linear model
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE) #calculating F-test p-value from linear model

#plotting p-value, adjusted R-squared value, and red outline on previous scatterplot if
#p-value<0.05 placement of p-value & adjusted R-squared value on plot can be adjusted accordingly
if (pValue < 0.05) {
  Li_calc <- Li_calc +
    geom_richtext(aes(x= 8.45, y = 17), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent")) 
}


#Carbion
#plotting X/Ca against carbonate parameter with points, linear regression line, and 
#removing all axis elements 
Li_carb <- ggplot(data = subset_data, aes(x= carbion, y= Li_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") + theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())

lmModel <-lm(Li_Ca ~ carbion, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Li_carb <- Li_carb +
    geom_richtext(aes(x= 8.45, y = 17), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}


#Alk
Li_alk <- ggplot(data = subset_data, aes(x= Alk, y= Li_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())

lmModel <-lm(Li_Ca ~ Alk, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Li_alk <- Li_alk +
    geom_richtext(aes(x= 8.45, y = 17), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}



#pH
Li_pH <- ggplot(data = subset_data, aes(x= pH, y= Li_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())

lmModel <-lm(Li_Ca ~ pH, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Li_pH <- Li_pH +
    geom_richtext(aes(x= 8.45, y = 17), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}


#pH_cf
Li_pH_cf <- ggplot(data = subset_data, aes(x= pH_cf, y= Li_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())

lmModel <-lm(Li_Ca ~ pH_cf, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Li_pH_cf <- Li_pH_cf +
    geom_richtext(aes(x= 8.45, y = 17), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}


####B/Ca####
#CalcRate
B_calc <- ggplot(data = subset_data, aes(x= CalcRate, y= B_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),axis.title.y = element_blank(),
        axis.text.y = element_text(size = 16, face = "bold",color = "black"))

lmModel <-lm(B_Ca ~ CalcRate, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  B_calc <- B_calc +
    geom_richtext(aes(x= 1.5, y = 495), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}


#Carbion
B_carb <- ggplot(data = subset_data, aes(x= carbion, y= B_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())

lmModel <-lm(B_Ca ~ carbion, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  B_carb <- B_carb +
    geom_richtext(aes(x= 55, y = 495), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}

#Alk
B_alk <- ggplot(data = subset_data, aes(x= Alk, y= B_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())

lmModel <-lm(B_Ca ~ Alk, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  B_alk <- B_alk +
    geom_richtext(aes(x= 1950, y = 495), hjust = 0, label.size =NA, size = 7, 
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}

#pH
B_pH <- ggplot(data = subset_data, aes(x= pH, y= B_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())

lmModel <-lm(B_Ca ~ pH, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  B_pH <- B_pH +
    geom_richtext(aes(x= 7.45, y = 495), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}

#pH_cf
B_pH_cf <- ggplot(data = subset_data, aes(x= pH_cf, y= B_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())

lmModel <-lm(B_Ca ~ pH_cf, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  B_pH_cf <- B_pH_cf +
    geom_richtext(aes(x= 8.45, y = 495), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}

####Mg/Ca####
#CalcRate
Mg_calc <- ggplot(data = subset_data, aes(x= CalcRate, y= Mg_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),axis.title.y = element_blank(),
        axis.text.y = element_text(size = 16, face = "bold",color = "black"))

lmModel <-lm(Mg_Ca ~ CalcRate, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Mg_calc <- Mg_calc +
    geom_richtext(aes(x= 1.5, y = 275), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}


#Carbion
Mg_carb <- ggplot(data = subset_data, aes(x= carbion, y= Mg_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())

lmModel <-lm(Mg_Ca ~ carbion, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Mg_carb <- Mg_carb +
    geom_richtext(aes(x= 55, y = 275), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}

#Alk
Mg_alk <- ggplot(data = subset_data, aes(x= Alk, y= Mg_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())

lmModel <-lm(Mg_Ca ~ Alk, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Mg_alk <- Mg_alk +
    geom_richtext(aes(x= 1950, y = 275), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}

#pH
Mg_pH <- ggplot(data = subset_data, aes(x= pH, y= Mg_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())

lmModel <-lm(Mg_Ca ~ pH, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Mg_pH <- Mg_pH +
    geom_richtext(aes(x= 7.45, y = 275), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}

#pH_cf
Mg_pH_cf <- ggplot(data = subset_data, aes(x= pH_cf, y= Mg_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())

lmModel <-lm(Mg_Ca ~ pH_cf, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Mg_pH_cf <- Mg_pH_cf +
    geom_richtext(aes(x= 8.45, y = 275), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}

####Zn/Ca####
#CalcRate
Zn_calc <- ggplot(data = subset_data, aes(x= CalcRate, y= Zn_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),axis.title.y = element_blank(),
        axis.text.y = element_text(size = 16, face = "bold",color = "black"))

lmModel <-lm(Zn_Ca ~ CalcRate, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Zn_calc <- Zn_calc +
    geom_richtext(aes(x= 1.5, y =120), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}

#Carbion
Zn_carb <- ggplot(data = subset_data, aes(x= carbion, y= Zn_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())

lmModel <-lm(Zn_Ca ~ carbion, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Zn_carb <- Zn_carb +
    geom_richtext(aes(x= 55, y =120), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}

#Alk
Zn_alk <- ggplot(data = subset_data, aes(x= Alk, y= Zn_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())


lmModel <-lm(Zn_Ca ~ Alk, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Zn_alk <- Zn_alk +
    geom_richtext(aes(x= 1950, y =120), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}

#pH
Zn_pH <- ggplot(data = subset_data, aes(x= pH, y= Zn_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())

lmModel <-lm(Zn_Ca ~ pH, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Zn_pH <- Zn_pH +
    geom_richtext(aes(x= 7.45, y =120), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}

#pH_cf
Zn_pH_cf <- ggplot(data = subset_data, aes(x= pH_cf, y= Zn_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())

lmModel <-lm(Zn_Ca ~ pH_cf, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Zn_pH_cf <- Zn_pH_cf +
    geom_richtext(aes(x= 8.45, y =120), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}

####Sr/Ca####
#CalcRate
Sr_calc <- ggplot(data = subset_data, aes(x= CalcRate, y= Sr_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),axis.title.y = element_blank(),
        axis.text.y = element_text(size = 16, face = "bold",color = "black"))

lmModel <-lm(Sr_Ca ~ CalcRate, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Sr_calc <- Sr_calc +
    geom_richtext(aes(x= 8.45, y =120), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}


#Carbion
Sr_carb <- ggplot(data = subset_data, aes(x= carbion, y= Sr_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())

lmModel <-lm(Sr_Ca ~ carbion, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Sr_carb <- Sr_carb +
    geom_richtext(aes(x= 8.45, y =120), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}

#Alk
Sr_alk <- ggplot(data = subset_data, aes(x= Alk, y= Sr_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())

lmModel <-lm(Sr_Ca ~ Alk, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Sr_alk <- Sr_alk +
    geom_richtext(aes(x= 8.45, y =120), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}


#pH
Sr_pH <- ggplot(data = subset_data, aes(x= pH, y= Sr_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())

lmModel <-lm(Sr_Ca ~ pH, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Sr_pH <- Sr_pH +
    geom_richtext(aes(x= 8.45, y =120), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}

#pH_cf
Sr_pH_cf <- ggplot(data = subset_data, aes(x= pH_cf, y= Sr_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())

lmModel <-lm(Sr_Ca ~ pH_cf, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Sr_pH_cf <- Sr_pH_cf +
    geom_richtext(aes(x= 8.45, y =120), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}

####Cd/Ca####
#CalcRate
Cd_calc <- ggplot(data = subset_data, aes(x= CalcRate, y= Cd_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),axis.title.y = element_blank(),
        axis.text.y = element_text(size = 16, face = "bold",color = "black"))

lmModel <-lm(Cd_Ca ~ CalcRate, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Cd_calc <- Cd_calc +
    geom_richtext(aes(x= 8.45, y =120), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}


#Carbion
Cd_carb <- ggplot(data = subset_data, aes(x= carbion, y= Cd_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())

lmModel <-lm(Cd_Ca ~ carbion, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Cd_carb <- Cd_carb +
    geom_richtext(aes(x= 8.45, y =120), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}

#Alk
Cd_alk <- ggplot(data = subset_data, aes(x= Alk, y= Cd_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())

lmModel <-lm(Cd_Ca ~ Alk, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Cd_alk <- Cd_alk +
    geom_richtext(aes(x= 8.45, y =120), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}


#pH
Cd_pH <- ggplot(data = subset_data, aes(x= pH, y= Cd_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())

lmModel <-lm(Cd_Ca ~ pH, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Cd_pH <- Cd_pH +
    geom_richtext(aes(x= 8.45, y =120), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}

#pH_cf
Cd_pH_cf <- ggplot(data = subset_data, aes(x= pH_cf, y= Cd_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())

lmModel <-lm(Cd_Ca ~ pH_cf, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Cd_pH_cf <- Cd_pH_cf +
    geom_richtext(aes(x= 8.45, y =120), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}

####Ba/Ca####
#CalcRate
Ba_calc <- ggplot(data = subset_data, aes(x= CalcRate, y= Ba_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),axis.title.y = element_blank(),
        axis.text.y = element_text(size = 16, face = "bold",color = "black"))

lmModel <-lm(Ba_Ca ~ CalcRate, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Ba_calc <- Ba_calc +
    geom_richtext(aes(x= 1.5, y = 180), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}

#Carbion
Ba_carb <- ggplot(data = subset_data, aes(x= carbion, y= Ba_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())

lmModel <-lm(Ba_Ca ~ carbion, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Ba_carb <- Ba_carb +
    geom_richtext(aes(x= 55, y = 180), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}

#Alk
Ba_alk <- ggplot(data = subset_data, aes(x= Alk, y= Ba_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())

lmModel <-lm(Ba_Ca ~ Alk, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Ba_alk <- Ba_alk +
    geom_richtext(aes(x= 1950, y = 180), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}

#pH
Ba_pH <- ggplot(data = subset_data, aes(x= pH, y= Ba_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())

lmModel <-lm(Ba_Ca ~ pH, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Ba_pH <- Ba_pH +
    geom_richtext(aes(x= 7.45, y = 180), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}

#pH_cf
Ba_pH_cf <- ggplot(data = subset_data, aes(x= pH_cf, y= Ba_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())

lmModel <-lm(Ba_Ca ~ pH_cf, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Ba_pH_cf <- Ba_pH_cf +
    geom_richtext(aes(x= 8.45, y = 180), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}

####U/Ca####
#CalcRate
U_calc <- ggplot(data = subset_data, aes(x= CalcRate, y= U_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),axis.title.y = element_blank(),
        axis.text.y = element_text(size = 16, face = "bold",color = "black"))

lmModel <-lm(U_Ca ~ CalcRate, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  U_calc <- U_calc +
    geom_richtext(aes(x= 1.5, y = 158), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}


#Carbion
U_carb <- ggplot(data = subset_data, aes(x= carbion, y= U_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())

lmModel <-lm(U_Ca ~ carbion, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  U_carb <- U_carb +
    geom_richtext(aes(x= 55, y = 158), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}

#Alk
U_alk <- ggplot(data = subset_data, aes(x= Alk, y= U_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())

lmModel <-lm(U_Ca ~ Alk, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  U_alk <- U_alk +
    geom_richtext(aes(x= 1950, y = 158), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}


#pH
U_pH <- ggplot(data = subset_data, aes(x= pH, y= U_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())

lmModel <-lm(U_Ca ~ pH, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  U_pH <- U_pH +
    geom_richtext(aes(x= 7.45, y = 158), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}

#pH_cf
U_pH_cf <- ggplot(data = subset_data, aes(x= pH_cf, y= U_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank())

lmModel <-lm(U_Ca ~ pH_cf, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  U_pH_cf <- U_pH_cf +
    geom_richtext(aes(x= 8.45, y = 158), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}

####Na/Ca####
#CalcRate
#plotting X/Ca against carbonate parameter with points, regression line, and 
#removing axis elements except for axis scale. All X/Ca vs CalcRate scatterplots are 
#leftmost plots in combined figure and require y-axis scale. X/Ca vs CalcRate scatterplots that
#are at the bottom of the combined plots require x-axis scale
Na_calc <- ggplot(data = subset_data, aes(x= CalcRate, y= Na_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 16, face = "bold",color = "black"),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 16, face = "bold",color = "black"))

lmModel <-lm(Na_Ca ~ CalcRate, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Na_calc <- Na_calc +
    geom_richtext(aes(x= 1.5, y = 17), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}

#Carbion
Na_carb <- ggplot(data = subset_data, aes(x= carbion, y= Na_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 16, face = "bold",color = "black"),
        axis.text.y = element_blank(),axis.title.y = element_blank())

lmModel <-lm(Na_Ca ~ carbion, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Na_carb <- Na_carb +
    geom_richtext(aes(x= 55, y = 17), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}

#Alk
Na_alk <- ggplot(data = subset_data, aes(x= Alk, y= Na_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 16, face = "bold",color = "black"),
        axis.text.y = element_blank(),axis.title.y = element_blank())

lmModel <-lm(Na_Ca ~ Alk, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Na_alk <- Na_alk +
    geom_richtext(aes(x= 1950, y = 17), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}

#pH
Na_pH <- ggplot(data = subset_data, aes(x= pH, y= Na_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 16, face = "bold",color = "black"),
        axis.text.y = element_blank(),axis.title.y = element_blank())

lmModel <-lm(Na_Ca ~ pH, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Na_pH <- Na_pH +
    geom_richtext(aes(x= 7.45, y = 17), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}

#pH_cf
Na_pH_cf <- ggplot(data = subset_data, aes(x= pH_cf, y= Na_Ca)) + geom_point(colour = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, color = "black") +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 16, face = "bold",color = "black"),
        axis.text.y = element_blank(),axis.title.y = element_blank())

lmModel <-lm(Na_Ca ~ pH_cf, data = subset_data)
rsqu <- summary(lmModel)$adj.r.squared
fs <- summary(lmModel)$fstatistic
pValue <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

if (pValue < 0.05) {
  Na_pH_cf <- Na_pH_cf +
    geom_richtext(aes(x= 8.45, y = 17), hjust = 0, label.size =NA, size = 7,
                  fill = "transparent",
                  label = paste0("<b>adj <i>R<sup>2</sup></i> = ", signif(rsqu, 3),
                                 "<br><b><i>p</i> = ",signif(pValue, 3))) +
    theme(panel.border = element_rect(color = "red", size = 3, 
                                      linetype = "solid",fill = "transparent"))
}

####SECTION 3: COMBINING SCATTERPLOTS AND EXPORTING IMAGE####
#naming image file to be exported with organism name 
filename <- paste0(subset_data$Organism[1],"_linear_final",".png") 
png(filename=filename, width=20, height=20,units = "in", bg="white", res=300)
#combining plots, number of columns specified according to number of x-axis parameters tested
multiplot(Li_calc, B_calc, Mg_calc, Zn_calc, Sr_calc, Cd_calc, Ba_calc, U_calc, Na_calc,
          Li_carb, B_carb, Mg_carb, Zn_carb, Sr_carb, Cd_carb, Ba_carb, U_carb, Na_carb,
          Li_alk, B_alk, Mg_alk, Zn_alk, Sr_alk, Cd_alk, Ba_alk, U_alk, Na_alk,
          Li_pH, B_pH, Mg_pH, Zn_pH, Sr_pH, Cd_pH, Ba_pH, U_pH, Na_pH,
          Li_pH_cf, B_pH_cf, Mg_pH_cf, Zn_pH_cf, Sr_pH_cf, Cd_pH_cf, Ba_pH_cf, U_pH_cf, Na_pH_cf,
          cols=5)
dev.off()