#Name: Anoop Nair
#Module: Intro to R
# Script to generate Monte Carlo
#Student Id: R00223644 

######## set directory  #######

setwd("D:/STUDY MATERIAL/Masters DS & Analytics/Intro to R/Project/")
#getwd()

####### Install and load required packages #######

library (ggplot2)
library(plotly)
library(tidyr)
library(pastecs)
library(tidyverse)
library(psych)
library(plyr)
library(e1071)
library(readxl)
library(lubridate)
library(anytime)
library(gganimate)
library(tibble)
library(formattable)
library(gridExtra)
library(GGally)
library(stats)
library(dplyr)
library(shiny)
library(DT)
library(ggpubr)


######### Loading adn cleaning the dataset ######

Co2data=read.csv("STAT8010_2022_assignment2_2022.csv")
head(Co2data)
str(Co2data)

#### change the long names  to concise and relevant ones ####

colnames(Co2data)=c('Make','Model','Class','Enginesize','Cylinder','Transmissiontype',
                    'Fueltype','Fuelconsumption_city','Fuelconsumption_highway','Fuelconsumption_both','Milespergallon_both','Co2emmissions')


######### collapsing levelS Of Class of vehicle  #############

Co2data$Class[grepl("SUV", Co2data$Class)]="SUV"
Co2data$Class[grepl("PICKUP", Co2data$Class)]="PICKUP"
Co2data$Class[grepl("VAN", Co2data$Class)]="VAN"
Co2data$Class[grepl("COMPACT", Co2data$Class)]="COMPACT"
Co2data$Class[grepl("SPECIAL", Co2data$Class)]="SPECIAL"
Co2data$Class[grepl("TWO-SEATER", Co2data$Class)]="SPORT"


######### CATEGORIZING OTHER CLASS VALUES INTO TOURISM ##########

Co2data$Class[grepl("STATION WAGON - MID-SIZE", Co2data$Class)]="TOURISM"
Co2data$Class[grepl("STATION WAGON - SMALL", Co2data$Class)]="TOURISM"
Co2data$Class[grepl("MID-SIZE", Co2data$Class)]="TOURISM"
Co2data$Class[grepl("FULL-SIZE", Co2data$Class)]="TOURISM"

######### Converting levelS Of Class of vehicle  #############


Co2data$Transmissiontype[grepl("A", Co2data$Transmissiontype)]="Automatic"
Co2data$Transmissiontype[grepl("M", Co2data$Transmissiontype)]="Manual"


####### Update gasoline #####


Co2data$Fueltype[Co2data$Fueltype=="X"]="Regular gasoline"
Co2data$Fueltype[Co2data$Fueltype=="Z"]="Premium gasoline"
Co2data$Fueltype[Co2data$Fueltype=="D"]="Diesel"
Co2data$Fueltype[Co2data$Fueltype=="E"]="Ethanol"
Co2data$Fueltype[Co2data$Fueltype=="N"]="Natural Gas"



########## convert certain variables into factors 


Co2data$Model=as.factor(Co2data$Model)
Co2data$Make=as.factor(Co2data$Make)
Co2data$Class=as.factor(Co2data$Class)
Co2data$Fueltype=as.factor(Co2data$Fueltype)
Co2data$Transmissiontype=as.factor(Co2data$Transmissiontype)

colmn=colnames(Co2data)
set.seed(100)
par(mfrow=c(1,1))

################# Monte Carlo Simulation  #################

Co2data_Copy=Co2data ### copying the original dataset to Co2data
str(Co2data_Copy)
############ best fit model #########

lm_model1=lm(Co2emmissions~Enginesize+Cylinder+Fuelconsumption_both+Milespergallon_both,Co2data_Copy)
summary(lm_model1)

Coeff1=summary(lm_model1)$coefficients[1, 1] 
Coeff1_engsize=summary(lm_model1)$coefficients[2, 1]
Coeff1_cyclinder=summary(lm_model1)$coefficients[3, 1]
Coeff1_Fuelconsumption_both=summary(lm_model1)$coefficients[4, 1]
Coeff1_Milespergallon_both=summary(lm_model1)$coefficients[5, 1]

residuals_SD1=summary(lm_model1)$sigma # 18.84

#### to simulate nrow values of residuals with mean as 0 and standard deviation from the model 
set.seed(100)

residualSD1_simulateddata=rnorm(n=nrow(Co2data_Copy),mean=0,sd=residuals_SD1)### montecarlo simulation for residuals

sum(residualSD1_simulateddata) # 314.4737

######### calculating the New CO2 values using Best firt linear model and the simulated residuals

for (i in c(1:nrow(Co2data_Copy)))
{
  # iterating through linear model and residuals to calculate CO2emissions value
    Co2data_Copy[i,c('co2emission_model1')]=Coeff1+(Coeff1_engsize*Co2data_Copy[i,c('Enginesize')])+
      (Coeff1_cyclinder*Co2data_Copy[i,c('Cylinder')])+
    (Coeff1_Fuelconsumption_both*Co2data_Copy[i,c('Fuelconsumption_both')])+
    (Coeff1_Milespergallon_both*Co2data_Copy[i,c('Milespergallon_both')])+residualSD1_simulateddata[i]
}

Co2data_Copy['co2emission_model1']=round(Co2data_Copy['co2emission_model1'])

#### fit the best fit model using the simulated residuals ####

lm_model1_sim=lm(co2emission_model1~Enginesize+Cylinder+Fuelconsumption_both+Milespergallon_both,Co2data_Copy)
summary(lm_model1_sim)

par(mfrow=c(1,2))

######  histogram comparing the original residuals and simulated residuals using the simulated model

hist(lm_model1$resid, main="Histogram of Residuals for Model 1 (Best Fit)",
     ylab="Frequency",xlab='Residuals',col="light pink")

hist(lm_model1_sim$resid, main="Histogram of simulated Residuals for Model 1 (Best Fit)",
     ylab="Frequency",xlab='Residuals',col="light pink")

####### comparing the coefficients and standard errors of original to simulated model #####

summary(lm_model1)

sqrt(diag(vcov(lm_model1)))
sqrt(diag(vcov(lm_model1_sim)))

summary(lm_model1)$coefficients[, 1]
summary(lm_model1_sim)$coefficients[, 1]


######### average of real co2 emissions and simulated co2 emissions ########

mean(Co2data_Copy$Co2emmissions) #250.5847
mean(Co2data_Copy$co2emission_model1) #250.6337

median(Co2data_Copy$Co2emmissions) #250.5847
median(Co2data_Copy$co2emission_model1) #250.6337

###################################### 2nd Model using Fuelconsumption_both as the only predictor variable #############################################################

lm_model2=lm(Co2emmissions~Fuelconsumption_both,Co2data_Copy)
summary(lm_model2)

Coeff2=summary(lm_model2)$coefficients[1, 1] 
Coeff2_Fuelconsumption_both=summary(lm_model2)$coefficients[2, 1]



# The residual standard error from the model is the standard deviation of the residuals.

residuals_SD2=summary(lm_model2)$sigma # 23.19957

######## test for normality of residuals #####
qqnorm(lm_model2$resid)
qqline(lm_model2$resid)

#### to simulate values of residuals with mean as 0 and variance from the model

nrow(Co2data_Copy)
set.seed(100)
residualSD2_simulateddata=rnorm(n=nrow(Co2data_Copy),mean=0,sd=residuals_SD2)

sum(residualSD2_simulateddata) #399.8185


######## populating new Co2emisson value using Model2 ######
for (i in c(1:nrow(Co2data_Copy)))
{
  # iterating through linear model and residuals to calculate CO2emissions value
  Co2data_Copy[i,c('co2emission_model2')]=Coeff2+(
    Coeff2_Fuelconsumption_both*Co2data_Copy[i,c('Fuelconsumption_both')])+residualSD2_simulateddata[i]
}

Co2data_Copy['co2emission_model2']=round(Co2data_Copy['co2emission_model2']) # rounding off the CO2 values to remove decimal

######### fit the 2nd Model  using the simulated values for CO2 present in co2emission_model2 #####

lm_model2_sim=lm(co2emission_model2~Fuelconsumption_both,Co2data_Copy)
summary(lm_model2_sim)

###### residuals comparison between real and simulated using histogram

hist(lm_model2$resid, main="Histogram of Residuals for Model 2 (FuelConsumption)",
     ylab="Frequency",xlab='Residuals',col="light pink") #Histogram of Residuals for best fit


hist(lm_model2_sim$resid, main="Histogram of simulated Residuals for Model 2 (FuelConsumption)",
     ylab="Frequency",xlab='Residuals',col="light pink") #Histogram of Residuals for best fit


####### comparing the coefficients and standard errors of original to simulated model #####

summary(lm_model1)

#### to calculate the standard error of the parameters
sqrt(diag(vcov(lm_model2))) 
sqrt(diag(vcov(lm_model2_sim)))

#### to calculate the coeffecients value 
summary(lm_model2)$coefficients[, 1]
summary(lm_model2_sim)$coefficients[, 1]

mean(Co2data_Copy$Co2emmissions) #250.5847
mean(Co2data_Copy$co2emission_model2) #250.6337

median(Co2data_Copy$Co2emmissions) #250.5847
median(Co2data_Copy$co2emission_model2) #250.6337


####################################### 3rd Model #########################################################

lm_model3=lm(Co2emmissions~Enginesize,Co2data_Copy)
summary(lm_model3)

Coeff3=summary(lm_model3)$coefficients[1, 1] 
Coeff3_Enginesize=summary(lm_model3)$coefficients[2, 1]
residuals_SD3=summary(lm_model3)$sigma #30.71721

######## test for normality of residuals #####
qqnorm(lm_model3$resid)
qqline(lm_model3$resid)

set.seed(100) #set seed
residualSD3_simulateddata=rnorm(n=nrow(Co2data_Copy),mean=0,sd=residuals_SD3) # simulated residuals using mean and sd for model 3
sum(residualSD3_simulateddata) #529.3767

##### populate the values co2 using model 3
for (i in c(1:nrow(Co2data_Copy)))
{
  
  Co2data_Copy[i,c('co2emission_model3')]=Coeff3+(
    Coeff3_Enginesize*Co2data_Copy[i,c('Enginesize')])+residualSD3_simulateddata[i] # iterating through linear model and residuals to calculate CO2emissions value
}


Co2data_Copy['co2emission_model3']=round(Co2data_Copy['co2emission_model3'])

lm_model3_sim=lm(co2emission_model3~Enginesize,Co2data_Copy)
summary(lm_model3_sim)
##### histogram for residuals of real and simulated data ########
par(mfrow=c(1,2))
hist(lm_model3$resid, main="Histogram of of Residuals for Model 3(EngineSize)",
     ylab="Frequency",xlab='Residuals',col="light pink")

hist(lm_model3_sim$resid, main="Histogram of simulated Residuals for Model 3 (EngineSize)",
     ylab="Frequency",xlab='Residuals',col="light pink") # newly simulated data







