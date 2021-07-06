#MULTIVARIATE ANALYSIS

library(dplyr)
library(ggplot2)

rm(list = ls())

setwd("C:/Users/pc/Desktop/Data access/multivariate")
dataset <- readxl::read_xlsx("Dataset_CatalinaSimone.xlsx")

summary(dataset)

#Creating a new dataset without missing values
data.nomiss <- na.omit(dataset)

#Thousands and decimals
options(scipen = 99)


#Checking the bivariate regression
lm <- lm(EFP ~ HRS, data = data.nomiss)
summary(lm)

#One point increase in Human Rights Score, rises the EFP by 0.9.



#Plotting HRS by country
data.nomiss %>% 
  ggplot(aes(x = Country, y = HRS))+
  geom_col()+
  ylab("HRS") + 
  xlab("Source") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



#Creating TRADE: the log of one plus the sum of a country’s imports and exports
data.nomiss$TRADE <- log(1 + data.nomiss$IMP + data.nomiss$EXP)

#Creating GDP per capita by country
data.nomiss$GDPcap <- data.nomiss$GDP/data.nomiss$POP



# 1.Regressing EFP on HRS and GDPcap
mod.mult <- lm(EFP ~ HRS + GDPcap, data = data.nomiss)
summary(mod.mult)

#Only GDPcap significant

# 2.Regression ECONOMICS: the ecological footprint on human rights score, trade and GDP per capita.
mod.mult1 <- lm(EFP ~ HRS + TRADE + GDPcap, data = data.nomiss)
summary(mod.mult1)

#Only GPDcap is significant. All the betas are positive and smaller than 0. (just a quick note)

# 3.Regressing the ecological footprint on human rights score, polity and trade.
mod.mult_try <- lm(EFP ~ HRS + TRADE + POL, data = data.nomiss)
summary(mod.mult_try)

#Human Rights Score and Trade results significant in explaining the ecological footprint.
#One point increase in HRS rises the EFP by 1.1, while one point in TRADE, rises the EFP by 0.9.



#Share of GDP by economic sectors. Import dataset created with data from World Bank.
eco.sector <- readxl::read_xlsx("GDP_by_sector_wb.xlsx")
eco.sector <- na.omit(eco.sector)

#Cleaning the dataset
eco.sector$Infrastructure <- (eco.sector$Industry - eco.sector$Manufactoring)
eco.sector <- eco.sector[,-3]
eco.sector <- rename(eco.sector, Primary = 'Agriculture, forestry, and fishing')

#Creating categorical variables according strongest sector (% of GDP). 

eco.sector$Main <- ifelse(eco.sector$Primary >= eco.sector$Manufactoring &
                            eco.sector$Primary >= eco.sector$Infrastructure &
                            eco.sector$Primary >= eco.sector$Services,
                          "Primary",
                          ifelse(eco.sector$Manufactoring >= eco.sector$Primary &
                            eco.sector$Manufactoring >= eco.sector$Infrastructure &
                            eco.sector$Manufactoring >= eco.sector$Services,
                          "Manufactoring",
                          ifelse(eco.sector$Infrastructure >= eco.sector$Primary &
                            eco.sector$Infrastructure >= eco.sector$Manufactoring &
                            eco.sector$Infrastructure >= eco.sector$Services,
                          "Infrastructure",
                          "Services")
                          ))
                          
# As expected, the sector of Services accounts for the largest part of GDP in nearly all the countries in the dataset.
# Does an economy mainly service-oriented have an impact on the environment health of a country?

mod.mult2 <- lm(data.nomiss$EFP ~  )

#CO2 PER CAPITA




