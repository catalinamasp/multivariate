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

# 2.Regressing the ecological footprint on human rights score, trade and GDP per capita.
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


#CO2 PER CAPITA




