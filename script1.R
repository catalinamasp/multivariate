#MULTIVARIATE ANALYSIS

library(dplyr)

setwd("C:/Users/pc/Desktop/Data access/multivariate")
dataset <- readxl::read_xlsx("Dataset-Final.xlsx")

summary(dataset)


#Converting column from character to numeric  
dataset[, c(6,14,15)] <- sapply(dataset[, c(6,14,15)], as.numeric)

#Removing missing values
dataset <- na.omit(dataset)

#Rename variables' name
dataset <- rename(dataset, c(EFP = 'Total Ecological Footprint [gha per capita]',
                             HRS = 'Human Rights Score 2017',
                             POL = Polity,
                             GDP = 'GDP annual growth',
                             POP = 'Population Growth',
                             URB = '%Urban Pop',
                             AGR = '% Agriculture',
                             MAN = '% Manufacture',
                             LAND = land,
                             AIR = Pollution,
                             EDU = EDUC))

#Checking the bivariate regression
lm <- lm(EFP ~ HRS, data = dataset)
summary(lm)

#L'incremento di un'unità nel rispetto dei diritti umani, aumenta l'impatto ambientale di 0.9.

#Plotting HRS by country
library(ggplot2)
dataset %>% 
  ggplot(aes(x = Country, y = HRS))+
  geom_col()+
  ylab("HRS") + 
  xlab("Source") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#Creating trade
dataset$TRADE <- log(1 + dataset$IMP + dataset$EXP)

#Multivariate model
mod.mult <- lm(EFP ~ TRADE + GDP + URB, data = dataset)
summary(mod.mult)