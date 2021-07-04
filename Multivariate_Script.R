#MULTIVARIATE ANALYSIS



library(dplyr)

rm(list = ls())
setwd("~/Documents/UNIMI/Multivariate analysis/")
dataset <- readxl::read_xlsx("Dataset-Multivariate.xlsx")

summary(dataset)


#thousands and decimals

options(scipen = 99)

#*********this piece of code is not useful anymore***********+

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

#*********this piece of code is not useful anymore***********+

#Descriptive Statistics

install.packages("pastecs")
library(pastecs)
desc <- stat.desc(dataset[,2:16])
desc



#Checking the bivariate regression
lm <- lm(EFP ~ HRS, data = dataset)
summary(lm)

#L'incremento di un'unità nel rispetto dei diritti umani, aumenta l'impatto ambientale di 0.9.

#multivariate model with all the variables

mod.mult <- lm(EFP ~ HRS + POL + GDP + POP + URB + AGR + MAN + EXP + IMP + LAND + ARAB + AIR + EDU + REN, data = dataset)
summary(mod.mult)

#Plotting HRS by country
library(ggplot2)
dataset %>% 
  ggplot(aes(x = Country, y = HRS))+
  geom_col()+
  ylab("HRS") + 
  xlab("Source") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))


hist(dataset$GDP)

#Creating trade
dataset$TRADE <- log(1 + dataset$IMP + dataset$EXP)

hist(dataset$TRADE)


#CREATING CATEGORICAL VARIABLE FOR AGRICULTURE AND MANUFACTURING 

dataset$primary <- ifelse(dataset$AGR >= dataset$MAN,
                          "Agr",
                                 "Man"
                          )

#SEE DISTRIBUTION
dataset %>% 
  group_by(primary) %>% 
  count() %>% 
  ggplot(aes(x = primary, y = n))+
  geom_col()+
  ylab("Primary Sector") + 
  xlab("Source") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))





