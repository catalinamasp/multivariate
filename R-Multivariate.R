#MULTIVARIATE ANALYSIS
library(foreign)
library(nnet)
library(mvtnorm)
library(ordinal)
library(lmtest)
library(sandwich)
library(car)
library(coefplot)
library(interplot)
library(ordinal)
library(dplyr)


#IMPORT G20 DATASET

rm(list = ls())
setwd("~/Documents/UNIMI/Multivariate analysis/")
g20data <- readxl::read_xlsx("G20.xlsx")

#Converting column from character to numeric  
g20data[, c(3,6,14,15)] <- sapply(g20data[, c(3,6,14,15)], as.numeric)
summary(g20data)


#Rename variables' name
g20data <- rename(g20data, c(EFP = 'Total Ecological Footprint [gha per capita]',
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

lm <- lm(g20data$EFP ~ g20data$HRS)
summary(lm)


#DISTRIBUTION HRS

hist(dataset$HRS)

dataset %>% 
        ggplot(aes(x = Country, y = HRS))+
        geom_col()+
        ylab("HRS") + 
        xlab("Source") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#DISTRIBUTION EFP

dataset %>% 
        ggplot(aes(x = Country, y = EFP))+
        geom_col()+
        ylab("EFP") + 
        xlab("Source") +
        theme_bw()     +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#IMPORT DATASET WORLD

data.world <- readxl::read_xlsx("Dataset-Multivariate.xlsx")
summary(data.world)

data.world[, c(3,6,7, 14,15,16)] <- sapply(data.world[, c(3,6,7, 14,15,16)], as.numeric)
summary(data.world)
data.world <- na.omit(data.world)

summary(data.world)

lm <- lm(data.world$EFP ~ data.world$HRS)
summary(lm)

#There's a negative effect, for every increase in point of HRS there's a negative effect in Ecological   footprint.


#human rights score new index

hrs <- Import("human-rights-scores.xls")


#TRYING HOMOSCEDASTICITY

b <- lm$coef
#beta
#capture the coefficients that are estimated
u <- lm$resid
#residuals to collect residuals

fit <- lm$fitted.values

plot(fit,
     u,
     pch = 19,
     col = "darkblue",
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Plot of Residual Variance"
)

#HETEROSCEIDASTICITY

library(lmtest)
bptest(lm)

#0.85 > 0.05 
#We can reject the null hypothesis saying that heterosceidasticity exists


#TRADE

dataset$trade <- log(1 + dataset$IMP + dataset$EXP)
summary(data.nomissing)



