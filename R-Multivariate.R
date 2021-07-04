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

setwd("~/Documents/UNIMI/Multivariate analysis/")
dataset <- readxl::read_xlsx("G20.xlsx")

setwd("C:/Users/pc/Desktop/Data access/multivariate")
dataset <- readxl::read_xlsx("G20.xlsx")
summary(dataset)

#Converting column from character to numeric  
dataset[, c(3,6,14,15)] <- sapply(dataset[, c(3,6,14,15)], as.numeric)


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

summary(dataset)


lm <- lm(dataset$EFP ~ dataset$HRS)
summary(lm)

#There's a negative effect, for every increase in point of HRS there's a negative effect in Ecological   footprint.

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




#GDP CAN'T BE NEGATIVE



#TRADE

dataset$trade <- log(1 + dataset$IMP + dataset$EXP)
summary(data.nomissing)

# Task 3: Let's check for autocorrelation #

install.packages("dwtest")
dwtest(lm)





