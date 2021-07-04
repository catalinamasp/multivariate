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