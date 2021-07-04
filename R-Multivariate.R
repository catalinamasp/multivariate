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

summary(dataset)

dataset$`Human Rights Score 2017`
