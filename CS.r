rm(list = ls())
library("plm")
library("plyr")
setwd("c:/Users/srahman/Documents/Econometrics/R/CSGROWTH")
CITY_SET <- read.csv("FF_CENCUS_VARS.csv", header = T, stringsAsFactors = F)


CS_MAG <-  read.csv("CS_SUM_MAG.csv", header = T, sep = ",")
GM_MAG <-  read.csv("GM_SUM_MAG.csv", header = T, sep = ",")
MSA_CENS <-  read.csv("FF_COMBINED_Relevant_Vars.csv", header = T, sep = ",")
SHAG <-  merge(CITY_GDP, CS_MAG, by.x = "MSA_YR" ,by.y = "MSA_YR")
SHAG2 <-  merge(SHAG, GM_MAG, by.x = "MSA_YR" ,by.y = "MSA_YR")
SHAGGED <-  merge(SHAG2, MSA_CENS, by.x = "MSA_YR" ,by.y = "MSA_YR")

is.numeric(CITY_SET$EST_VC03)

SHAGGED$GDP_PP <-  SHAGGED$GDP_ABS / SHAGGED$EST_VC03


CITY_SET_Char = read.csv("VARS.csv", header=T, stringsAsFactors = F, blank)
CITY_SET_num = data.frame(data.matrix(CITY_SET_Char))
numeric.columns = sapply(CITY_SET_num, function(x){mean(as.numeric(is.na(x)))<0.5})
data_set = data.frame(CITY_SET_num[,numeric_columns], char_data[,!numeric_columns])

dat1 <- read.csv("VARS.csv", header=F, stringsAsFactors = F, skip=1)
data2 <- read.delim("VARS.TXT", sep = "")
dat1 <- read.csv("VARS3.csv", header=F, stringsAsFactors = F)

#Note Headings needed to be stripped to maintain data integrity
CITY_SET <- read.csv("CITYCS.csv", header=F, stringsAsFactors = F)

#Creating 'Working Age Variable' V85 - by combining percentages of all groups between 18 and 64. 
CITY_SET$V85 <-  CITY_SET$V6 + CITY_SET$V7 + CITY_SET$V8  + CITY_SET$V9 + CITY_SET$V10

PANCITY <- pdata.frame(CITY_SET)

#Running GMM model, in 'two steps' and including 'two ways' i.e. 
cscity.gmm.brd5 <- pgmm(V84 ~ lag(V84,1) + lag(V12,1) + V13 + V85 + V19 + 
                         V20 + V21 + V22 + V26 + V27 + V29 + V30 + V31 + V32 + V33 + V34 + V35
                       + V36 + V37 + V38 + V39 + V40 + V41 |
                         lag(V84,1:517), data = PANCITY, effect = "twoway", model = "twosteps", Robust = T)

cscity.gmm.nrw <- pgmm(V84 ~ lag(V84,1) + lag(V12,1) + V13 + V85 + V40 + V41 |
                         lag(V84,1:517), data = CITY_SET, effect = "twoway", model = "twosteps")
