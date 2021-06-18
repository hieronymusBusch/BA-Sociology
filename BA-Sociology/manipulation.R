##########################################################
#                                                        #
# BA Sociology:                                          #
# "COVID-19 in Germany, a social-geographic perspective" #
#                                                        #
# 2021/04                                                #
#                                                        #
# Alexander Busch (alexander.busch@stud.uni-hd.de)       #
#                                                        #
# Data Manipulation                                      #
#                                                        #
##########################################################


## read in libraries
library(sf)
library(readxl)
library(plyr)
library(stringr) 
library(ggpubr)
library(stargazer)
library(spdep)

# for some reason tidyverse doesnt load packages?!

library(dplyr)
library(ggplot2)
library(purrr)

# disable scientific notation (1.25e+2 => 125)
options(scipen = 99)  

# set system messages to english
Sys.setenv(lang = "en_US")

## read in data frames & manipulate

setwd("C:/Users/alexa/Desktop/Uni/Bachelorarbeit/R-Code")

# dfds contains shapefile for German county-data, but also some waterways that need to be dropped 
dfds <- st_read("Data/shapefiles/250_NUTS3.shp")
dfds <- subset(dfds, select = c(3,4))
dfds <- dfds[-c(402:428),]

# dfdd contains the actual data on county-level
dfdd <- read_excel("Data/inkar.xls", col_types = c("guess", "guess", "guess", rep("numeric",21)))

dfdd <- dfdd[-1,]
names(dfdd) <- c("KRS", "countyName", "countyCity", 
                 "unemployment", "workersNoEdu", "workersAcadem", "shareForeign",
                 "shareWomen", 
                 "age0.5", "age6.17", "age18.24", "age25.29",
                 "age30.49", "age50.64", "age65.74", "age75.84", 
                 "age85", "avgAge", "AfD", "hhInc", "medInc", "hospBeds", 
                 "popDensity", "popPerDoc")
    # east-west dummy and state variables 
dfdd$east <- ifelse(
  str_detect(dfdd$KRS, "^12")|str_detect(dfdd$KRS, "^13")|
    str_detect(dfdd$KRS, "^14")|str_detect(dfdd$KRS, "^15")|
    str_detect(dfdd$KRS, "^16")|str_detect(dfdd$KRS, "^11")
  ,1,0)
dfdd$state <- 0
dfdd$state[str_detect(dfdd$KRS, "^01")] <- "SH"
dfdd$state[str_detect(dfdd$KRS, "^02")] <- "HH"
dfdd$state[str_detect(dfdd$KRS, "^03")] <- "NI"
dfdd$state[str_detect(dfdd$KRS, "^04")] <- "HB"
dfdd$state[str_detect(dfdd$KRS, "^13")] <- "MV"
dfdd$state[str_detect(dfdd$KRS, "^12")] <- "BB"
dfdd$state[str_detect(dfdd$KRS, "^11")] <- "BE"
dfdd$state[str_detect(dfdd$KRS, "^15")] <- "ST"
dfdd$state[str_detect(dfdd$KRS, "^14")] <- "SN"
dfdd$state[str_detect(dfdd$KRS, "^16")] <- "TH"
dfdd$state[str_detect(dfdd$KRS, "^05")] <- "NW"
dfdd$state[str_detect(dfdd$KRS, "^06")] <- "HE"
dfdd$state[str_detect(dfdd$KRS, "^07")] <- "RP"
dfdd$state[str_detect(dfdd$KRS, "^09")] <- "BY"
dfdd$state[str_detect(dfdd$KRS, "^10")] <- "SL"
dfdd$state[str_detect(dfdd$KRS, "^08")] <- "BW"

dfdd$SH <- ifelse(dfdd$state == "SH",1,0)
dfdd$HH <- ifelse(dfdd$state == "HH",1,0)
dfdd$NI <- ifelse(dfdd$state == "NI",1,0)
dfdd$HB <- ifelse(dfdd$state == "HB",1,0)
dfdd$MV <- ifelse(dfdd$state == "MV",1,0)
dfdd$BB <- ifelse(dfdd$state == "BB",1,0)
dfdd$BE <- ifelse(dfdd$state == "BE",1,0)
dfdd$ST <- ifelse(dfdd$state == "ST",1,0)
dfdd$SN <- ifelse(dfdd$state == "SN",1,0)
dfdd$TH <- ifelse(dfdd$state == "TH",1,0)
dfdd$NW <- ifelse(dfdd$state == "NW",1,0)
dfdd$HE <- ifelse(dfdd$state == "HE",1,0)
dfdd$RP <- ifelse(dfdd$state == "RP",1,0)
dfdd$BY <- ifelse(dfdd$state == "BY",1,0)
dfdd$SL <- ifelse(dfdd$state == "SL",1,0)

    # replace 2 missing values in hospBeds by state averages
hospBedsMeans <- aggregate(dfdd[, "hospBeds"], list(dfdd$state), mean, na.rm = TRUE)
dfdd[288,"hospBeds"] <- hospBedsMeans[4,2]
dfdd[392,"hospBeds"] <- hospBedsMeans[16,2]

# dfgisd contains German Index of Social Deprivation by RKI
    # 0 needs to be added to some KRS
    # two counties merged in 2016, thus their weighted avg GISD is calculated and the old county deleted
dfgisd <- read.csv("Data/Kreis_2014.csv", colClasses = c("NULL", "character", "NULL", "numeric","numeric","NULL","NULL","NULL"), 
                   col.names = c("","KRS","","population","GISD","","",""))
dfgisd$KRS <- ifelse(str_length(dfgisd$KRS)==4,str_c("0", dfgisd$KRS, sep = "", collapse = NULL),dfgisd$KRS)
dfgisd[21,2] <- (dfgisd[21,2]*dfgisd[21,3]+dfgisd[25,2]*dfgisd[25,3])/(dfgisd[21,2]+dfgisd[25,2])
dfgisd[21,1] <- "03159"
dfgisd <- dfgisd[-c(25),]
dfgisd[,"population"] <- NULL


# dfmerge contains both referencing systems of df
dfmerge <- read_excel("Data/04_KreiseVorjahr.xlsx", sheet = 2, range = "A6:F478", 
                      col_types = c("guess", "skip", "guess", "guess", "skip", "numeric"))
names(dfmerge) <- c("KRS","name", "NUTS_CODE", "population")

# delete observations that are states, not counties, and other rows without data
dfmerge$KRSnew <- dfmerge$KRS
dfmerge$KRSnew <- as.numeric(as.character(dfmerge$KRSnew))
dfmerge <- subset(dfmerge, KRSnew > 999)
dfmerge$KRSnew <- NULL


# Combine dfdd and dfgisd 
dfdd <- merge(dfdd, dfgisd, by.dfdd = KRS, by.dfgisd = KRS)

# Combine dfdd and dfmerge for data analysis 
dfdd <- merge(dfmerge, dfdd, by.dfmerge = KRS, by.dfdd = KRS)

### Working with COVID-19 Data

# Reading in RKI COVID-19 data from Germany, reading in Reported Date as Date
dfrki <- read.csv("Data/RKI_COVID19.csv",
                  colClasses=c(
                    "IdBundesland" = "NULL", "Datenstand" = "NULL", 
                    "Meldedatum" = "Date", "IdLandkreis" = "character"
                  ))

# Creating subsets for each month of the pandemic
dfrki20.02 <- subset(dfrki, Meldedatum <= "2020-02-28")
dfrki20.03 <- subset(dfrki, Meldedatum <= "2020-03-31" & Meldedatum > "2020-02-28")
dfrki20.04 <- subset(dfrki, Meldedatum <= "2020-04-30" & Meldedatum > "2020-03-31")
dfrki20.05 <- subset(dfrki, Meldedatum <= "2020-05-31" & Meldedatum > "2020-04-30")
dfrki20.06 <- subset(dfrki, Meldedatum <= "2020-06-30" & Meldedatum > "2020-05-31")
dfrki20.07 <- subset(dfrki, Meldedatum <= "2020-07-31" & Meldedatum > "2020-06-30")
dfrki20.08 <- subset(dfrki, Meldedatum <= "2020-08-31" & Meldedatum > "2020-07-31")
dfrki20.09 <- subset(dfrki, Meldedatum <= "2020-09-30" & Meldedatum > "2020-08-31")
dfrki20.10 <- subset(dfrki, Meldedatum <= "2020-10-31" & Meldedatum > "2020-09-30")
dfrki20.11 <- subset(dfrki, Meldedatum <= "2020-11-30" & Meldedatum > "2020-10-31")
dfrki20.12 <- subset(dfrki, Meldedatum <= "2020-12-31" & Meldedatum > "2020-11-30")
dfrki21.01 <- subset(dfrki, Meldedatum <= "2021-01-31" & Meldedatum > "2020-12-31")
dfrki21.02 <- subset(dfrki, Meldedatum <= "2021-02-28" & Meldedatum > "2021-01-31")
dfrki21.03 <- subset(dfrki, Meldedatum <= "2021-03-31" & Meldedatum > "2021-02-28")
dfrki21.04 <- subset(dfrki, Meldedatum <= "2021-04-30" & Meldedatum > "2021-03-31")
dfrki21.05 <- subset(dfrki, Meldedatum <= "2021-05-31" & Meldedatum > "2021-04-30")


# Aggregating cases and deaths for each quarter (as of now old package, still to change asap!)
# first aggregating cases and deaths
# second merging them in a unified df
# third creating df with aggregate Berlin cases/deaths with correct geographic reference
# fourth adding new row with aggregate Berlin to aggregate df
# fifth deleting rows with old Berlin data
# finally, giving unique names in order to later match


aggregateTransform<- function(a,b,d,e){
  a <- ddply(b, .(IdLandkreis), summarise, aggrCases=sum(AnzahlFall))
  c <- ddply(b, .(IdLandkreis), summarise, aggrDeaths=sum(AnzahlTodesfall))
  a <- merge(a,c,by.x="IdLandkreis",by.y="IdLandkreis")
  names(a)[names(a)=="IdLandkreis"] <- "KRS"
  zahl1<- a %>% filter(
    KRS == "11001"|KRS =="11002"|KRS =="11003"|KRS =="11004"|KRS =="11005"|KRS =="11006"|
      KRS =="11007"|KRS =="11008"|KRS =="11009"|
      KRS =="11010"|KRS =="11011"|KRS =="11012") %>% summarise(aggrCases=sum(aggrCases))
  zahl2<- a %>% filter(
    KRS == "11001"|KRS =="11002"|KRS =="11003"|KRS =="11004"|KRS =="11005"|KRS =="11006"|
      KRS =="11007"|KRS =="11008"|KRS =="11009"|
      KRS =="11010"|KRS =="11011"|KRS =="11012") %>% summarise(aggrDeaths=sum(aggrDeaths))
  neu<- cbind(zahl1,zahl2)
  neu<- neu %>% mutate(KRS= "11000")
  a<- rbind(a,neu)
  a<- a %>% filter (!(
    KRS == "11001"|KRS =="11002"|KRS =="11003"|KRS =="11004"|KRS =="11005"|KRS =="11006"|
      KRS =="11007"|KRS =="11008"|KRS =="11009"|
      KRS =="11010"|KRS =="11011"|KRS =="11012")
  )
  as.character(d)
  as.character(e)
  names(a)[names(a)=="aggrCases"] <- d
  names(a)[names(a)=="aggrDeaths"] <- e
  a
}

dfrkiaggr20.02 <- aggregateTransform(dfrkiaggr20.02,dfrki20.02,"aggrCases20.02","aggrDeaths20.02")
dfrkiaggr20.03 <- aggregateTransform(dfrkiaggr20.03,dfrki20.03,"aggrCases20.03","aggrDeaths20.03")
dfrkiaggr20.04 <- aggregateTransform(dfrkiaggr20.04,dfrki20.04,"aggrCases20.04","aggrDeaths20.04")
dfrkiaggr20.05 <- aggregateTransform(dfrkiaggr20.05,dfrki20.05,"aggrCases20.05","aggrDeaths20.05")
dfrkiaggr20.06 <- aggregateTransform(dfrkiaggr20.06,dfrki20.06,"aggrCases20.06","aggrDeaths20.06")
dfrkiaggr20.07 <- aggregateTransform(dfrkiaggr20.07,dfrki20.07,"aggrCases20.07","aggrDeaths20.07")
dfrkiaggr20.08 <- aggregateTransform(dfrkiaggr20.08,dfrki20.08,"aggrCases20.08","aggrDeaths20.08")
dfrkiaggr20.09 <- aggregateTransform(dfrkiaggr20.09,dfrki20.09,"aggrCases20.09","aggrDeaths20.09")
dfrkiaggr20.10 <- aggregateTransform(dfrkiaggr20.10,dfrki20.10,"aggrCases20.10","aggrDeaths20.10")
dfrkiaggr20.11 <- aggregateTransform(dfrkiaggr20.11,dfrki20.11,"aggrCases20.11","aggrDeaths20.11")
dfrkiaggr20.12 <- aggregateTransform(dfrkiaggr20.12,dfrki20.12,"aggrCases20.12","aggrDeaths20.12")
dfrkiaggr21.01 <- aggregateTransform(dfrkiaggr21.01,dfrki21.01,"aggrCases21.01","aggrDeaths21.01")
dfrkiaggr21.02 <- aggregateTransform(dfrkiaggr21.02,dfrki21.02,"aggrCases21.02","aggrDeaths21.02")
dfrkiaggr21.03 <- aggregateTransform(dfrkiaggr21.03,dfrki21.03,"aggrCases21.03","aggrDeaths21.03")
dfrkiaggr21.04 <- aggregateTransform(dfrkiaggr21.04,dfrki21.04,"aggrCases21.04","aggrDeaths21.04")
dfrkiaggr21.05 <- aggregateTransform(dfrkiaggr21.05,dfrki21.05,"aggrCases21.05","aggrDeaths21.05")
dfrkiaggr <- aggregateTransform(dfrkiaggr,dfrki,"aggrCases","aggrDeaths")

### Combining data 

listrkiaggr <- list(dfrkiaggr20.02,dfrkiaggr20.03,dfrkiaggr20.04,dfrkiaggr20.05,dfrkiaggr20.06,dfrkiaggr20.07,
                    dfrkiaggr20.08,dfrkiaggr20.09,dfrkiaggr20.10,dfrkiaggr20.11,dfrkiaggr20.12,
                    dfrkiaggr21.01,dfrkiaggr21.02,dfrkiaggr21.03,dfrkiaggr21.04,dfrkiaggr21.05)

for (a in listrkiaggr) {
  dfrkiaggr <- merge(dfrkiaggr, a, by = "KRS",all = TRUE)
}
dfrkiaggr[is.na(dfrkiaggr)] <- 0
dfrkiaggr$population <- dfdd$population

# Calculating cases per 100,000 inhabitants

dfrkiaggr$IR <- (dfrkiaggr$aggrCases / dfrkiaggr$population) * 100000
dfrkiaggr$IR20.02 <- (dfrkiaggr$aggrCases20.02 / dfrkiaggr$population) * 100000
dfrkiaggr$IR20.03 <- (dfrkiaggr$aggrCases20.03 / dfrkiaggr$population) * 100000
dfrkiaggr$IR20.04 <- (dfrkiaggr$aggrCases20.04 / dfrkiaggr$population) * 100000
dfrkiaggr$IR20.05 <- (dfrkiaggr$aggrCases20.05 / dfrkiaggr$population) * 100000
dfrkiaggr$IR20.06 <- (dfrkiaggr$aggrCases20.06 / dfrkiaggr$population) * 100000
dfrkiaggr$IR20.07 <- (dfrkiaggr$aggrCases20.07 / dfrkiaggr$population) * 100000
dfrkiaggr$IR20.08 <- (dfrkiaggr$aggrCases20.08 / dfrkiaggr$population) * 100000
dfrkiaggr$IR20.09 <- (dfrkiaggr$aggrCases20.09 / dfrkiaggr$population) * 100000
dfrkiaggr$IR20.10 <- (dfrkiaggr$aggrCases20.10 / dfrkiaggr$population) * 100000
dfrkiaggr$IR20.11 <- (dfrkiaggr$aggrCases20.11 / dfrkiaggr$population) * 100000
dfrkiaggr$IR20.12 <- (dfrkiaggr$aggrCases20.12 / dfrkiaggr$population) * 100000
dfrkiaggr$IR21.01 <- (dfrkiaggr$aggrCases21.01 / dfrkiaggr$population) * 100000
dfrkiaggr$IR21.02 <- (dfrkiaggr$aggrCases21.02 / dfrkiaggr$population) * 100000
dfrkiaggr$IR21.03 <- (dfrkiaggr$aggrCases21.03 / dfrkiaggr$population) * 100000
dfrkiaggr$IR21.04 <- (dfrkiaggr$aggrCases21.04 / dfrkiaggr$population) * 100000
dfrkiaggr$IR21.05 <- (dfrkiaggr$aggrCases21.05 / dfrkiaggr$population) * 100000

dfrkiaggr$CFR <- (dfrkiaggr$aggrDeaths / dfrkiaggr$aggrCases)
dfrkiaggr$CFR20.02 <- (dfrkiaggr$aggrDeaths20.02 / dfrkiaggr$aggrCases20.02)
dfrkiaggr$CFR20.03 <- (dfrkiaggr$aggrDeaths20.03 / dfrkiaggr$aggrCases20.03)
dfrkiaggr$CFR20.04 <- (dfrkiaggr$aggrDeaths20.04 / dfrkiaggr$aggrCases20.04)
dfrkiaggr$CFR20.05 <- (dfrkiaggr$aggrDeaths20.05 / dfrkiaggr$aggrCases20.05)
dfrkiaggr$CFR20.06 <- (dfrkiaggr$aggrDeaths20.06 / dfrkiaggr$aggrCases20.06)
dfrkiaggr$CFR20.07 <- (dfrkiaggr$aggrDeaths20.07 / dfrkiaggr$aggrCases20.07)
dfrkiaggr$CFR20.08 <- (dfrkiaggr$aggrDeaths20.08 / dfrkiaggr$aggrCases20.08)
dfrkiaggr$CFR20.09 <- (dfrkiaggr$aggrDeaths20.09 / dfrkiaggr$aggrCases20.09)
dfrkiaggr$CFR20.10 <- (dfrkiaggr$aggrDeaths20.10 / dfrkiaggr$aggrCases20.10)
dfrkiaggr$CFR20.11 <- (dfrkiaggr$aggrDeaths20.11 / dfrkiaggr$aggrCases20.11)
dfrkiaggr$CFR20.12 <- (dfrkiaggr$aggrDeaths20.12 / dfrkiaggr$aggrCases20.12)
dfrkiaggr$CFR21.01 <- (dfrkiaggr$aggrDeaths21.01 / dfrkiaggr$aggrCases21.01)
dfrkiaggr$CFR21.02 <- (dfrkiaggr$aggrDeaths21.02 / dfrkiaggr$aggrCases21.02)
dfrkiaggr$CFR21.03 <- (dfrkiaggr$aggrDeaths21.03 / dfrkiaggr$aggrCases21.03)
dfrkiaggr$CFR21.04 <- (dfrkiaggr$aggrDeaths21.04 / dfrkiaggr$aggrCases21.04)
dfrkiaggr$CFR21.05 <- (dfrkiaggr$aggrDeaths21.05 / dfrkiaggr$aggrCases21.05)

# CFR with 1 month lag
dfrkiaggr$CFRlag20.04 <- (dfrkiaggr$aggrDeaths20.04 / dfrkiaggr$aggrCases20.03)
dfrkiaggr$CFRlag20.05 <- (dfrkiaggr$aggrDeaths20.05 / dfrkiaggr$aggrCases20.04)
dfrkiaggr$CFRlag20.06 <- (dfrkiaggr$aggrDeaths20.06 / dfrkiaggr$aggrCases20.05)
dfrkiaggr$CFRlag20.07 <- (dfrkiaggr$aggrDeaths20.07 / dfrkiaggr$aggrCases20.06)
dfrkiaggr$CFRlag20.08 <- (dfrkiaggr$aggrDeaths20.08 / dfrkiaggr$aggrCases20.07)
dfrkiaggr$CFRlag20.09 <- (dfrkiaggr$aggrDeaths20.09 / dfrkiaggr$aggrCases20.08)
dfrkiaggr$CFRlag20.10 <- (dfrkiaggr$aggrDeaths20.10 / dfrkiaggr$aggrCases20.09)
dfrkiaggr$CFRlag20.11 <- (dfrkiaggr$aggrDeaths20.11 / dfrkiaggr$aggrCases20.10)
dfrkiaggr$CFRlag20.12 <- (dfrkiaggr$aggrDeaths20.12 / dfrkiaggr$aggrCases20.11)
dfrkiaggr$CFRlag21.01 <- (dfrkiaggr$aggrDeaths21.01 / dfrkiaggr$aggrCases20.12)
dfrkiaggr$CFRlag21.02 <- (dfrkiaggr$aggrDeaths21.02 / dfrkiaggr$aggrCases21.01)
dfrkiaggr$CFRlag21.03 <- (dfrkiaggr$aggrDeaths21.03 / dfrkiaggr$aggrCases21.02)
dfrkiaggr$CFRlag21.04 <- (dfrkiaggr$aggrDeaths21.04 / dfrkiaggr$aggrCases21.03)
dfrkiaggr$CFRlag21.05 <- (dfrkiaggr$aggrDeaths21.05 / dfrkiaggr$aggrCases21.04)

# recode undefined (x/0) CFR to 0 as described in Thesis
dfrkiaggr[is.na(dfrkiaggr)] <- 0
dfrkiaggr[dfrkiaggr == Inf] <- 0

## Combinig aggr data and spatial / geographic data
dfdd <- merge(dfdd, dfrkiaggr, by.dfdd = KRS, by.dfrkiaggr = KRS)

# ln
dfdd$LNIR20.02 <- log(dfdd$IR20.02)
dfdd$LNIR20.03 <- log(dfdd$IR20.03)
dfdd$LNIR20.04 <- log(dfdd$IR20.04)
dfdd$LNIR20.05 <- log(dfdd$IR20.05)
dfdd$LNIR20.06 <- log(dfdd$IR20.06)
dfdd$LNIR20.07 <- log(dfdd$IR20.07)
dfdd$LNIR20.08 <- log(dfdd$IR20.08)
dfdd$LNIR20.09 <- log(dfdd$IR20.09)
dfdd$LNIR20.10 <- log(dfdd$IR20.10)
dfdd$LNIR20.11 <- log(dfdd$IR20.11)
dfdd$LNIR20.12 <- log(dfdd$IR20.12)
dfdd$LNIR21.01 <- log(dfdd$IR21.01)
dfdd$LNIR21.02 <- log(dfdd$IR21.02)
dfdd$LNIR21.03 <- log(dfdd$IR21.03)
dfdd$LNIR21.04 <- log(dfdd$IR21.04)
dfdd$LNIR21.05 <- log(dfdd$IR21.05)
dfdd$LNIR <- log(dfdd$IR)

dfdd$LNavgAge <- log(dfdd$avgAge)
dfdd$LNmedInc <- log(dfdd$medInc)
dfdd$LNpopPerDoc <- log(dfdd$popPerDoc)
dfdd$GISDforeign <- dfdd$GISD * dfdd$shareForeign
dfdd$GISDeast <- dfdd$GISD * dfdd$east

# to prevent 0s to become neg. infinite (by ln(0)), replace those with -2.3 (incedence of 1 per 1,000,000)
dfdd[dfdd < -10000] <- -2.3

# Combine dfds and dfdd for mapping / SAR Modeling
dfds <- merge(dfds, dfdd, by.dfds = NUTS_CODE, by.dfdd = NUTS_CODE)

dfds$NUTS_NAME <- NULL
dfds$name <- NULL
dfdd$name <- NULL


