##########################################################
#                                                        #
# BA Sociology:                                          #
# "COVID-19 in Germany, a social-geographic perspective" #
#                                                        #
# 2021/04                                                #
#                                                        #
# Alexander Busch (alexander.busch@stud.uni-hd.de)       #
#                                                        #
# Data Analysis                                          #
#                                                        #
##########################################################

source("manipulation.R")
source("functions.R")

# Create lists needed for analysis
varlistLNIR <- c("LNIR20.02","LNIR20.03","LNIR20.04","LNIR20.05","LNIR20.06",
                        "LNIR20.07","LNIR20.08","LNIR20.09","LNIR20.10",
                        "LNIR20.11","LNIR20.12","LNIR21.01","LNIR21.02",
                        "LNIR21.03","LNIR21.04")
varlistIR <- c("IR20.02","IR20.03","IR20.04","IR20.05","IR20.06",
                 "IR20.07","IR20.08","IR20.09","IR20.10",
                 "IR20.11","IR20.12","IR21.01","IR21.02",
                 "IR21.03","IR21.04")
varlistLNdeathRate <- c("LNdeathRate20.02","LNdeathRate20.03","LNdeathRate20.04","LNdeathRate20.05","LNdeathRate20.06",
                        "LNdeathRate20.07","LNdeathRate20.08","LNdeathRate20.09","LNdeathRate20.10",
                        "LNdeathRate20.11","LNdeathRate20.12","LNdeathRate21.01","LNdeathRate21.02",
                        "LNdeathRate21.03","LNdeathRate21.04")
varlistCFR <- c("CFR20.02","CFR20.03","CFR20.04","CFR20.05","CFR20.06",
                        "CFR20.07","CFR20.08","CFR20.09","CFR20.10",
                        "CFR20.11","CFR20.12","CFR21.01","CFR21.02",
                        "CFR21.03","CFR21.04")
varlistControl <- c("shareWomen","age18.24","age25.29","age30.49","age50.64","age65.74","age75.84","age85")
varlistControlForeign <- c("shareForeign","shareWomen","age18.24","age25.29","age30.49","age50.64","age65.74","age75.84","age85")
varlistControlEast <- c("east","shareWomen","age18.24","age25.29","age30.49","age50.64","age65.74","age75.84","age85")
month <- c("02/20","03/20","04/20","05/20","06/20","07/20","08/20","09/20","10/20",
           "11/20","12/20","01/21","02/21","03/21","04/21")

neighbors <- poly2nb(dfds)
weighted_neighbors <- nb2listw(neighbors, zero.policy=T)
weighted_neighbors

### Analysis

## Test for spatial Autocorrelation in data
moran.test(dfds$IR, weighted_neighbors, zero.policy=T)
moran.test(dfds$IR20.04, weighted_neighbors, zero.policy=T)
moran.test(dfds$IR21.04, weighted_neighbors, zero.policy=T)
moran.plot(dfds$IR, weighted_neighbors, zero.policy=T,xlab="IR", ylab="spatially lagged IR")

moran.test(dfds$CFR, weighted_neighbors, zero.policy=T)
moran.test(dfds$CFR20.04, weighted_neighbors, zero.policy=T)
moran.test(dfds$CFR21.04, weighted_neighbors, zero.policy=T)
moran.plot(dfds$CFR, weighted_neighbors, zero.policy=T)

## Hypothesis I, IR ~ GISD
dfIR.GISD <- SARvarlist2("GISD",varlistLNIR, varlistControl)
displayCoeff(dfIR.GISD,"LN IR ~ GISD, Average Total Effects (SAR)", "Average Total Effect")
dfIR.GISD


dfIR.unempl <- SARvarlist2("unemployment",varlistLNIR, varlistControl)
displayCoeff(dfIR.unempl,"LN IR ~ unemployment, Average Total Effects (SAR)", "Average Total Effect")
dfIR.unempl

dfIR.LNmedInc <- SARvarlist2("LNmedInc",varlistLNIR, varlistControl)
displayCoeff(dfIR.LNmedInc,"LN IR ~ LN Median Income, Average Total Effects (SAR)", "Average Total Effect")
dfIR.LNmedInc

dfIR.workersAcadem <- SARvarlist2("workersAcadem",varlistLNIR, varlistControl)
displayCoeff(dfIR.workersAcadem,"LN IR ~ Workers Academic Education, Average Total Effects (SAR)", "Average Total Effect")
dfIR.workersAcadem


ggplot(dfdd, aes(x = GISD, y = LNIR20.04)) +
  geom_point() +
  xlab("GISD") +
  ylab("LN IR") +
  theme_bw()

## Hypothesis I, CFR ~ GISD
dfCFR.GISD <- SARvarlist2("GISD",varlistCFR, varlistControl)
displayCoeff(dfCFR.GISD,"CFR ~ GISD, Average Total Effects (SAR)", "Average Total Effect")
dfCFR.GISD

dfCFR.GISD2 <- OLSvarlist2("GISD",varlistCFR, varlistControl)
displayCoeff(dfCFR.GISD2,"CFR ~ GISD, OLS Coefficients", "Coefficient")
dfCFR.GISD2

dfCFR.unempl <- OLSvarlist2("unemployment",varlistCFR, varlistControl)
displayCoeff(dfCFR.unempl,"CFR ~ unemployment, Regression Coefficient (OLS)", "Regression Coefficient")
dfCFR.unempl

dfCFR.LNmedInc <- OLSvarlist2("LNmedInc",varlistCFR, varlistControl)
displayCoeff(dfCFR.LNmedInc,"CFR ~ LN Median Income, Regression Coefficient (OLS)", "Regression Coefficient")
dfCFR.LNmedInc

dfCFR.workersAcadem <- OLSvarlist2("workersAcadem",varlistCFR, varlistControl)
displayCoeff(dfCFR.workersAcadem,"CFR ~ Workers Academic Education, Regression Coefficient (OLS)", "Regression Coefficient")
dfCFR.workersAcadem


## Hypothesis II.1, Mediation of Hypothesis I via foreigners
dfIR.foreign <- SARvarlist2("GISDforeign",varlistLNIR, varlistControlForeign)
displayCoeff(dfIR.foreign,"LN IR ~ GISD * Share Foreigners, Average Total Effects (SAR)", "Average Total Effect")
dfIR.foreign

dfCFR.foreign <- OLSvarlist2("shareForeign",varlistCFR, varlistControlForeign)
displayCoeff(dfCFR.foreign,"CFR ~ GISD * Share Foreigners, Regression Coefficient (OLS)", "Regression Coefficient")
dfCFR.foreign

plot(dfds["shareForeign"], key.pos = 4, nbreaks = 10,border="white")

## Hypothesis II.2, Mediation of Hypothesis I via east-west
dfIR.east <- SARvarlist2("GISD",varlistLNIR, varlistControlEast)
displayCoeff(dfIR.east,"LN IR ~ GISD * east, Average Total Effects (SAR)", "Average Total Effect")
dfIR.east

dfCFR.east <- OLSvarlist2("GISD",varlistCFR, varlistControlEast)
displayCoeff(dfCFR.east,"CFR ~ GISD * east, Regression Coefficient (OLS)", "Regression Coefficient")
dfCFR.east

## Hypothesis E1, IR ~ AfD
dfIR.AfD <- SARvarlist2("AfD",varlistLNIR, varlistControl)
displayCoeff(dfIR.AfD,"AfD vote (2017)", "Average Total Effect")
dfIR.AfD

## Hypothesis E2, CFR ~ Healthcare facilities
dfCFR.popPerDoc <- OLSvarlist2("LNpopPerDoc",varlistCFR, varlistControl)
displayCoeff(dfCFR.popPerDoc,"CFR ~ LN People per Doctor, Regression Coefficient (OLS)", "Regression Coefficient")
dfCFR.popPerDoc

dfCFR.hospBeds <- OLSvarlist2("hospBeds",varlistCFR, varlistControl)
displayCoeff(dfCFR.hospBeds,"CFR ~ Hospital Beds p.c., Regression Coefficient (OLS)", "Regression Coefficient")
dfCFR.hospBeds

# normal distribution assumption
hist(dfdd$IR, breaks = c(500,1000,1500,2000,2500,3000,3500,4000,4500,
                         5000,5500,6000,6500,7000,7500,8000,8500,9000)) 
hist(dfdd$CFR) 
hist(dfdd$IR) 
hist(dfdd$IR21.04) 
shapiro.test(dfdd$IR) 

plot(dfdd$GISD,dfdd$IR)

plot(dfdd$AfD,dfdd$IR)

plot(dfdd$shareForeigners,dfdd$IR)

plot(dfdd$above65,dfdd$deathRate)


# Spatial regression
lag20.04 = lagsarlm(IR20.04 ~ medInc, data=dfds, listw = weighted_neighbors,
               tol.solve=1.0e-30, zero.policy=T)
lag21.04 = lagsarlm(IR21.04 ~ medInc, data=dfds, listw = weighted_neighbors,
               tol.solve=1.0e-30, zero.policy=T)
summary(lag20.04)
summary(lag21.04)

moran.plot(lag20.03$residuals, weighted_neighbors, zero.policy=T)
moran.plot(lag21.03$residuals, weighted_neighbors, zero.policy=T)

### Tables etc. for LaTeX Output

# table with independent variables 
stargazer(dfdd[c("GISD","unemployment","medInc","workersAcadem","popDensity",
                 "population", "shareWomen","avgAge","shareForeign",
                 "AfD","hospBeds","popPerDoc")], 
          type = "latex", digits=1,flip = FALSE, omit.summary.stat = 
            c("p25","p75"))

# table with dependent variables SHORT
stargazer(dfdd[c("CFR","IR","aggrCases","aggrDeaths","CFR20.04","CFR21.04","IR20.04",
                 "IR21.04")], 
          type = "latex", digits=1,flip = FALSE, omit.summary.stat = 
            c("median","p25","p75"))

# table with dependent variables LONG
stargazer(dfdd[c("CFR","IR","aggrCases","aggrDeaths","CFR20.02",
                 "CFR20.03","CFR20.04","CFR20.05","CFR20.06",
                 "CFR20.07","CFR20.08","CFR20.09","CFR20.10",
                 "CFR20.11","CFR20.12","CFR21.01","CFR21.02",
                 "CFR21.03","CFR21.04",
                 "IR20.02","IR20.03","IR20.04","IR20.05","IR20.06",
                 "IR20.07","IR20.08","IR20.09","IR20.10",
                 "IR20.11","IR20.12","IR21.01","IR21.02",
                 "IR21.03","IR21.04")], 
          type = "latex", digits=1,flip = FALSE, omit.summary.stat = 
            c("median","p25","p75"))

# SAR effects LN IR ~ GISD
effects_LNIR.GISD <- SARvarlist3("GISD",varlistLNIR, varlistControl)
effects_LNIR.GISD[["LNIR20.02"]]
effects_LNIR.GISD[["LNIR20.03"]]
effects_LNIR.GISD[["LNIR20.04"]]
effects_LNIR.GISD[["LNIR20.05"]]
effects_LNIR.GISD[["LNIR20.06"]]
effects_LNIR.GISD[["LNIR20.07"]]
effects_LNIR.GISD[["LNIR20.08"]]
effects_LNIR.GISD[["LNIR20.09"]]
effects_LNIR.GISD[["LNIR20.10"]]
effects_LNIR.GISD[["LNIR20.11"]]
effects_LNIR.GISD[["LNIR20.12"]]
effects_LNIR.GISD[["LNIR21.01"]]
effects_LNIR.GISD[["LNIR21.02"]]
effects_LNIR.GISD[["LNIR21.03"]]
effects_LNIR.GISD[["LNIR21.04"]]

#Maps
plot(dfds["IR"], key.pos = 4, nbreaks = 10,border="white")
plot(dfds["IR20.04"], key.pos = 4, nbreaks = 10,border="white")
plot(dfds["IR21.04"], key.pos = 4, nbreaks = 10,border="white")
plot(dfds["CFR"], key.pos = 4, nbreaks = 10,border="white")
plot(dfds["CFR20.04"], key.pos = 4, nbreaks = 10,border="white")
plot(dfds["CFR21.04"], key.pos = 4, nbreaks = 10,border="white")
plot(dfds["GISD"], key.pos = 4, nbreaks = 10,border="white")

#Course of Pandemic in Germany
dfaggrIR <- dfdd[,varlistIR]
dfaggrIRmean <- colMeans(dfaggrIR)
dfaggrIRmean <- data.frame(dfaggrIRmean)
dfaggrIRmean$month <- month
names(dfaggrIRmean)[names(dfaggrIRmean)=="dfaggrIRmean"] <- "IR"
dfaggrIRmean

dfaggrCFR <- dfdd[,varlistCFR]
dfaggrCFRmean <- colMeans(dfaggrCFR)
dfaggrCFRmean <- data.frame(dfaggrCFRmean)
dfaggrCFRmean$month <- month
names(dfaggrCFRmean)[names(dfaggrCFRmean)=="dfaggrCFRmean"] <- "CFR"
dfaggrCFRmean

ggplot(dfaggrCFRmean, aes(x=month, y=CFR, group=1)) +
  geom_line() +
  geom_point() +
  scale_y_continuous()+ 
  scale_x_discrete(limits=dfaggrCFRmean$month)+
  labs(title="Average county-CFR per month",x="Month", y = "CFR")+
  theme_bw()


ggplot(dfaggrIRmean, aes(x=month, y=IR, group=1)) +
  geom_line() +
  geom_point() +
  scale_y_continuous()+ 
  scale_x_discrete(limits=dfaggrIRmean$month)+
  labs(title="Average county-IR per month",x="Month", y = "CFR")+
  theme_bw()

