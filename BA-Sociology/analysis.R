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

# Varlist for Models
varlistLNIR <- c("LNIR20.02","LNIR20.03","LNIR20.04","LNIR20.05","LNIR20.06",
                 "LNIR20.07","LNIR20.08","LNIR20.09","LNIR20.10",
                 "LNIR20.11","LNIR20.12","LNIR21.01","LNIR21.02",
                 "LNIR21.03","LNIR21.04","LNIR21.05","LNIR21.06")
varlistLNdeathRate <- c("LNdeathRate20.02","LNdeathRate20.03","LNdeathRate20.04","LNdeathRate20.05","LNdeathRate20.06",
                        "LNdeathRate20.07","LNdeathRate20.08","LNdeathRate20.09","LNdeathRate20.10",
                        "LNdeathRate20.11","LNdeathRate20.12","LNdeathRate21.01","LNdeathRate21.02",
                        "LNdeathRate21.03","LNdeathRate21.04","LNdeathRate21.05","LNdeathRate21.06")
varlistCFR <- c("CFR20.02","CFR20.03","CFR20.04","CFR20.05","CFR20.06",
                "CFR20.07","CFR20.08","CFR20.09","CFR20.10",
                "CFR20.11","CFR20.12","CFR21.01","CFR21.02",
                "CFR21.03","CFR21.04","CFR21.05","CFR21.06")
varlistCFRlag <- c("CFRlag20.04","CFRlag20.05","CFRlag20.06",
                   "CFRlag20.07","CFRlag20.08","CFRlag20.09","CFRlag20.10",
                   "CFRlag20.11","CFRlag20.12","CFRlag21.01","CFRlag21.02",
                   "CFRlag21.03","CFRlag21.04","CFRlag21.05","CFRlag21.06")
varlistControl <- c("shareWomen","age18.24","age25.29","age30.49","age50.64","age65.74","age75.84","age85")
varlistControlStates <- c("shareWomen","age18.24","age25.29","age30.49","age50.64","age65.74","age75.84","age85",
                          "SH","HH","NI","HB","MV","BB","BE","ST","SN","TH","NW","HE","RP","BY","SL")
month <- c("02/20","03/20","04/20","05/20","06/20","07/20","08/20","09/20","10/20",
           "11/20","12/20","01/21","02/21","03/21","04/21","05/21","06/21")

# Create lists needed for analysis
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


## Hypothesis II.1, Effect on Hypothesis I of foreigners
dfIR.foreign <- SARvarlist3("shareForeign","GISD",varlistLNIR,varlistControl)
displayCoeff(dfIR.foreign,"LN IR ~ Share Foreigners (+GISD), Average Total Effects (SAR)", "Average Total Effect")
dfIR.foreign

stargazer(dfIR.foreign, summary = FALSE, type = "latex")


dfCFR.foreign <- OLSvarlist3("shareForeign","GISD",varlistLNIR, varlistControl)
displayCoeff(dfCFR.foreign,"CFR ~ Share Foreigners (+GISD), Regression Coefficients (OLS)", "Regression Coefficient")
dfCFR.foreign

stargazer(dfCFR.foreign, summary = FALSE, type = "latex")


## Hypothesis II.2, Effect on Hypothesis I of east-west
dfIR.east <- SARvarlist3("GISD","east",varlistLNIR, varlistControl)
displayCoeff(dfIR.east,"LN IR ~ East (+GISD), Average Total Effects (SAR)", "Average Total Effect")
dfIR.east

stargazer(dfIR.east, summary = FALSE, type = "latex")

dfCFR.east <- OLSvarlist3("GISD","east",varlistLNIR, varlistControl)
displayCoeff(dfCFR.east,"CFR ~ East (+GISD), Regression Coefficients (OLS)", "Regression Coefficient")
dfCFR.east

stargazer(dfCFR.east, summary = FALSE, type = "latex")


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

## Robustness Check 
#CFR lagged
dfCFRlag.GISD <- OLSvarlist4("GISD",varlistCFRlag, varlistControl)
displayCoeff(dfCFRlag.GISD,"CFR (lagged) ~ GISD, Average Total Effects (SAR)", "Average Total Effect")
dfCFRlag.GISD
#IR with states
dfIR.GISD2 <- SARvarlist2("GISD",varlistLNIR, varlistControlStates)
displayCoeff(dfIR.GISD2,"LN IR ~ GISD, Average Total Effects (SAR)", "Average Total Effect")
dfIR.GISD2
#CFR with states
dfCFR.GISD3 <- SARvarlist2("GISD",varlistCFR, varlistControlStates)
displayCoeff(dfCFR.GISD3,"CFR ~ GISD, Average Total Effects (SAR)", "Average Total Effect")
dfCFR.GISD3


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
                 "CFR21.03","CFR21.04","CFR21.05","CFR21.06",
                 "IR20.02","IR20.03","IR20.04","IR20.05","IR20.06",
                 "IR20.07","IR20.08","IR20.09","IR20.10",
                 "IR20.11","IR20.12","IR21.01","IR21.02",
                 "IR21.03","IR21.04","IR21.05","IR21.06")], 
          type = "latex", digits=2,flip = FALSE, omit.summary.stat = 
            c("median","p25","p75"))

summary(dfdd$CFR20.07)
summary(dfdd$CFR20.08)
summary(dfdd$CFR20.09)
summary(dfdd$CFR20.10)
summary(dfdd$CFR20.11)
summary(dfdd$CFR21.04)
summary(dfdd$CFR21.05)
summary(dfdd$CFR20.02)

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
effects_LNIR.GISD[["LNIR21.05"]]
effects_LNIR.GISD[["LNIR21.06"]]

#Maps
plot(dfds["IR"], key.pos = 4, nbreaks = 10,border="white")
plot(dfds["IR20.04"], key.pos = 4, nbreaks = 10,border="white")
plot(dfds["IR21.04"], key.pos = 4, nbreaks = 10,border="white")
plot(dfds["CFR"], key.pos = 4, nbreaks = 10,border="white")
plot(dfds["CFR20.04"], key.pos = 4, nbreaks = 10,border="white")
plot(dfds["CFR21.04"], key.pos = 4, nbreaks = 10,border="white")
plot(dfds["GISD"], key.pos = 4, nbreaks = 10,border="white")

#Course of Pandemic in Germany
dfaggrCases <- dfrkiaggr[,c("aggrCases20.02","aggrCases20.03","aggrCases20.04","aggrCases20.05","aggrCases20.06",
                    "aggrCases20.07","aggrCases20.08","aggrCases20.09","aggrCases20.10",
                    "aggrCases20.11","aggrCases20.12","aggrCases21.01","aggrCases21.02",
                    "aggrCases21.03","aggrCases21.04","aggrCases21.05","aggrCases21.06")]
dfaggrCases$population <- dfdd$population
dfaggrCasesSum <- colSums(dfaggrCases)
dfaggrCasesSum <- data.frame(dfaggrCasesSum)
dfaggrCasesSum <- dfaggrCasesSum[-17]
dfaggrCasesSum <- data.frame(dfaggrCasesSum)
dfaggrCasesSum$month <- month
names(dfaggrCasesSum)[names(dfaggrCasesSum)=="dfaggrCasesSum"] <- "cases"
dfaggrCasesSum$IR <- 0
dfaggrCasesSum$IR <- (dfaggrCasesSum$cases / dfaggrCasesSum[18,1]) * 100000
dfaggrCasesSum <- dfaggrCasesSum[1:17,]

dfaggrCFR <- dfrkiaggr[,c("aggrDeaths20.02","aggrDeaths20.03","aggrDeaths20.04","aggrDeaths20.05","aggrDeaths20.06",
                            "aggrDeaths20.07","aggrDeaths20.08","aggrDeaths20.09","aggrDeaths20.10",
                            "aggrDeaths20.11","aggrDeaths20.12","aggrDeaths21.01","aggrDeaths21.02",
                            "aggrDeaths21.03","aggrDeaths21.04","aggrDeaths21.05","aggrDeaths21.06"
                            )]
dfaggrCFRSum <- colSums(dfaggrCFR)
dfaggrCFRSum <- data.frame(dfaggrCFRSum)
names(dfaggrCFRSum)[names(dfaggrCFRSum)=="dfaggrCFRSum"] <- "deaths"
dfaggrCFRSum$cases <- dfaggrCasesSum$cases
dfaggrCFRSum$month <- month
dfaggrCFRSum$CFR <- dfaggrCFRSum$deaths / dfaggrCFRSum$cases
dfaggrCFRSum

dfaggrCasesSum$month <- dfaggrCFRSum$month


ggplot(dfaggrCFRSum, aes(x=month, y=CFR, group=1)) +
  geom_line() +
  geom_point() +
  scale_y_continuous()+ 
  scale_x_discrete(limits=dfaggrCFRSum$month)+
  labs(title="Average German CFR per month",x="Month", y = "CFR")+
  theme_bw()


ggplot(dfaggrCasesSum, aes(x=month, y=IR, group=1)) +
  geom_line() +
  geom_point() +
  scale_y_continuous()+ 
  scale_x_discrete(limits=dfaggrCasesSum$month)+
  labs(title="Average German IR per month",x="Month", y = "IR")+
  theme_bw()

