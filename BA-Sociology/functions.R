##########################################################
#                                                        #
# BA Sociology:                                          #
# "COVID-19 in Germany, a social-geographic perspective" #
#                                                        #
# 2021/04                                                #
#                                                        #
# Alexander Busch (alexander.busch@stud.uni-hd.de)       #
#                                                        #
# Functions for Data Analysis                            #
#                                                        #
##########################################################



# OLS regress a var against a previously defined list of variables, output as df
OLSvarlist <- function(var, varList){
  dfoutput <- data.frame()
  for(a in varList) {
    inputvar1 <- a
    inputvar2 <- var
    f <- as.formula(paste(inputvar1,paste(inputvar2, collapse = " + "), sep = "~"))
    b <- summary(lm(f,dfdd,na.action=na.omit))$coefficients
    c <- data.frame(coefficientReg = b[2, 1])
    dfoutput <- rbind(dfoutput,c)
  }
  dfoutput$month <- c("02/20","03/20","04/20","05/20","06/20","07/20","08/20","09/20","10/20",
                      "11/20","12/20","01/21","02/21","03/21","04/21","05/21","06/21")
  dfoutput$coefficientReg <- round(dfoutput$coefficientReg, digits = 10)
  dfoutput
}

# OLS regress a var against a previously defined list of variables, output as df, including controls
OLSvarlist2 <- function(var, varList, controlList){
  dfoutput <- data.frame()
  input2 <- c(var,controlList)
  for(a in varList) {
    inputvar1 <- a
    f <- as.formula(paste(inputvar1,paste(input2, collapse = " + "), sep = "~"))
    b <- summary(lm(f,dfdd,na.action=na.omit))$coefficients
    c <- data.frame(coefficientReg = b[2, 1])
    dfoutput <- rbind(dfoutput,c)
  }
  dfoutput$month <- c("02/20","03/20","04/20","05/20","06/20","07/20","08/20","09/20","10/20",
                      "11/20","12/20","01/21","02/21","03/21","04/21","05/21","06/21")
  dfoutput$coefficientReg <- round(dfoutput$coefficientReg, digits = 10)
  dfoutput
}

# OLS regress a var against a previously defined list of variables, 
   # for 2 variables
OLSvarlist3 <- function(var1, var2, varList, controlList){
  dfoutput <- data.frame()
  dfoutput1 <- data.frame()
  dfoutput2 <- data.frame()
  input2 <- c(var1,var2,controlList)
  for(a in varList) {
    inputvar1 <- a
    f <- as.formula(paste(inputvar1,paste(input2, collapse = " + "), sep = "~"))
    f <- as.formula(paste(inputvar1,paste(input2, collapse = " + "), sep = "~"))
    b <- summary(lm(f,dfdd,na.action=na.omit))$coefficients
    c <- data.frame(coefficientReg1 = b[2, 1])
    d <- data.frame(coefficientReg2 = b[3, 1])
    dfoutput1 <- rbind(dfoutput1,c)
    dfoutput2 <- rbind(dfoutput2,d)
  }
  m <- data.frame(month = c("02/20","03/20","04/20","05/20","06/20","07/20","08/20","09/20","10/20",
                            "11/20","12/20","01/21","02/21","03/21","04/21","05/21","06/21"))
  dfoutput <- rbind(dfoutput,m)
  dfoutput$coefficientReg <- round(dfoutput1$coefficientReg, digits = 5)
  dfoutput$coefficientReg2 <- round(dfoutput2$coefficientReg2, digits = 5)
  dfoutput
}

# OLS regress a var against a previously defined list of variables, output as df, including controls
   # without february and march for lagged CFR robustnesstest
OLSvarlist4 <- function(var, varList, controlList){
  dfoutput <- data.frame()
  input2 <- c(var,controlList)
  for(a in varList) {
    inputvar1 <- a
    f <- as.formula(paste(inputvar1,paste(input2, collapse = " + "), sep = "~"))
    b <- summary(lm(f,dfdd,na.action=na.omit))$coefficients
    c <- data.frame(coefficientReg = b[2, 1])
    dfoutput <- rbind(dfoutput,c)
  }
  dfoutput$month <- c("04/20","05/20","06/20","07/20","08/20","09/20","10/20",
                      "11/20","12/20","01/21","02/21","03/21","04/21","05/21","06/21")
  dfoutput$coefficientReg <- round(dfoutput$coefficientReg, digits = 10)
  dfoutput
}

# SAR regress a var against a previously defined list of variables, output as df
SARvarlist <- function(var, varList){
  dfoutput <- data.frame()
  for(a in varList) {
    inputvar1 <- a
    inputvar2 <- var
    f <- as.formula(paste(inputvar1,paste(inputvar2, collapse = " + "), sep = "~"))
    lag = lagsarlm(f, data=dfds, listw = weighted_neighbors,
                   tol.solve=1.0e-30, zero.policy=T)
    b <- impacts(lag, listw = weighted_neighbors)
    c <- data.frame(coefficientReg = as.numeric(b[3]))
    dfoutput <- rbind(dfoutput,c)
  }
  dfoutput$month <- c("02/20","03/20","04/20","05/20","06/20","07/20","08/20","09/20","10/20",
                      "11/20","12/20","01/21","02/21","03/21","04/21","05/21","06/21")
  dfoutput$coefficientReg <- round(dfoutput$coefficientReg, digits = 10)
  dfoutput
}

# SAR regress a var against a previously defined list of variables, output as df of avg total effects & rho, including controls
SARvarlist2 <- function(var, varList, controlList){
  dfoutput <- data.frame()
  dfoutput2 <- data.frame()
  input2 <- c(var,controlList)
  for(a in varList) {
    inputvar1 <- a
    f <- as.formula(paste(inputvar1,paste(input2, collapse = " + "), sep = "~"))
    lag = lagsarlm(f, data=dfds, listw = weighted_neighbors,
                   tol.solve=1.0e-30, zero.policy=T)
    b <- impacts(lag, listw = weighted_neighbors)
    b <- unlist(b)
    c <- data.frame(coefficientReg = as.numeric(b["total1"]))
    dfoutput <- rbind(dfoutput,c)
    d <- data.frame(rho = as.numeric(summary(lag)$rho))
    dfoutput2 <- rbind(dfoutput2,d)
  }
  dfoutput$rho <- dfoutput2$rho
  dfoutput$month <- c("02/20","03/20","04/20","05/20","06/20","07/20","08/20","09/20","10/20",
                      "11/20","12/20","01/21","02/21","03/21","04/21","05/21","06/21")
  dfoutput$coefficientReg <- round(dfoutput$coefficientReg, digits = 10)
  dfoutput
}

# SAR regress a var against a previously defined list of variables, 
 # output as df of avg total effects & rho, including controls
 # for 2 vars
SARvarlist3 <- function(var1, var2, varList, controlList){
  dfoutput <- data.frame()
  dfoutput1 <- data.frame()
  dfoutput2 <- data.frame()
  dfoutput3 <- data.frame()
  input2 <- c(var1,var2,controlList)
  for(a in varList) {
    inputvar1 <- a
    f <- as.formula(paste(inputvar1,paste(input2, collapse = " + "), sep = "~"))
    lag = lagsarlm(f, data=dfds, listw = weighted_neighbors,
                   tol.solve=1.0e-30, zero.policy=T)
    b <- impacts(lag, listw = weighted_neighbors)
    b <- unlist(b)
    c <- data.frame(coefficientReg1 = as.numeric(b["total1"]))
    d <- data.frame(coefficientReg2 = as.numeric(b["total2"]))
    dfoutput1 <- rbind(dfoutput1,c)
    dfoutput3 <- rbind(dfoutput3,d)
    g <- data.frame(rho = as.numeric(summary(lag)$rho))
    dfoutput2 <- rbind(dfoutput2,g)
  }
  m <- data.frame(month = c("02/20","03/20","04/20","05/20","06/20","07/20","08/20","09/20","10/20",
                            "11/20","12/20","01/21","02/21","03/21","04/21","05/21","06/21"))
  dfoutput <- rbind(dfoutput,m)
  dfoutput$coefficientReg <- round(dfoutput1$coefficientReg, digits = 5)
  dfoutput$coefficientReg2 <- round(dfoutput3$coefficientReg2, digits = 5)
  dfoutput$rho <- dfoutput2$rho
  dfoutput
}

# for displaying scales with 4 digits after .+ display two barplots over time
scaledigits <- function(x) sprintf("%.4f", x)

displayRegCor <- function(df,text,textReg,textCor){
  plot1 <- ggplot(df, aes(x=month, y=coefficientReg)) +
    geom_bar(stat="identity",
             colour="black", 
             fill = "white",
             size=.5,
             width=0.5) +
    scale_y_continuous(limits=c(min(df[,"coefficientReg"]),max(df[,"coefficientReg"])),labels=scaledigits)+ 
    scale_x_discrete(limits=df$month)+
    theme_bw()
  plot2 <- ggplot(df, aes(x=month, y=coefficientCor)) +
    geom_bar(stat="identity",
             colour="black", 
             fill = "white",
             size=.5,
             width=0.5) +
    scale_y_continuous(limits=c(-1,1),labels=scaledigits)+ 
    scale_x_discrete(limits=df$month)+
    theme_bw() 
  plot3 <- ggarrange(plot1,plot2,labels = c(textReg,textCor),ncol=1,nrow=2)
  annotate_figure(plot3, top = text_grob(text))
}

# display time and coefficients in barplots
displayCoeff <- function(df, title, yaxis){
  plot1 <- ggplot(df, aes(x=month, y=coefficientReg)) +
    geom_bar(stat="identity",
             colour="black", 
             fill = "white",
             size=.5,
             width=0.5) +
    xlab("Month") +
    ylab(yaxis) +
    scale_y_continuous(limits=c(ifelse(min(df[,"coefficientReg"])<0,min(df[,"coefficientReg"]),0)
                                ,ifelse(max(df[,"coefficientReg"])>0,max(df[,"coefficientReg"]),0)),labels=scaledigits)+ 
    scale_x_discrete(limits=df$month)+
    theme_bw()
  annotate_figure(plot1, top = text_grob(title))
}



