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
                      "11/20","12/20","01/21","02/21","03/21","04/21")
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
                      "11/20","12/20","01/21","02/21","03/21","04/21")
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
                      "11/20","12/20","01/21","02/21","03/21","04/21")
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
                      "11/20","12/20","01/21","02/21","03/21","04/21")
  dfoutput$coefficientReg <- round(dfoutput$coefficientReg, digits = 10)
  dfoutput
}


# correlate a var against a previously defined list of variables, output as df
CORvarlist <- function(var, varList){
  dfoutput <- data.frame()
  for(a in varList) {
    inputvar1 <- a
    inputvar2 <- var
    b <- cor(dfdd[[inputvar1]], dfdd[[inputvar2]], method = "pearson",use="complete.obs")
    c <- data.frame(coefficientCor = b)
    dfoutput <- rbind(dfoutput,c)
  }
  dfoutput$month <- c("02/20","03/20","04/20","05/20","06/20","07/20","08/20","09/20","10/20",
                      "11/20","12/20","01/21","02/21","03/21","04/21")
  dfoutput$coefficientCor <- round(dfoutput$coefficientCor, digits = 10)
  dfoutput
}

# correlate and OLS regress a varlist in one function
COROLSvarlist <- function(var1, var2, varList){
  dfoutput1 <- CORvarlist(var1, varList)
  dfoutput2 <- OLSvarlist(var2, varList)
  dfoutput1 <- inner_join(dfoutput1,dfoutput2,by = "month")
  #dfoutput1 <- dfoutput1[,c(2,1,3)]  
  dfoutput1
}

# correlate and SAR regress a varlist in one function
CORSARvarlist <- function(var1, var2, varList){
  dfoutput1 <- CORvarlist(var1, varList)
  dfoutput2 <- SARvarlist(var2, varList)
  dfoutput1 <- inner_join(dfoutput1,dfoutput2,by = "month")
  #dfoutput1 <- dfoutput1[,c(2,1,3)]  
  dfoutput1
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
    scale_y_continuous(limits=c(min(df[,3]),max(df[,3])),labels=scaledigits)+ 
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
    scale_y_continuous(limits=c(ifelse(min(df[,1])<0,min(df[,1]),0),ifelse(max(df[,1])>0,max(df[,1]),0)),labels=scaledigits)+ 
    scale_x_discrete(limits=df$month)+
    theme_bw()
  annotate_figure(plot1, top = text_grob(title))
}

# SAR regress a var against a previously defined list of variables, output SAR effects
SARvarlist3 <- function(var, varList, controlList){
  dfoutput <- data.frame()
  listeffects <- list(Name="listeffects")
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
    listeffects[[a]] <- impacts(lag, listw = weighted_neighbors)
  }
  listeffects
}




