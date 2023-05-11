tempdata <- read.csv("dat48.csv",na.strings = "***",
                     header=TRUE)
tpda <- ts(tempdata["Expenditure"], frequency=4, start=c(1950,1))
tpda
############# Q1a ####################
plot(tpda,col="blue", lwd = 2, lty=1,
     ylab="Expenditure(thousand of dollars)",
     main="Expenditure from 1950 to 2020")

############# Q1b ####################
# # 
# logda <- log(tpda)
# plot(logda,col="red", lwd = 2, lty=1,
#      ylab="log-scale Expenditure",
#      main="Log-Scale Expenditure from 1950 to 2020")
# # abline(h=7,col=1,lwd = 2, lty=4)
# tt <- time(logda)
# fit<- lm(logda~tt)
# coefT <- coef(fit)
# trend<-coefT[1] + coefT[2]*tt
# lines(trend,col=1, lwd=3, lty=4)

################# Q1c ###############
# g <- diff(tpda)/lag(tpda,-1) ## no percentage yet
# g <- (1+g)^4-1 ## exact formula
# g <- g*100 ## back to percentage
# g
# plot(g,col="red", lwd = 2, lty=1,
#      ylab="growth rate(%)",
#      main="annualized growth rate of expenditure")
# abline(h=4,col=1,lwd = 2, lty=4)


################## Q2a ###############
plot(tpda,col="blue", lwd = 2, lty=1,
     ylab="Expenditure(thousand of dollars)",
     main="Expenditure from 1950 to 2020")

t <- time(tpda)
t2 <-t^2
fit1<- lm(tpda~t)
fit2 <- lm(tpda~t+t2)
coefT1 <- coef(fit1)
coefT2 <- coef(fit2)
trend1<-coefT1[1] + coefT1[2]*t
trend2 <- coefT2[1] + coefT2[2]*t + coefT2[3]*t2

lines(trend1,col=1, lwd=3, lty=4)
lines(trend2,col=2, lwd=3, lty=3)

legend("topleft", c("data", "linear trend", "quadratic trend"),col=c("blue",1,2), lty=1:4, lwd=2, cex=0.6)

################ Q2b ##############
# logda <- log(tpda)
# plot(logda,col="red", lwd = 2, lty=1,
#      ylab="log-scale Expenditure",
#      main="Log-Scale Expenditure from 1950 to 2020")
# tt <- time(logda)
# tt2<-t^2
# fit<- lm(logda~tt)
# fitt<-lm(logda~tt+tt2)
# coefT <- coef(fit)
# coefTT <- coef(fitt)
# trend<-coefT[1] + coefT[2]*tt
# trendd<-coefTT[1] + coefTT[2]*tt+coefTT[3]*tt2
# lines(trend,col=1, lwd=3, lty=4)
# lines(trendd,col=3, lwd=3, lty=4)
# legend("topleft", c("log-data", "linear trend", "quadratic trend"),col=c("blue",1,2), lty=1:4, lwd=2, cex=0.6)

################# Q2c #################
detrended<-logda-trendd
which.max(detrended)
which.min(detrended)
# 
# detrended
plot(detrended, lwd = 2, lty=1,
     ylab="log-scale Expenditure",
     main="Detrended log-scale Expenditure from 1950 to 2020")

################# Q2d #################
C <- filter(detrended, filter=rep(1/5,5))
decomposed <- decompose(detrended, filter=rep(1/5,5))
cyclical <- decomposed$trend
plot(cyclical, lwd = 2, lty=1,
          ylab="log-scale Expenditure",
          main="Cyclical Component of Expenditure from 1950 to 2020")
which.max(cyclical)
which.min(cyclical)
C

# ################# Q2e ##################
lowf <- trendd + cyclical
plot(lowf, lwd = 2, lty=1,
     ylab="log-scale Expenditure",
     main="Low Frequency of Expenditure from 1950 to 2020")
# 
# ################# Q2f ###################
seasonalc <- decomposed$figure
seasonalc
barplot(seasonalc, density=10, col="gray",
        border="red",
        main="Seasonal Component of Expenditure",
        ylab="log-scale Expenditure",
        xlab="Time")

################### Q3a #################
# logda <- log(tpda)
tempdata2 <- read.csv("dat149.csv",na.strings = "***",
                     header=TRUE)
tpda2 <- ts(tempdata2["Expenditure"], frequency=4, start=c(1950,1))
logda2 <- log(tpda2)
plot(logda,logda2, col="red",
     xlab="My Expenditure in logs",
     ylab="Selected Expenditure in logs",
     main="my series against the series in dat149 from 1950 to 2020",
     xy.labels=FALSE, xy.lines=FALSE,
     pch=2, bg=1)

################### Q3b #####################
# newt <- time(logda2)
# 
# newt2<-newt^2
# newfit<-lm(logda2~newt+newt2)
# newcoef<-coef(newfit)
# newtrend<-newcoef[1] + newcoef[2]*newt+newcoef[3]*newt2
# 
# detrended2<-logda2-newtrend
# decomposed2 <- decompose(detrended2, filter=rep(1/5,5))
# cyclical2 <- decomposed2$trend
# 
# plot(cyclical2, lwd = 2, lty=1,
#           ylab="Expenditure",
#           main="Cyclical Component2 of Expenditure from 1950 to 2020")
# 
# plot(cyclical,cyclical2, col="red",
#      xlab="My cyclical component",
#      ylab="Cyclical component in dat149",
#      main="My Cycle against the Cycle in dat149 from 1950 to 2020",
#      xy.labels=FALSE, xy.lines=FALSE,
#      pch=2, bg=1)