a2wage <- read.csv("C:/Users/韩佳芮/Desktop/econ102/A2/Wage70.csv",na.strings = "***",
                   header=TRUE)
a2wage_des <- read.csv("C:/Users/韩佳芮/Desktop/econ102/A2/wage-description.csv",na.strings = "***",
                       header=TRUE)
wage_male <- ts(a2wage[,"Males"],start=1997, end = 2019)
wage_female <- ts(a2wage[,"Females"],start=1997, end =2019)
wage_CPI <- ts(a2wage[,"CPI"],start=1997, end =2019)
wage_descript <- a2wage_des[70, ]

############# Q1 ####################
plot(wage_male, main="Hourly Nominal Wage for Males and Females", lwd=2,
     col="blue", ylim=c(10,40), ylab="Wage (dollars per hour)")
lines(wage_female, col="red" ,lty=2, lwd=2)
legend("topleft", c("Males", "Females"), col=c("blue","red"), lty=c(1,2))

############# Q2 ####################
RW_female <- wage_female/wage_CPI *100
RW_male <- wage_male/wage_CPI *100
plot(RW_male, main="Hourly Real Wage in Dollars of 2002 for Males and Females",
     ylab="Wage in Dollars of 2002 (dollars per hour)", ylim=c(15,25), col="blue",lwd=2)
lines(RW_female, col="red" ,lty=2, lwd=2)
legend("topleft", c("Males", "Females"), col=c("blue","red"), lty=c(1,2), bty="n")

############# Q3 ####################
t <- time(RW_female, offset=0.5)
femaleT <-coef(lm(RW_female~t))
maleT <-coef(lm(RW_male~t))
Ftrend <- femaleT[1] + femaleT[2]*t
Mtrend <- maleT[1] + maleT[2]*t
plot(Ftrend, lwd=2, cex.main=0.9, col="blue",
     main="Trend of Hourly Real Wage in Dollars of 2002 for Males and Females",
     ylab="Wage in Dollars of 2002 (dollars per hour)", ylim=c(15,28))
lines(Mtrend, lwd=2, col="red", lty=2)
legend("topleft", c("Males","Females"), col=c("blue","red"), lty=c(1,2), bty="n")

############# Q4 ####################
a <- RW_male-Mtrend
b<- RW_female-Ftrend
plot(a,b, cex.main=0.9, xlim=c(-1.5,1.5), ylim=c(-1.5,1.5), xy.labels=FALSE, xy.lines=FALSE,
     pch=17, ylab="Female Wage (dollars per hour)", xlab="Male Wage (dollars per hour)",
     main="Cyclical Components of Hourly Real Wage Series for Males vesus Females")

############# Part2 ####################
assigned_countries <- read.csv("C:/Users/韩佳芮/Desktop/econ102/A2/assigned-countries.csv",na.strings = "***",
                       header=TRUE)
realgdp <- read.csv("C:/Users/韩佳芮/Desktop/econ102/A2/realgdp.csv",na.strings = "***",
                               header=TRUE)
ass_con <- assigned_countries[70,]
BGR <- ts(realgdp$BGR, start=1970)
SAU <- ts(realgdp$SAU, start=1970)
ZWE <- ts(realgdp$ZWE, start=1970)
LBR <- ts(realgdp$LBR, start=1970)

lBGR <- log(BGR)
lSAU <- log(SAU)
lZWE <- log(ZWE)
lLBR <- log(LBR)
############# Q1 ####################
plot(lBGR, main="Real Per Capita GDP of the Four Countries (Log-scale)",
      lwd=2, col=1, ylim=c(5,15),
     ylab="Real per capita GDP")
lines(lSAU, lty=2, lwd=2, col=2)
lines(lZWE, lty=3, lwd=2, col=3)
lines(lLBR, lty=4, lwd=2, col=4)
legend(1968, 15.8, c("Bulgaria","Saudi Arabia","Zimbabwe","Liberia"), col=c(1,2,3,4), lty=c(1,2,3,4),
       lwd=c(2,2,2,2), bty="n", cex=0.6)

############# Q2 ####################
BGRt = time(lBGR)
SAUt = time(lSAU)
ZWEt = time(lZWE)
LBRt = time(lLBR)

BGRt2 <- BGRt^2
SAUt2 <- SAUt^2
ZWEt2 <- ZWEt^2
LBRt2 <- LBRt^2

coefBGRt <- coef(lm(lBGR~BGRt+BGRt2))
coefSAUt <- coef(lm(lSAU~SAUt+SAUt2))
coefZWEt <- coef(lm(lZWE~ZWEt+ZWEt2))
coefLBRt <- coef(lm(lLBR~LBRt+LBRt2))

trendBGR <- coefBGRt[1] + coefBGRt[2]*BGRt + coefBGRt[3]*BGRt2
trendSAU <- coefSAUt[1] + coefSAUt[2]*SAUt + coefSAUt[3]*SAUt2
trendZWE <- coefZWEt[1] + coefZWEt[2]*ZWEt + coefZWEt[3]*ZWEt2
trendLBR <- coefLBRt[1] + coefLBRt[2]*LBRt + coefLBRt[3]*LBRt2

detrendedBGR<-lBGR-trendBGR
detrendedSAU<-lSAU-trendSAU
detrendedZWE<-lZWE-trendZWE
detrendedLBR<-lLBR-trendLBR

plot(detrendedBGR, main="cyclical component of each series expressed in logs",
     lwd=2, col=1, ylim=c(-1.5,1),
     ylab="Real per capita GDP")
lines(detrendedSAU, lty=2, lwd=2, col=2)
lines(detrendedZWE, lty=3, lwd=2, col=3)
lines(detrendedLBR, lty=4, lwd=2, col=4)
legend("topleft", c("Bulgaria","Saudi Arabia","Zimbabwe","Liberia"), col=c(1,2,3,4), lty=c(1,2,3,4),
       lwd=c(2,2,2,2), bty="n", cex=0.6)


############# Q3 ####################

avg_g_BGR <- mean(diff(BGR)/lag(BGR,-1))*100
avg_g_SAU <- mean(diff(SAU)/lag(SAU,-1))*100
avg_g_ZWE <- mean(diff(ZWE)/lag(ZWE,-1))*100
avg_g_LBR <- mean(diff(LBR)/lag(LBR,-1))*100
g <- c(avg_g_BGR, avg_g_SAU, avg_g_ZWE, avg_g_LBR)
lGDP <- c(lBGR[1], lSAU[1], lZWE[1], lLBR[1])
plot(lGDP, g, xlab="Log Real per capita GDP (international dollars of 2011)", bg=2,
     pch=21, ylab="Average Annual Growth Rate (%)", ylim=c(-1, 5), xlim=c(7,10),
     main="Average Annual Growth Rate versus Real per Capita GDP in 1970 (Log-scale)",
     cex.main=0.98)
text(lGDP,g,c("Bulgaria","Saudi Arabia","Zimbabwe","Liberia"),pos=1)
############# Q4 ####################
rgdp <- ts(realgdp, start=1970)
realgdp1 <- window(rgdp, start=1973, end=1973)/1000
a <- hist(realgdp1, plot=FALSE, breaks=25)
plot(a, xaxt = "n",  ylim=c(0,50), cex.main=0.9, labels=TRUE,
     main="Distribution of Real Per Capita GDP in 1973",
     xlab="Thousands of International Dollars of 2011")
axis(1, a$mids, las=2, cex.axis=0.7, labels = c("0-1","2-3","5-6","7-8","9-10","11-12", "13-14","15-16","17-18","19-20","21-22","23-24","25-26","27-28","29-30",
                                                "31-32","33-34","35-36"))


realgdp2 <- window(rgdp, start=2014, end=2014)/1000
b <- hist(realgdp2, plot=FALSE, breaks=25)
plot(b, xaxt = "n", label=TRUE, cex.main=0.9, ylim=c(0,50),
     main="Distribution of Real Per Capita GDP \n Across 152 Countries in 2014",
     xlab="Thousands of International Dollars of 2011")
axis(1, b$mids, las=2, cex.axis=0.7, labels = c("0-1","2-3","5-6","7-8","9-10","11-12", "13-14","15-16","17-18","19-20","21-22","23-24","25-26","27-28","29-30",
                                                "31-32","33-34","35-36", "37-38"))
############# Q5 ####################
rgdp <- ts(realgdp, start=1970)
realgdp1 <- log(window(rgdp, start=1973, end=1973))
c <- hist(realgdp1, breaks=25, plot=FALSE)
plot(c, col="gray", label=TRUE, border="black", ylim=c(0,15), xaxt = "n", cex.main=0.8,
     main="Distribution of Real Per Capita GDP in 1973 (Log-Scale)",
     xlab="International Dollars of 2011")
axis(1, c$mids, las=2, cex.axis=0.7, labels = c("6.2-6.4","","6.6-6.8","","7-7.2","",
                                                "7.4-7.6","","7.8-8","","8.2-8.4","","8.6-8.8","","9-9.2","","9.4-9.6",
                                                "","9.8-10","","10.2-10.4"))
############# Q6 ###################
realgdp2 <- log(window(rgdp, start=2014, end=2014))
d <- hist(realgdp2, breaks=25, plot=FALSE)
plot(d, col="gray", label=TRUE, border="black", ylim=c(0,20), xaxt = "n", cex.main=0.8,
     main="Distribution of Real Per Capita GDP Across \n 152 Countries in 2014 (Log-Scale)",
     xlab="International Dollars of 2011")
axis(1, d$mids, las=2, cex.axis=0.7, labels = c("6.2-6.4","","6.6-6.8","","7-7.2","",
                                                "7.4-7.6","","7.8-8","","8.2-8.4","","8.6-8.8","","9-9.2","","9.4-9.6",
                                                "","9.8-10","","10.2-10.4","","10.6-10.8","","11-11.2", ""))