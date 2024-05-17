####################################################################################
####################################################################################
##  MICROSIMUALTION APPLICATION SHARE LIFE                                        ##
##                                                                                ##
##  Illustration 2: Changing Marital Status, Educational Attainment,              ##
##                  Fertility Status                                              ##
##                                                                                ##
##  SZ, Mai 2024                                                                  ##
####################################################################################
####################################################################################

x <- 0:60
par(mfrow=c(2,2))

lIm <- max(fert1Rates(age=x,calTime=2000), fert1Rates(age=x,calTime=2021))
plot(x,fert1Rates(age=x,calTime=2000),type="l",xlab="age",ylab="rate",
     main="0->1",cex.main=1.4,cex.lab=1.4, col="blue", ylim=c(0,lIm))
lines(x,fert1Rates(age=x,calTime=2021),col="red")
legend("topleft", c("<=2020",">2020"), fill=c("blue","red"))

lIm <- max(fert2Rates(age=x,calTime=2000, duration=1), fert2Rates(age=x,calTime=2021, duration=1))
plot(x,fert2Rates(age=x,calTime=2000, duration=1),type="l",xlab="age",ylab="rate",
     main="1->2+",cex.main=1.4,cex.lab=1.4, col="blue", ylim=c(0,lIm))
lines(x,fert2Rates(age=x,calTime=2021, duration=1),col="red")
#legend("topleft", c("<=2020",">2020"), fill=c("blue","red"))

lIm <- max(marriage1Rates(age=x,calTime=2000), marriage1Rates(age=x,calTime=2021))
plot(x,marriage1Rates(age=x,calTime=2000),type="l",xlab="age",ylab="rate",
     main="NM->M",cex.main=1.4,cex.lab=1.4, col="blue", ylim=c(0,lIm))
lines(x,marriage1Rates(age=x,calTime=2021),col="red")
#legend("topleft", c("<=2020",">2020"), fill=c("blue","red"))

lIm <- max(marriage2Rates(age=x,calTime=2000), marriage2Rates(age=x,calTime=2021))
plot(x,marriage2Rates(age=x,calTime=2000),type="l",xlab="age",ylab="rate",
     main="D/W->M",cex.main=1.4,cex.lab=1.4, col="blue", ylim=c(0,lIm))
lines(x,marriage2Rates(age=x,calTime=2021),col="red")
#legend("topleft", c("<=2020",">2020"), fill=c("blue","red"))

par(mfrow=c(2,2))
lIm <- max(divorceRates(age=x,calTime=2000), divorceRates(age=x,calTime=2021))
plot(x,divorceRates(age=x,calTime=2000),type="l",xlab="age",ylab="rate",
     main="M->D",cex.main=1.4,cex.lab=1.4, col="blue", ylim=c(0,lIm))
lines(x,divorceRates(age=x,calTime=2021),col="red")
legend("topleft", c("<=2020",">2020"), fill=c("blue","red"))

lIm <- max(widowhoodRates(age=x,calTime=2000))
plot(x,widowhoodRates(age=x),type="l",xlab="age",ylab="rate",
     main="M->W",cex.main=1.4,cex.lab=1.4, col="blue", ylim=c(0,lIm))

lIm <- max(lowToMedEduRates(age=x), medToHighEduRates(age=x))
plot(x,lowToMedEduRates(x),type="l",xlab="age",ylab="rate",
     main="Edu",cex.main=1.4,cex.lab=1.4, col="blue")
lines(x,medToHighEduRates(age=x),col="red")
legend("topright", c("low->med","med->high"), fill=c("blue","red"))

lIm <- max(mortRates(age=x,calTime=2000), mortRates(age=x,calTime=2021))
plot(x,mortRates(age=x,calTime=2000),type="l",xlab="age",ylab="rate",
     main="Mort",cex.main=1.4,cex.lab=1.4, col="blue", ylim=c(0,lIm))
lines(x,mortRates(age=x,calTime=2021),col="red")
legend("topleft", c("<=2020",">2020"), fill=c("blue","red"))

# Base Population
K <- cbind(initPop$initState, trunc((simHorizon[1]-initPop[,2])/365.25))
tab <- table(K[,1],K[,2])
tab <- tab[,order(as.numeric(colnames(tab)))]
par(mfrow=c(1,2))
par(mar=c(5, 4, 4, 0))
barplot(tab, col=rainbow(nrow(tab)))
par(mar=c(0,0,5,1))
plot(0,0,"n",axes=FALSE,xlab="",ylab="")
legend("topright", (rownames(tab)), fill=rainbow(nrow(tab)), ncol=4, cex=0.7)