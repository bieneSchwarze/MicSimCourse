####################################################################################
####################################################################################
##  MICROSIMUALTION APPLICATION SHARE LIFE                                        ##
##                                                                                ##
##  Illustration 1: Transition to First Child  (aux file for plotting rates)      ##
##                                                                                ##
##  SZ, Mai 2024                                                                  ##
####################################################################################
####################################################################################

# from childless ParHome (to Alone, Cohab, Married, Child)
x <- 0:60
par(mfrow=c(2,2))
plot(x,ratesHA_f(age=x),type="s",xlab="age",ylab="rate",
     main="H -> A",cex.main=1.4,cex.lab=1.4)
plot(x,ratesHC_f(age=x),type="s",xlab="age",ylab="rate",
     main="H -> C",cex.main=1.4,cex.lab=1.4)
plot(x,ratesHM_f(age=x),type="s",xlab="age",ylab="rate",
     main="H -> M",cex.main=1.4,cex.lab=1.4)
plot(x,ratesHC_f(age=x),type="s",xlab="age",ylab="rate",
     main="H -> 1",cex.main=1.4,cex.lab=1.4)
     
# from childless Alone (to Cohab, Married, Child)
par(mfrow=c(3,3))
plot(x,ratesAC_f(age=x),type="s",xlab="age",ylab="rate",
     main="A -> C",cex.main=1.4,cex.lab=1.4)
plot(x,ratesAM_f(age=x),type="s",xlab="age",ylab="rate",
     main="A -> M",cex.main=1.4,cex.lab=1.4)
plot(x,ratesAK_f(age=x),type="s",xlab="age",ylab="rate",
     main="A -> 1",cex.main=1.4,cex.lab=1.4)

# from childless Cohab (to Alone, Married, Child)
plot(x,ratesCA_f(age=x),type="s",xlab="age",ylab="rate",
     main="C -> A",cex.main=1.4,cex.lab=1.4)
plot(x,ratesCM_f(age=x),type="s",xlab="age",ylab="rate",
     main="C -> M",cex.main=1.4,cex.lab=1.4)
plot(x,ratesCK_f(age=x),type="s",xlab="age",ylab="rate",
     main="C -> 1",cex.main=1.4,cex.lab=1.4)

# from childless Married (to Alone, Cohab, Child)
plot(x,ratesMA_f(age=x),type="s",xlab="age",ylab="rate",
     main="M -> A",cex.main=1.4,cex.lab=1.4)
plot(x,ratesMC_f(age=x),type="s",xlab="age",ylab="rate",
     main="M -> C",cex.main=1.4,cex.lab=1.4)
plot(x,ratesMK_f(age=x),type="s",xlab="age",ylab="rate",
     main="M -> 1",cex.main=1.4,cex.lab=1.4)