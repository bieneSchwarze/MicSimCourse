####################################################################################
####################################################################################
##  MICROSIMUALTION APPLICATION HYPOTHETICAL CHANGES OVER THE LIFE COURSE         ##
##                                                                                ##
##  Illustration 2: Changing Marital Status, Educational Attainment,              ##
##                  Fertility Status                                              ##
##                                                                                ##
##  SZ, Mai 2024                                                                  ##
####################################################################################
####################################################################################

# ---------------------------------------------------------------------------------
# Clean workspace and load library 
rm(list=ls())
library(MicSim)
# ---------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------
# Defining simulation horizon
startDate <- 20140101 # yyyymmdd
endDate   <- 20241231 # yyyymmdd
simHorizon <- c(startDate=startDate, endDate=endDate)

# ---------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------
# Seed for random number generator
set.seed(2892)
# ---------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------
# Definition of maximal age
maxAge <- 110

# ---------------------------------------------------------------------------------
# Defining nonabsorbing and absorbing states
sex <- c("m","f")
fert <- c("0","1","2","3+")
marital <- c("NM","M","D","W")
edu <- c("no","low","med","high")
stateSpace <- expand.grid(sex=sex,fert=fert,marital=marital,edu=edu)
absStates <- c("dead","rest")

# ---------------------------------------------------------------------------------
# Specifying a base population
# (In this example: Create base population randomly, though, following some commonsense
# rules based on based on limits imposed by the law and by human nature.)
N = 10000                                                       
birthDates <- runif(N, min=getInDays(19500101), max=getInDays(20131231)) 
getRandInitState <- function(birthDate){
  age <- trunc((getInDays(simHorizon[1]) - birthDate)/365.25) 
  s1 <- sample(sex,1)
  s2 <- ifelse(age<=18, fert[1], sample(fert,1))
  s3 <- ifelse(age<=18, marital[1], ifelse(age<=22, sample(marital[1:3],1), sample(marital,1)))
  s4 <- ifelse(age<=7, edu[1], ifelse(age<=18, edu[2], ifelse(age<=23, sample(edu[2:3],1), sample(edu[-1],1))))
  initState <- paste(c(s1,s2,s3,s4),collapse="/")
  return(initState)
}
initPop <- data.frame(ID=1:N, birthDate=birthDates, initState=sapply(birthDates, getRandInitState))
initPop$birthDate <- getInDateFormat(initPop$birthDate)
range(initPop$birthDate)

# ---------------------------------------------------------------------------------
# Specifying the set of immigrants entering the population during simulation 
# (In this example: Create immigrants randomly, though, following some commonsense
# rules based on based on limits imposed by the law and by human nature.)
M = 2000                                                           
immigrDates <- runif(M, min=getInDays(20140101), max=getInDays(20241231)) 
immigrAges <- runif(M, min=15*365.25, max=70*365.25)
immigrBirthDates <- immigrDates - immigrAges
IDmig <- max(as.numeric(initPop[,"ID"]))+(1:M)
immigrPop <- data.frame(ID = IDmig, immigrDate = immigrDates, birthDate=immigrBirthDates, 
                        immigrInitState=sapply(immigrBirthDates, getRandInitState))  
immigrPop$birthDate <- getInDateFormat(immigrPop$birthDate)
immigrPop$immigrDate <- getInDateFormat(immigrPop$immigrDate)

# ---------------------------------------------------------------------------------
# Defining set of transition rates used
# Fertility rates: first parity
fert1Rates <- function(age, calTime, duration){
  b <- ifelse(calTime<=2020, 5.5, 5.1)
  c <- ifelse(calTime<=2020, 28, 29)
  rate <-  (b/c)*(c/age)^(3/2)*exp(-b^2*(c/age+age/c-2))
  rate[age<=15 | age>=45] <- 0
  return(rate)
}
# Fertility rates: second and higher order parity
fert2Rates <- function(age, calTime, duration){
  b <- ifelse(calTime<=2020, 6.0, 5.7)
  c <- ifelse(calTime<=2020, 32, 33)
  rate <-  (b/c)*(c/age)^(3/2)*exp(-b^2*(c/age+age/c-2))
  rate[age<=15 | age>=45 | duration<0.75] <- 0
  return(rate)
}
# Transition rates to first marriage
marriage1Rates <- function(age, calTime, duration){
  b <- ifelse(calTime<=2020, 0.07, 0.10)
  p <- ifelse(calTime<=2020, 2.7,2.7)
  lambda <- ifelse(calTime<=2020, 0.04, 0.03)
  rate <- b*p*(lambda*age)^(p-1)/(1+(lambda*age)^p)
  rate[age<=16] <- 0
  return(rate)
}
# Transition rates to higher order marriage
marriage2Rates <- function(age, calTime, duration){
  b <- ifelse(calTime<=2020, 0.15, 0.17)
  p <- ifelse(calTime<=2020, 2.7, 3)
  lambda <- ifelse(calTime<=2020, 0.09, 0.1)
  rate <- b*p*(lambda*age)^(p-1)/(1+(lambda*age)^p)
  rate[age<=22] <- 0
  return(rate)
}
# Divorce rates
divorceRates <- function(age, calTime, duration){
  m <- 40
  s <- ifelse(calTime<=2020, 7, 6)
  rate <- 3*dnorm(age,mean=m,sd=s)
  rate[age<=18] <- 0
  return(rate)
}
# Transition rates to widowhood
widowhoodRates <- function(age, calTime, duration){
  rate <- ifelse(age<=30, 0, pgamma(age-20, shape=6, rate=0.02))
  return(rate)
}
# Transition rates to elementary school enrollment
noToLowEduRates <- function(age, calTime, duration){
  rate <- ifelse(age==7,Inf,0)
  return(rate)
}
# Transition rates to lower secondary education
lowToMedEduRates <- function(age, calTime, duration){
  rate <- dnorm(age,mean=16,sd=1)
  rate[age<=15 | age>=25] <- 0
  return(rate)
}
# Transition rates to upper secondary or tertiary education
medToHighEduRates <- function(age, calTime, duration){
  rate <- dnorm(age,mean=20,sd=3)
  rate[age<=18 | age>=35] <- 0
  return(rate)
}
# Mortality rates
mortRates <- function(age, calTime, duration){
  a <- 0.00003
  b <- ifelse(calTime<=2020, 0.1, 0.097)
  rate <- a*exp(b*age)
  return(rate)
}
# Emigration rates
emigrRates <- function(age, calTime, duration){
  rate <- ifelse(age<=18,0,0.0025)
  return(rate)
}

# ---------------------------------------------------------------------------------
# Defining transition matrix
fertTrMatrix <- cbind(c("0->1","1->2","2->3+","3+->3+"),
  c("fert1Rates", "fert2Rates", "fert2Rates","fert2Rates"))
maritalTrMatrix <- cbind(c("NM->M","M->D","M->W","D->M","W->M"),
  c("marriage1Rates","divorceRates","widowhoodRates", "marriage2Rates","marriage2Rates"))
eduTrMatrix <- cbind(c("no->low","low->med","med->high"),
  c("noToLowEduRates","lowToMedEduRates","medToHighEduRates"))
allTransitions <- rbind(fertTrMatrix, maritalTrMatrix, eduTrMatrix)
absTransitions <- rbind(c("dead","mortRates"),c("rest","emigrRates"))
transitionMatrix <- buildTransitionMatrix(allTransitions=allTransitions,
  absTransitions=absTransitions,stateSpace=stateSpace)

# ---------------------------------------------------------------------------------
# Defining transitions triggering a birth event
fertTr <- fertTrMatrix[,1]
# Definition of initial states for newborns 
varInitStates <- rbind(c("m","0","NM","no"),c("f","0","NM","no")) 
# Definition of related occurrence probabilities
initStatesProb <- c(0.515,0.485)    
# ---------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------
# General month of enrollment to elementary school
monthSchoolEnrol <- 9
# ---------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------
# Execute microsimulation 
pop <- micSim(initPop=initPop, immigrPop=immigrPop, 
              transitionMatrix=transitionMatrix, 
              absStates=absStates, 
              varInitStates=varInitStates, 
              initStatesProb=initStatesProb, 
              maxAge=maxAge, 
              simHorizon=simHorizon, 
              fertTr=fertTr, 
              monthSchoolEnrol=monthSchoolEnrol)  
# ---------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------
# Executing microsimulation (multicore run, four cores are accessed)
# cores <- 4
# seeds <- seeds <- c(1233, 1245, 1234, 5)
# pop <- micSimParallel(initPop=initPop, immigrPop=immigrPop, 
#                       transitionMatrix=transitionMatrix, 
#                       absStates=absStates, 
#                       varInitStates=varInitStates, 
#                       initStatesProb=initStatesProb, 
#                       maxAge=maxAge, 
#                       simHorizon=simHorizon, 
#                       fertTr=fertTr, 
#                       monthSchoolEnrol=monthSchoolEnrol
#                       cores=cores, seeds=seeds)
# ---------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------
# Transforming microsimulation output into long format
popLong <- convertToLongFormat(pop,migr=TRUE)
# ---------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------
# Pick all newborns
ids <- c(initPop$ID, immigrPop$ID)
popLongNew <- popLong[!(popLong$ID %in% ids),]
popLongNew$SojTime <- (getInDays(popLongNew$Tstop) - getInDays(popLongNew$Tstart))/ 365.25 # compute waiting times in states
# ---------------------------------------------------------------------------------

