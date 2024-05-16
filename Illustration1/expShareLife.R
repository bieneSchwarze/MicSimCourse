####################################################################################
####################################################################################
##  MICROSIMUALTION APPLICATION SHARE LIFE                                        ##
##                                                                                ##
##  Illustration 1: Transition to First Child                                     ##
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
startDate <- 20100101 # yyyymmdd
endDate   <- 20301231 # yyyymmdd
simHorizon <- c(startDate=startDate, endDate=endDate)
# ---------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------
# Seed for random number generator
set.seed(234)
# ---------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------
# Definition of maximal age
maxAge <- 101
# ---------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------
# Definition of the state space (non-absorbing and absorbing states)
sex <- "f"                          
livArr <- c("H","A","C","M")
fert <- c("0","1")        
stateSpace <- expand.grid(sex=sex,livArr=livArr, fert=fert)
absStates <- "dead"   
# ---------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------
# Create initial population
createBirthDate <- function(age, refY){
  birthYear <- refY-1-age
  daysInBirthYear <- ifelse(((birthYear %% 4 == 0) & (birthYear %% 100 != 0)) || (birthYear %% 400 == 0),366,365) # born in a leap year?
  birthYearRefDays <- getInDays(as.numeric(paste(birthYear,"01", "01",sep=""))) 
  birthDateInDays <- birthYearRefDays + runif(1,min=0,max=daysInBirthYear) # pick a birth day in the birth year at random
  return(getInDateFormat(birthDateInDays))
}
N <- 100 # 100 female individuals per age group
initFemD <- rep(20:25,N) # considered age groups: 20 to 25, in sum 600 female individuals in the initial population
initFemB <- sapply(initFemD,createBirthDate, refY= trunc(startDate/10000))
initPop <-  data.frame(ID=1:length(initFemB), birthDate=initFemB, initState="f/H/0")  
# ---------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------
#Defintion of transition rates
setwd("D:\\HU\\SimSocScience\\BasicsCourse\\MicSim\\MicSim_Session\\Illustration1\\Data") # PLEASE SPECIFY PATH FOR RATES
load("rates.f.RData") 
dimnames(rates.f$M)
# From `parental home' to `living alone', females (1)
ratesHA_f <- function(age, calTime, duration){
  return(rates.f$M[age+1,2,1])
}
# From `parental home' to `cohabitation', females (2)
ratesHC_f <- function(age, calTime, duration){
  return(rates.f$M[age+1,3,1])
}
# From `parental home' to `married', females (3)
ratesHM_f <- function(age, calTime, duration){
  rate <- ifelse(age >=16, rates.f$M[age+1,4,1], 0)
  return(rate)
}
# From `parental home' to `child birth', females (4)
ratesHK_f <- function(age, calTime, duration){
  rate <- ifelse(age >=12 & age <=49, rates.f$M[age+1,5,1], 0)
  return(rate)
}
# From `living alone' to `cohabition', females (5)
ratesAC_f <- function(age, calTime, duration){
  return(rates.f$M[age+1,3,2])
}
# From `living alone' to `married', females (6)
ratesAM_f <- function(age, calTime, duration){
  rate <- ifelse(age >=16, rates.f$M[age+1,4,2], 0)
  return(rate)
}
# From `living alone' to `child birth', females (7)
ratesAK_f <- function(age, calTime, duration){
  rate <- ifelse(age >=12 & age <=49, rates.f$M[age+1,5,2], 0)
  return(rate)
}
# From `cohabition' to `living alone', females (8)
ratesCA_f <- function(age, calTime, duration){
  return(rates.f$M[age+1,2,3])
}
# From `cohabition' to `married', females (9)
ratesCM_f <- function(age, calTime, duration){
  rate <- ifelse(age >=16, rates.f$M[age+1,4,3], 0)
  return(rate)
}
# From `cohabition' to `child birth', females (10)
ratesCK_f <- function(age, calTime, duration){
  rate <- ifelse(age >=12 & age <=49, rates.f$M[age+1,5,3], 0)
  return(rate)
}
# From `married' to `living alone', females (11)
ratesMA_f <- function(age, calTime, duration){
  return(rates.f$M[age+1,2,4])
}
# From `married' to `cohabition', females (12)
ratesMC_f <- function(age, calTime, duration){
  return(rates.f$M[age+1,3,4])
}
# From `married' to `child birth', females (13)
ratesMK_f <- function(age, calTime, duration){
  rate <- ifelse(age >=12 & age <=49, rates.f$M[age+1,5,4], 0)
  return(rate)
}
# From `child birth' to `living alone', females (14)
ratesKA_f <- function(age, calTime, duration){
  return(rates.f$M[age+1,2,5])
}
# From `child birth' to `cohabition', females (15)
ratesKC_f <- function(age, calTime=1990, duration){
  return(rates.f$M[age+1,3,5])
}
# From `child birth' to `married', females (16)
ratesKM_f <- function(age, calTime, duration){
  return(rates.f$M[age+1,4,5])
}
# Mortality rates (17)
mortRates <- function(age, calTime, duration){
  rate <- ifelse(age==maxAge,Inf,0) 
  return(rate)
}
# If you want plot the rates and have a look (use file "plotRates.r")
# ---------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------
# Transition pattern and assignment of functions specifying transition rates
ParHomeTrMatrix_f <- cbind(c("f/H/0->f/A/0","f/H/0->f/C/0","f/H/0->f/M/0","f/H/0->f/H/1"),
                           c("ratesHA_f","ratesHC_f","ratesHM_f","ratesHK_f"))
LivAloneTrMatrix_f <- cbind(c("f/A/0->f/C/0","f/A/0->f/M/0", "f/A/0->f/A/1"),
                            c("ratesAC_f", "ratesAM_f","ratesAK_f"))
CohabitionTrMatrix_f <- cbind(c("f/C/0->f/A/0","f/C/0->f/M/0","f/C/0->f/C/1"), 
                              c("ratesCA_f","ratesCM_f","ratesCK_f"))
MarriedTrMatrix_f <- cbind(c("f/M/0->f/A/0","f/M/0->f/C/0","f/M/0->f/M/1"), 
                           c("ratesMA_f","ratesMC_f","ratesMK_f"))
allTransitions <- rbind(ParHomeTrMatrix_f, LivAloneTrMatrix_f, 
                        CohabitionTrMatrix_f, MarriedTrMatrix_f)
absTransitions <- rbind(c("dead","mortRates"))
transitionMatrix <- buildTransitionMatrix(allTransitions=allTransitions,
                                          absTransitions=absTransitions, stateSpace=stateSpace)
# ---------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------
# Run microsimulation (single core)
pop <- micSim(initPop=initPop,transitionMatrix=transitionMatrix, absStates=absStates,
              maxAge=maxAge, simHorizon=simHorizon)
# ---------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------
# Run microsimulation (multi-core)              
pop <- micSimParallel(initPop=initPop,transitionMatrix=transitionMatrix, absStates=absStates,
              maxAge=maxAge, simHorizon=simHorizon, cores=3, seeds=c(1254,23,456)) 
# ---------------------------------------------------------------------------------

# Convert output into Long format      
popLong <- convertToLongFormat(pop)
