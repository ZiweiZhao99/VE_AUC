library(tidyverse)
library(survival)

# function: risktime(start, end, start.t, end.t)
# use this to add a column to the dataframe calculating the time at risk during certain interval from *start.t* to *end.t*
# for suvival data with recurrent events
# *start*: start time in the data
# *end*: end time in the data
# *start.t*: start time of a certain interval of interest
# *end.t*: end time of a certain interval of interest
risktime <- function(start, end, start.t, end.t){ 
  case_when(
    # experimental interval exclusively before given interval
    end<=start.t ~ 0, 
    # experimental interval intersects with given interval on the left
    start<=start.t & end<=end.t ~ end-start.t,
    # given interval within experimental interval
    start<=start.t & end>end.t ~ end.t-start.t,
    # experimental interval within given interval
    start>start.t & end<=end.t ~ end-start,
    #experimental interval intersects with given interval on the right
    start>start.t & start<end.t & end>end.t ~ end.t-start,
    # experimental interval exclusively after given interval
    TRUE ~ 0)
}

# function: trial_convert(dataframe)
# add 4 columns to a dataframe of survival data representing time at risk of each row during each 3-month interval
# assume the total follow-up time is no more that 12 months
trial_convert <- function (data) {
  mutate(data,
         risktime1 = risktime(start, end, 0, 3),
         risktime2 = risktime(start, end, 3, 6),
         risktime3 = risktime(start, end, 6, 9),
         risktime4 = risktime(start, end, 9, 12)
  )
}
start.0 <- function (data) {
  mutate(data,
         risktime1 = risktime(start, end, 0, 3),
         risktime2 = risktime(start, end, 3, 6),
         risktime3 = risktime(start, end, 6, 9),
         risktime4 = risktime(start, end, 9, 12)
  )
}

start.1 <- function (data) {
  mutate(data,
         risktime1 = risktime(start, end, 1, 4),
         risktime2 = risktime(start, end, 4, 7),
         risktime3 = risktime(start, end, 7, 10),
         risktime4 = risktime(start, end, 10, 12)+risktime(start, end, 0, 1)
  )
}

start.2 <- function (data) {
  mutate(data,
         risktime1 = risktime(start, end, 2, 5),
         risktime2 = risktime(start, end, 5, 8),
         risktime3 = risktime(start, end, 8, 11),
         risktime4 = risktime(start, end, 11, 12)+risktime(start, end, 0, 2)
  )
}

start.3 <- function (data) {
  mutate(data,
         risktime1 = risktime(start, end, 3, 6),
         risktime2 = risktime(start, end, 6, 9),
         risktime3 = risktime(start, end, 9, 12),
         risktime4 = risktime(start, end, 0, 3)
  )
}

start.4 <- function (data) {
  mutate(data,
         risktime1 = risktime(start, end, 4, 7),
         risktime2 = risktime(start, end, 7, 10),
         risktime3 = risktime(start, end, 10, 12)+risktime(start, end, 0, 1),
         risktime4 = risktime(start, end, 1, 4)
  )
}
start.5 <- function (data) {
  mutate(data,
         risktime1 = risktime(start, end, 5, 8),
         risktime2 = risktime(start, end, 8, 11),
         risktime3 = risktime(start, end, 11, 12)+risktime(start, end, 0, 2),
         risktime4 = risktime(start, end, 2, 5)
  )
}

start.6 <- function (data) {
  mutate(data,
         risktime1 = risktime(start, end, 6, 9),
         risktime2 = risktime(start, end, 9, 12),
         risktime3 = risktime(start, end, 0, 3),
         risktime4 = risktime(start, end, 3, 6)
  )
}

start.7 <- function (data) {
  mutate(data,
         risktime1 = risktime(start, end, 7, 10),
         risktime2 = risktime(start, end, 10, 12)+risktime(start, end, 0, 1),
         risktime3 = risktime(start, end, 1, 4),
         risktime4 = risktime(start, end, 4, 7)
  )
}

start.8 <- function (data) {
  mutate(data,
         risktime1 = risktime(start, end, 8, 11),
         risktime2 = risktime(start, end, 11, 12)+risktime(start, end, 0, 2),
         risktime3 = risktime(start, end, 2, 5),
         risktime4 = risktime(start, end, 5, 8)
  )
}

start.9 <- function (data) {
  mutate(data,
         risktime1 = risktime(start, end, 9, 12),
         risktime2 = risktime(start, end, 0, 3),
         risktime3 = risktime(start, end, 3, 6),
         risktime4 = risktime(start, end, 6, 9)
  )
}

start.10 <- function (data) {
  mutate(data,
         risktime1 = risktime(start, end, 10, 12)+risktime(start, end, 0, 1),
         risktime2 = risktime(start, end, 1, 4),
         risktime3 = risktime(start, end, 4, 7),
         risktime4 = risktime(start, end, 7, 10)
  )
}

start.11 <- function (data) {
  mutate(data,
         risktime1 = risktime(start, end, 11, 12)+risktime(start, end, 0, 2),
         risktime2 = risktime(start, end, 2, 5),
         risktime3 = risktime(start, end, 5, 8),
         risktime4 = risktime(start, end, 8, 11)
  )
}

trial_convert.10 <- function (data) {
  mutate(data,
         risktime1 = risktime(start, end, 0, 3),
         risktime2 = risktime(start, end, 3, 6),
         risktime3 = risktime(start, end, 6, 9),
         risktime4 = risktime(start, end, 9, 10)
  )
}

# Analytic solution of 1-exp(ax+b):
auc.fun <- function(t, beta0, beta1) t-exp(beta0+beta1*t)/beta1

# function: cases_averted(dataframe)
# return a vector contains EXPERIMENTAL number of cases averted per 1000 subjects, calculated by:
# 1. number of cases in control - number of cases in intervention (abbr: simple)
# 2. use 3-month incidence to estimate number of cases in the intervention and control arm and calculate the difference (abbr: incidence)
cases_averted <- function(trial) {
  # simple: number of cases in control - number of cases in intervention
  simple.c = nrow(filter(trial, Z==0 & event==1))
  simple.i = nrow(filter(trial, Z==1 & event==1))
  simple <- simple.c-simple.i
  # incidence:  use 3-month incidence to estimate number of cases in the intervention and control arm and calculate number of cases averted
  events.c = filter(trial, event==1 & Z==0)
  events.i = filter(trial, event==1 & Z==1)
  incidence.c = sum(
    nrow(filter(events.c, event_type==1))/sum(pull(filter(trial, Z==0),risktime1))*3,
    nrow(filter(events.c, event_type==2))/sum(pull(filter(trial, Z==0),risktime2))*3,
    nrow(filter(events.c, event_type==3))/sum(pull(filter(trial, Z==0),risktime3))*3,
    nrow(filter(events.c, event_type==4))/sum(pull(filter(trial, Z==0),risktime4))*round(max(trial$end)-9, digit = 0))*1000
  incidence.i = sum(
    nrow(filter(events.i, event_type==1))/sum(pull(filter(trial, Z==1),risktime1))*3,
    nrow(filter(events.i, event_type==2))/sum(pull(filter(trial, Z==1),risktime2))*3,
    nrow(filter(events.i, event_type==3))/sum(pull(filter(trial, Z==1),risktime3))*3,
    nrow(filter(events.i, event_type==4))/sum(pull(filter(trial, Z==1),risktime4))*round(max(trial$end)-9, digit = 0))*1000
  incidence <- incidence.c - incidence.i
  c(simple.c,
    #simple.i, 
    simple, 
    incidence.c, 
    incidence.i, 
    incidence)
}


cases_averted.10 <- function(trial) {
  # simple: number of cases in control - number of cases in intervention
  simple.c = nrow(filter(trial, Z==0 & event==1 & end <= 10))
  simple.i = nrow(filter(trial, Z==1 & event==1 & end <= 10))
  simple <- simple.c-simple.i
  # incidence:  use 3-month incidence to estimate number of cases in the intervention and control arm and calculate number of cases averted
  events.c = filter(trial, event==1 & Z==0)
  events.i = filter(trial, event==1 & Z==1)
  incidence.c = sum(
    nrow(filter(events.c, event_type==1))/sum(pull(filter(trial, Z==0),risktime1))*3,
    nrow(filter(events.c, event_type==2))/sum(pull(filter(trial, Z==0),risktime2))*3,
    nrow(filter(events.c, event_type==3))/sum(pull(filter(trial, Z==0),risktime3))*3,
    nrow(filter(events.c, event_type==4))/sum(pull(filter(trial, Z==0),risktime4))*1)*1000
  incidence.i = sum(
    nrow(filter(events.i, event_type==1))/sum(pull(filter(trial, Z==1),risktime1))*3,
    nrow(filter(events.i, event_type==2))/sum(pull(filter(trial, Z==1),risktime2))*3,
    nrow(filter(events.i, event_type==3))/sum(pull(filter(trial, Z==1),risktime3))*3,
    nrow(filter(events.i, event_type==4))/sum(pull(filter(trial, Z==1),risktime4))*1)*1000
  incidence <- incidence.c - incidence.i
  c(simple.c,
    #simple.i, 
    simple, 
    incidence.c, 
    incidence.i, 
    incidence)
}


# function: cases_averted.auc(dataframe, beta1, beta2)
# return the THEORETICAL number of cases averted per 1000 subjects
# -- first using 3-month incidence to estimate number of cases in the control arm only
# -- then apply %risk reduction estimated from auc of vaccine trajectory of efficacy to calculate number of cases averted
cases_averted.auc <- function(trial, beta0, beta1){
  events.c = filter(trial, event==1 & Z==0)
  auc.cases = 
    nrow(filter(events.c, event_type==1))/sum(pull(filter(trial, Z==0),risktime1))*3*1000*((auc.fun(3, beta0, beta1)-auc.fun(0, beta0, beta1))/3)+
    nrow(filter(events.c, event_type==2))/sum(pull(filter(trial, Z==0),risktime2))*3*1000*((auc.fun(6, beta0, beta1)-auc.fun(3, beta0, beta1))/3)+
    nrow(filter(events.c, event_type==3))/sum(pull(filter(trial, Z==0),risktime3))*3*1000*((auc.fun(9, beta0, beta1)-auc.fun(6, beta0, beta1))/3)+
    nrow(filter(events.c, event_type==4))/sum(pull(filter(trial, Z==0),risktime4))*round(max(trial$end)-9, digits = 0)*1000*((auc.fun(round(max(trial$end), digits = 0), beta0, beta1)-auc.fun(9, beta0, beta1))/round(max(trial$end)-9, digits = 0))
  auc.cases
}

cases_averted.auc.10 <- function(trial, beta0, beta1){
  events.c = filter(trial, event==1 & Z==0)
  auc.cases = 
    nrow(filter(events.c, event_type==1))/sum(pull(filter(trial, Z==0),risktime1))*3*1000*((auc.fun(3, beta0, beta1)-auc.fun(0, beta0, beta1))/3)+
    nrow(filter(events.c, event_type==2))/sum(pull(filter(trial, Z==0),risktime2))*3*1000*((auc.fun(6, beta0, beta1)-auc.fun(3, beta0, beta1))/3)+
    nrow(filter(events.c, event_type==3))/sum(pull(filter(trial, Z==0),risktime3))*3*1000*((auc.fun(9, beta0, beta1)-auc.fun(6, beta0, beta1))/3)+
    nrow(filter(events.c, event_type==4))/sum(pull(filter(trial, Z==0),risktime4))*1*1000*((auc.fun(10, beta0, beta1)-auc.fun(9, beta0, beta1))/1)
  auc.cases
}



List <- list()
# Start of for loop
for (q in 1:1) { #number of replicates
  ptm0 <- proc.time()

###################################################### Simulation #######################################################

#Sample size 2000 
n = 2000
z <- c(rep(1,n/2), rep(0, n/2)) # 1000 intervention and 1000 control

# Set parameters
beta0 = -4
beta1 = 0.33
beta = 0

######### Scenario 1: constant hazard & longer follow-up #########

# function to generate recurrent event times
lambda.t <- function(t) 0.15*exp(beta+(beta0+beta1*t)*z)

Tau = 12
### Trial1: constant hazard, longer follow-up, non-censored
Ctime <- rep(Tau, n) # Non-censored
lambda = 0.1575
data.l.nc <- NULL
for( i in 1:n) {
  
  T = vector()
  Tstar = 0
  j = 0
  
  while (Tstar < Ctime[i]) {
    R <- rexp(1,lambda)
    Tstar = Tstar+R
    v <- runif(1)
    if ((lambda.t(rep(Tstar,n))[i] >= v*lambda) & Tstar < Ctime[i]){
      j = j+1
      T[j] = Tstar
    }
  }
  
  id <- rep(i,j+1)
  Z <- rep(z[i],j+1)
  start <- c(0,T)
  end <- c(T,Ctime[i])
  event <- c(rep(1,j),0)
  add <- data.frame(cbind(id=id, start=start, end=end, event=event,  Z=Z))
  data.l.nc <- rbind(data.l.nc, add)
}

### Trial3: constant hazard, longer follow-up, censored
Ctime <- runif(n,0.6,1)*Tau # Censored
data.l.c <- NULL
for( i in 1:n) {
  
  T = vector()
  Tstar = 0
  j = 0
  
  while (Tstar < Ctime[i]) {
    R <- rexp(1,lambda)
    Tstar = Tstar+R
    v <- runif(1)
    if ((lambda.t(rep(Tstar,n))[i] >= v*lambda) & Tstar < Ctime[i]){
      j = j+1
      T[j] = Tstar
    }
  }
  
  id <- rep(i,j+1)
  Z <- rep(z[i],j+1)
  start <- c(0,T)
  end <- c(T,Ctime[i])
  event <- c(rep(1,j),0)
  add <- data.frame(cbind(id=id, start=start, end=end, event=event,  Z=Z))
  data.l.c <- rbind(data.l.c, add)
}

######### Scenario 2: constant hazard & shorter follow-up #########

### Trial2: constant hazard, shorter follow-up, non-censored
Tau = 10 
Ctime <- rep(Tau, n) # Non-censored
data.s.nc <- NULL
for( i in 1:n) {
  
  T = vector()
  Tstar = 0
  j = 0
  
  while (Tstar < Ctime[i]) {
    R <- rexp(1,lambda)
    Tstar = Tstar+R
    v <- runif(1)
    if ((lambda.t(rep(Tstar,n))[i] >= v*lambda) & Tstar < Ctime[i]){
      j = j+1
      T[j] = Tstar
    }
  }
  
  id <- rep(i,j+1)
  Z <- rep(z[i],j+1)
  start <- c(0,T)
  end <- c(T,Ctime[i])
  event <- c(rep(1,j),0)
  add <- data.frame(cbind(id=id, start=start, end=end, event=event,  Z=Z))
  data.s.nc <- rbind(data.s.nc, add)
}


### Trial4: constant hazard, shorter follow-up, non-censored
Ctime <- runif(n,0.6,1)*Tau # Censored
data.s.c <- NULL
for( i in 1:n) {
  
  T = vector()
  Tstar = 0
  j = 0
  
  while (Tstar < Ctime[i]) {
    R <- rexp(1,lambda)
    Tstar = Tstar+R
    v <- runif(1)
    if ((lambda.t(rep(Tstar,n))[i] >= v*lambda) & Tstar < Ctime[i]){
      j = j+1
      T[j] = Tstar
    }
  }
  
  id <- rep(i,j+1)
  Z <- rep(z[i],j+1)
  start <- c(0,T)
  end <- c(T,Ctime[i])
  event <- c(rep(1,j),0)
  add <- data.frame(cbind(id=id, start=start, end=end, event=event,  Z=Z))
  data.s.c <- rbind(data.s.c, add)
}


######### Scenario 3: seasonal incidence low-high #########

# function to generate recurrent event times
lambda.t <- function(t) (0.1*(t<6)+0.2*(t>=6))*exp(beta+(beta0+beta1*t)*z)
lambda.i <- function(t) (0.1*(t<6)+0.2*(t>=6))*exp(beta+beta0+beta1*t) #hazard of intervention arm
lambda.c <- function(t) (0.1*(t<6)+0.2*(t>=6))*exp(beta) #hazard of control arm


### Trial5: seasonal incidence low-high, non-censored
Tau = 12
Ctime <- rep(Tau, n) #Non censored
lambda = 1.05*max(lambda.i(12), 0.2)
data.s1.nc <- NULL
for( i in 1:n) {
  
  T = vector()
  Tstar = 0
  j = 0
  
  while (Tstar < Ctime[i]) {
    R <- rexp(1,lambda)
    Tstar = Tstar+R
    v <- runif(1)
    if ((lambda.t(rep(Tstar,n))[i] >= v*lambda) & Tstar < Ctime[i]){
      j = j+1
      T[j] = Tstar
    }
  }
  
  id <- rep(i,j+1)
  Z <- rep(z[i],j+1)
  start <- c(0,T)
  end <- c(T,Ctime[i])
  event <- c(rep(1,j),0)
  add <- data.frame(cbind(id=id, start=start, end=end, event=event,  Z=Z))
  data.s1.nc <- rbind(data.s1.nc, add)
}

### Trial7: seasonal incidence low-high, censored
Ctime <- runif(n,0.6,1)*Tau #Censored
data.s1.c <- NULL
for( i in 1:n) {
  
  T = vector()
  Tstar = 0
  j = 0
  
  while (Tstar < Ctime[i]) {
    R <- rexp(1,lambda)
    Tstar = Tstar+R
    v <- runif(1)
    if ((lambda.t(rep(Tstar,n))[i] >= v*lambda) & Tstar < Ctime[i]){
      j = j+1
      T[j] = Tstar
    }
  }
  
  id <- rep(i,j+1)
  Z <- rep(z[i],j+1)
  start <- c(0,T)
  end <- c(T,Ctime[i])
  event <- c(rep(1,j),0)
  add <- data.frame(cbind(id=id, start=start, end=end, event=event,  Z=Z))
  data.s1.c <- rbind(data.s1.c, add)
}

######### Scenario 4: seasonal incidence high-low #########

# function to generate recurrent event times
lambda.t <- function(t) (0.2*(t<6)+0.1*(t>=6))*exp(beta+(beta0+beta1*t)*z)
lambda.i <- function(t) (0.2*(t<6)+0.1*(t>=6))*exp(beta+beta0+beta1*t) #hazard of intervention arm
lambda.c <- function(t) (0.2*(t<6)+0.1*(t>=6))*exp(beta) #hazard of control arm


### Trial6: seasonal incidence high-low, non-censored
Ctime <- rep(Tau, n) #Non censored
lambda = 1.05*max(lambda.i(12), 0.2)
data.s2.nc <- NULL
for( i in 1:n) {
  
  T = vector()
  Tstar = 0
  j = 0
  
  while (Tstar < Ctime[i]) {
    R <- rexp(1,lambda)
    Tstar = Tstar+R
    v <- runif(1)
    if ((lambda.t(rep(Tstar,n))[i] >= v*lambda) & Tstar < Ctime[i]){
      j = j+1
      T[j] = Tstar
    }
  }
  
  id <- rep(i,j+1)
  Z <- rep(z[i],j+1)
  start <- c(0,T)
  end <- c(T,Ctime[i])
  event <- c(rep(1,j),0)
  add <- data.frame(cbind(id=id, start=start, end=end, event=event,  Z=Z))
  data.s2.nc <- rbind(data.s2.nc, add)
}

### Trial8: seasonal incidence high-low, censored
Ctime <- runif(n,0.6,1)*Tau #Censored
data.s2.c <- NULL
for( i in 1:n) {
  
  T = vector()
  Tstar = 0
  j = 0
  
  while (Tstar < Ctime[i]) {
    R <- rexp(1,lambda)
    Tstar = Tstar+R
    v <- runif(1)
    if ((lambda.t(rep(Tstar,n))[i] >= v*lambda) & Tstar < Ctime[i]){
      j = j+1
      T[j] = Tstar
    }
  }
  
  id <- rep(i,j+1)
  Z <- rep(z[i],j+1)
  start <- c(0,T)
  end <- c(T,Ctime[i])
  event <- c(rep(1,j),0)
  add <- data.frame(cbind(id=id, start=start, end=end, event=event, Z=Z))
  data.s2.c <- rbind(data.s2.c, add)
}


###################################################### Modeling #########################################################

####### Trial 1 and 2 #######

#### Fit PH model
Y.l = Surv(data.l.nc$start, data.l.nc$end, data.l.nc$event==1)
Y.s = Surv(data.s.nc$start, data.s.nc$end, data.s.nc$event==1)

fit.ph.l <- coxph(Y.l ~ Z + cluster(id), data=data.l.nc, control = coxph.control(timefix = FALSE))
fit.ph.s <- coxph(Y.s ~ Z + cluster(id), data=data.s.nc, control = coxph.control(timefix = FALSE))

# Obtain estimates
trial1.beta <- fit.ph.l[[1]][[1]]
trial2.beta <- fit.ph.s[[1]][[1]]

# Calculate PE
trial1.PE <- 1-exp(trial1.beta)
trial2.PE <- 1-exp(trial2.beta)

##### Fit model with time-varying intervention effect
fit.extend.l <- coxph(Y.l ~ Z + tt(Z) + cluster(id), data=data.l.nc, tt=function(x, t,...) x*t, control = coxph.control(timefix = FALSE))
fit.extend.s <- coxph(Y.s ~ Z + tt(Z) + cluster(id), data=data.s.nc, tt=function(x, t,...) x*t, control = coxph.control(timefix = FALSE))

# Obtain estimates
trial1.beta0 <- fit.extend.l[[1]][[1]]
trial1.beta1 <- fit.extend.l[[1]][[2]]
trial2.beta0 <- fit.extend.s[[1]][[1]]
trial2.beta1 <- fit.extend.s[[1]][[2]]

# Calculate PE from AUC
f.trial1 <- function(t) t-exp(trial1.beta1*t+trial1.beta0)/trial1.beta1
f.trial2 <- function(t) t-exp(trial2.beta1*t+trial2.beta0)/trial2.beta1
trial1.auc = (f.trial1(12)-f.trial1(0))/12
trial2.auc = (f.trial2(10)-f.trial2(0))/10
trial1.auc.10 =  (f.trial1(10)-f.trial1(0))/10
trial2.auc.10 =  (f.trial2(10)-f.trial2(0))/10
####### Trial 3 and 4 #######

#### Fit PH model
Y.l.c = Surv(data.l.c$start, data.l.c$end, data.l.c$event==1)
Y.s.c = Surv(data.s.c$start, data.s.c$end, data.s.c$event==1)

fit.ph.l.c <- coxph(Y.l.c ~ Z + cluster(id), data=data.l.c, control = coxph.control(timefix = FALSE))
fit.ph.s.c <- coxph(Y.s.c ~ Z + cluster(id), data=data.s.c, control = coxph.control(timefix = FALSE))

# Obtain estimates
trial3.beta <- fit.ph.l.c[[1]][[1]]
trial4.beta <- fit.ph.s.c[[1]][[1]]

# Calculate PE
trial3.PE <- 1-exp(trial3.beta)
trial4.PE <- 1-exp(trial4.beta)

##### Fit model with time-varying intervention effect
fit.extend.l.c <- coxph(Y.l.c ~ Z + tt(Z) + cluster(id), data=data.l.c, tt=function(x, t,...) x*t, control = coxph.control(timefix = FALSE))
fit.extend.s.c <- coxph(Y.s.c ~ Z + tt(Z) + cluster(id), data=data.s.c, tt=function(x, t,...) x*t, control = coxph.control(timefix = FALSE))
# Obtain estimates
trial3.beta0 <- fit.extend.l.c[[1]][[1]]
trial3.beta1 <- fit.extend.l.c[[1]][[2]]
trial4.beta0 <- fit.extend.s.c[[1]][[1]]
trial4.beta1 <- fit.extend.s.c[[1]][[2]]

# Calculate PE from AUC
f.trial3 <- function(t) t-exp(trial3.beta1*t+trial3.beta0)/trial3.beta1
f.trial4 <- function(t) t-exp(trial4.beta1*t+trial4.beta0)/trial4.beta1
trial3.auc = (f.trial3(12)-f.trial3(0))/12
trial4.auc = (f.trial4(10)-f.trial4(0))/10
trial3.auc.10 = (f.trial3(10)-f.trial3(0))/10
trial4.auc.10 = (f.trial4(10)-f.trial4(0))/10

####### Trial 5 and 6 #######

#### Fit PH model
Y.s1 = Surv(data.s1.nc$start, data.s1.nc$end, data.s1.nc$event==1)
Y.s2 = Surv(data.s2.nc$start, data.s2.nc$end, data.s2.nc$event==1)

fit.ph.s1 <- coxph(Y.s1 ~ Z + cluster(id), data=data.s1.nc, control = coxph.control(timefix = FALSE))
fit.ph.s2 <- coxph(Y.s2 ~ Z + cluster(id), data=data.s2.nc, control = coxph.control(timefix = FALSE))

# Obtain estimates
trial5.beta <- fit.ph.s1[[1]][[1]]
trial6.beta <- fit.ph.s2[[1]][[1]]

# Calculate PE
trial5.PE <- 1-exp(trial5.beta)
trial6.PE <- 1-exp(trial6.beta)

##### Fit model with time-varying intervention effect
fit.extend.s1 <- coxph(Y.s1 ~ Z + tt(Z) + cluster(id), data=data.s1.nc, tt=function(x, t,...) x*t, control = coxph.control(timefix = FALSE))
fit.extend.s2 <- coxph(Y.s2 ~ Z + tt(Z) + cluster(id), data=data.s2.nc, tt=function(x, t,...) x*t, control = coxph.control(timefix = FALSE))

# Obtain estimates
trial5.beta0 <- fit.extend.s1[[1]][[1]]
trial5.beta1 <- fit.extend.s1[[1]][[2]]
trial6.beta0 <- fit.extend.s2[[1]][[1]]
trial6.beta1 <- fit.extend.s2[[1]][[2]]

# Calculate PE from AUC
f.trial5 <- function(t) t-exp(trial5.beta1*t+trial5.beta0)/trial5.beta1
f.trial6 <- function(t) t-exp(trial6.beta1*t+trial6.beta0)/trial6.beta1
trial5.auc = (f.trial5(12)-f.trial5(0))/12
trial6.auc = (f.trial6(12)-f.trial6(0))/12
trial5.auc.10 = (f.trial5(10)-f.trial5(0))/10
trial6.auc.10 = (f.trial6(10)-f.trial6(0))/10

####### Trial 7 and 8 #######

#### Fit PH model
Y.s1.c = Surv(data.s1.c$start, data.s1.c$end, data.s1.c$event==1)
Y.s2.c = Surv(data.s2.c$start, data.s2.c$end, data.s2.c$event==1)

fit.ph.s1.c <- coxph(Y.s1.c ~ Z + cluster(id), data=data.s1.c, control = coxph.control(timefix = FALSE))
fit.ph.s2.c <- coxph(Y.s2.c ~ Z + cluster(id), data=data.s2.c, control = coxph.control(timefix = FALSE))

# Obtain estimates
trial7.beta <- fit.ph.s1.c[[1]][[1]]
trial8.beta <- fit.ph.s2.c[[1]][[1]]

# Calculate PE
trial7.PE <- 1-exp(trial7.beta)
trial8.PE <- 1-exp(trial8.beta)

##### Fit model with time-varying intervention effect
fit.extend.s1.c <- coxph(Y.s1.c ~ Z + tt(Z) + cluster(id), data=data.s1.c, tt=function(x, t,...) x*t, control = coxph.control(timefix = FALSE))
fit.extend.s2.c <- coxph(Y.s2.c ~ Z + tt(Z) + cluster(id), data=data.s2.c, tt=function(x, t,...) x*t, control = coxph.control(timefix = FALSE))

# Obtain estimates
trial7.beta0 <- fit.extend.s1.c[[1]][[1]]
trial7.beta1 <- fit.extend.s1.c[[1]][[2]]
trial8.beta0 <- fit.extend.s2.c[[1]][[1]]
trial8.beta1 <- fit.extend.s2.c[[1]][[2]]

# Calculate PE from AUC
f.trial7 <- function(t) t-exp(trial7.beta1*t+trial7.beta0)/trial7.beta1
f.trial8 <- function(t) t-exp(trial8.beta1*t+trial8.beta0)/trial8.beta1
trial7.auc = (f.trial7(12)-f.trial7(0))/12
trial8.auc = (f.trial8(12)-f.trial8(0))/12
trial7.auc.10 = (f.trial7(10)-f.trial7(0))/10
trial8.auc.10 = (f.trial8(10)-f.trial8(0))/10

####### Summary table #######
table1 <-
  tribble(
    ~trial,   ~beta,        ~PE.ph,    ~beta0,       ~beta1,       ~PE.auc,    ~PE.auc.10,
    "trial1", trial1.beta,  trial1.PE, trial1.beta0, trial1.beta1, trial1.auc, trial1.auc.10,
    "trial2", trial2.beta,  trial2.PE, trial2.beta0, trial2.beta1, trial2.auc, trial2.auc.10,
    "trial3", trial3.beta,  trial3.PE, trial3.beta0, trial3.beta1, trial3.auc, trial3.auc.10,
    "trial4", trial4.beta,  trial4.PE, trial4.beta0, trial4.beta1, trial4.auc, trial4.auc.10,
    "trial5", trial5.beta,  trial5.PE, trial5.beta0, trial5.beta1, trial5.auc, trial5.auc.10,
    "trial6", trial6.beta,  trial6.PE, trial6.beta0, trial6.beta1, trial6.auc, trial6.auc.10,
    "trial7", trial7.beta,  trial7.PE, trial7.beta0, trial7.beta1, trial7.auc, trial7.auc.10,
    "trial8", trial8.beta,  trial8.PE, trial8.beta0, trial8.beta1, trial8.auc, trial8.auc.10
  )


################################################# Calculate number of cases averted ####################################################

trial1 <- data.l.nc
trial2 <- data.s.nc
trial3 <- data.l.c
trial4 <- data.s.c
trial5 <- data.s1.nc
trial6 <- data.s2.nc
trial7 <- data.s1.c
trial8 <- data.s2.c

################## Calculate cases averted up to planned follow-up ########################

df.list<-lapply(1:8, function(x) eval(parse(text=paste0("trial", x)))) # all trials in one list using their name
names(df.list)<-lapply(1:8, function(x) paste0("trial", x)) #Adding the name of each df in case you want to unlist the list afterwards
# Add columns of 3-month person-time at risk to each of the 8 trials
for (i in 1:length(df.list)) {
  df.list[[i]] <- trial_convert(df.list[[i]])%>%
    mutate(event_type = case_when(end <= 3 ~ 1,
                                  end <= 6 ~ 2,
                                  end <= 9 ~ 3,
                                  TRUE ~ 4))
}
list2env(df.list,.GlobalEnv) # return list back to the environment




# Apply cases_averted and cases_averted.auc to all trials
df.list<-lapply(1:8, function(x) eval(parse(text=paste0("trial", x)))) #all datasets in one list using their name
names(df.list)<-lapply(1:8, function(x) paste0("trial", x)) #Adding the name of each df in case you want to unlist the list afterwards
summary_cases <- data.frame(NULL)
for (i in 1:length(df.list)) {
  summary_cases <- rbind(summary_cases, cases_averted(df.list[[i]]))
}
colnames(summary_cases) <- c("cases_c.s", "averted.s", "cases_c.i", "cases_i.i", "averted.i")
averted.auc <- NULL
for (i in 1:length(df.list)) {
  averted.auc <- c(averted.auc, cases_averted.auc((df.list[[i]]), table1$beta0[i], table1$beta1[i]))
}


################## Calculate cases averted up to 10 months ########################


df.list<-lapply(1:8, function(x) eval(parse(text=paste0("trial", x)))) # all trials in one list using their name
names(df.list)<-lapply(1:8, function(x) paste0("trial", x)) #Adding the name of each df in case you want to unlist the list afterwards
# Add columns of 3-month person-time at risk to each of the 8 trials
for (i in 1:length(df.list)) {
  df.list[[i]] <- trial_convert.10(df.list[[i]])%>%
    mutate(event_type = case_when(end <= 3 ~ 1,
                                  end <= 6 ~ 2,
                                  end <= 9 ~ 3,
                                  end <= 10 ~ 4))
}
list2env(df.list,.GlobalEnv) # return list back to the environment

# Apply cases_averted and cases_averted.auc to all trials
df.list<-lapply(1:8, function(x) eval(parse(text=paste0("trial", x)))) #all datasets in one list using their name
names(df.list)<-lapply(1:8, function(x) paste0("trial", x)) #Adding the name of each df in case you want to unlist the list afterwards
summary_cases.10 <- data.frame(NULL)
for (i in 1:length(df.list)) {
  summary_cases.10 <- rbind(summary_cases.10, cases_averted.10(df.list[[i]]))
}
colnames(summary_cases.10) <- c("cases_c.s.10", "averted.s.10", "cases_c.i.10", "cases_i.i.10", "averted.i.10")
averted.auc.10 <- NULL
for (i in 1:length(df.list)) {
  averted.auc.10 <- c(averted.auc.10, cases_averted.auc.10((df.list[[i]]), table1$beta0[i], table1$beta1[i]))
}


################## Calculate cases averted up to planned follow-up ########################
################## if trial starts at 6th month ########################

df.list<-lapply(5:8, function(x) eval(parse(text=paste0("trial", x)))) # all trials in one list using their name
names(df.list)<-lapply(5:8, function(x) paste0("trial", x)) #Adding the name of each df in case you want to unlist the list afterwards
# Add columns of 3-month person-time at risk to each of the 8 trials
for (i in 1:length(df.list)) {
  df.list[[i]] <- start.6(df.list[[i]])%>%
    mutate(event_type = case_when(end <= 3 ~ 3,
                                  end <= 6 ~ 4,
                                  end <= 9 ~ 1,
                                  TRUE ~ 2))
}
list2env(df.list,.GlobalEnv) # return list back to the environment


# Apply cases_averted and cases_averted.auc to all trials
df.list<-lapply(5:8, function(x) eval(parse(text=paste0("trial", x)))) #all datasets in one list using their name
names(df.list)<-lapply(5:8, function(x) paste0("trial", x)) #Adding the name of each df in case you want to unlist the list afterwards
averted.auc.6th <- NULL
for (i in 1:length(df.list)) {
  averted.auc.6th <- c(averted.auc, cases_averted.auc((df.list[[i]]), table1$beta0[i+4], table1$beta1[i+4]))
}

averted.seasonal <- rbind(NA, NA, NA, NA, averted.auc.6th)


summary <- cbind(table1, summary_cases, averted.auc, summary_cases.10, averted.auc.10, averted.seasonal)%>%
  #mutate(PE.IRR = 1-cases_i.i/cases_c.i)%>%
  select(beta:PE.auc.10, averted.s, averted.s.10, averted.i, averted.i.10, averted.auc, averted.auc.10, averted.seasonal)

List[[q]]<- summary


ptm1=proc.time() - ptm0
jnk=as.numeric(ptm1[3])
cat('\n','It took ', jnk, "seconds to do iteration", q)
}
# end of for-loop


#save(List, file = "1000 replicated.RData")



