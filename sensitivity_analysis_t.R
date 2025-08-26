library(tidyverse)
library(survival)
library(furrr)

List_t <- list()
# Start of for loop
one_simulation <- function(q) { #number of replicates
  ptm0 <- proc.time()

 
###################################################### Simulation #######################################################
set.seed(q)
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
lambda.i <- function(t) (0.1*(t<6)+0.2*(t>=6))*exp(beta+(beta0+beta1*t)) #hazard of intervention arm
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
lambda.i <- function(t) (0.2*(t<6)+0.1*(t>=6))*exp(beta+(beta0+beta1*t)) #hazard of intervention arm
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

################################# Trial 1 and 2 ###################################

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

#################### Fit model with time-varying intervention effect x*t #################
fit.extend.l <- coxph(Y.l ~ Z + tt(Z) + cluster(id), data=data.l.nc, tt=function(x, t,...) x*t, control = coxph.control(timefix = FALSE))
fit.extend.s <- coxph(Y.s ~ Z + tt(Z) + cluster(id), data=data.s.nc, tt=function(x, t,...) x*t, control = coxph.control(timefix = FALSE))

# Obtain estimates
trial1.beta0.t <- fit.extend.l[[1]][[1]]
trial1.beta1.t <- fit.extend.l[[1]][[2]]
trial2.beta0.t <- fit.extend.s[[1]][[1]]
trial2.beta1.t <- fit.extend.s[[1]][[2]]

# Calculate PE from AUC
f.trial1.t <- function(t) t-exp(trial1.beta1.t*t+trial1.beta0.t)/trial1.beta1.t
f.trial2.t <- function(t) t-exp(trial2.beta1.t*t+trial2.beta0.t)/trial2.beta1.t
trial1.auc.t = (f.trial1.t(12)-f.trial1.t(0))/12
trial2.auc.t = (f.trial2.t(12)-f.trial2.t(0))/12
trial1.auc.t.10 =  (f.trial1.t(10)-f.trial1.t(0))/10
trial2.auc.t.10 =  (f.trial2.t(10)-f.trial2.t(0))/10

#################### Fit model with time-varying intervention effect x*log(t) #################
fit.extend.l <- coxph(Y.l ~ Z + tt(Z) + cluster(id), data=data.l.nc, tt=function(x, t,...) x*log(t), control = coxph.control(timefix = FALSE))
fit.extend.s <- coxph(Y.s ~ Z + tt(Z) + cluster(id), data=data.s.nc, tt=function(x, t,...) x*log(t), control = coxph.control(timefix = FALSE))

# Obtain estimates
trial1.beta0.logt <- fit.extend.l[[1]][[1]]
trial1.beta1.logt <- fit.extend.l[[1]][[2]]
trial2.beta0.logt <- fit.extend.s[[1]][[1]]
trial2.beta1.logt <- fit.extend.s[[1]][[2]]

# Calculate PE from AUC
f.trial1.logt <- function(t) t-(exp(trial1.beta0.logt)*t^(trial1.beta1.logt+1))/(trial1.beta1.logt+1)
f.trial2.logt <- function(t) t-(exp(trial2.beta0.logt)*t^(trial2.beta1.logt+1))/(trial2.beta1.logt+1)
trial1.auc.logt = (f.trial1.logt(12)-f.trial1.logt(0))/12
trial2.auc.logt = (f.trial2.logt(12)-f.trial2.logt(0))/12
trial1.auc.logt.10 =  (f.trial1.logt(10)-f.trial1.logt(0))/10
trial2.auc.logt.10 =  (f.trial2.logt(10)-f.trial2.logt(0))/10

#################### Fit model with time-varying intervention effect x*sqrt(t) #################
fit.extend.l <- coxph(Y.l ~ Z + tt(Z) + cluster(id), data=data.l.nc, tt=function(x, t,...) x*sqrt(t), control = coxph.control(timefix = FALSE))
fit.extend.s <- coxph(Y.s ~ Z + tt(Z) + cluster(id), data=data.s.nc, tt=function(x, t,...) x*sqrt(t), control = coxph.control(timefix = FALSE))

# Obtain estimates
trial1.beta0.sqrt <- fit.extend.l[[1]][[1]]
trial1.beta1.sqrt <- fit.extend.l[[1]][[2]]
trial2.beta0.sqrt <- fit.extend.s[[1]][[1]]
trial2.beta1.sqrt <- fit.extend.s[[1]][[2]]

# Calculate PE from AUC
f.trial1.sqrt <- function(t) t-2*(trial1.beta1.sqrt*sqrt(t)-1)*exp(trial1.beta1.sqrt*sqrt(t)+trial1.beta0.sqrt)/(trial1.beta1.sqrt**2)
f.trial2.sqrt <- function(t) t-2*(trial2.beta1.sqrt*sqrt(t)-1)*exp(trial2.beta1.sqrt*sqrt(t)+trial2.beta0.sqrt)/(trial2.beta1.sqrt**2)
trial1.auc.sqrt = (f.trial1.sqrt(12)-f.trial1.sqrt(0))/12
trial2.auc.sqrt = (f.trial2.sqrt(12)-f.trial2.sqrt(0))/12
trial1.auc.sqrt.10 =  (f.trial1.sqrt(10)-f.trial1.sqrt(0))/10
trial2.auc.sqrt.10 =  (f.trial2.sqrt(10)-f.trial2.sqrt(0))/10


############################################## Trial 3 and 4 ######################################

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

#################### Fit model with time-varying intervention effect x*t #################
fit.extend.l.c <- coxph(Y.l.c ~ Z + tt(Z) + cluster(id), data=data.l.c, tt=function(x, t,...) x*t, control = coxph.control(timefix = FALSE))
fit.extend.s.c <- coxph(Y.s.c ~ Z + tt(Z) + cluster(id), data=data.s.c, tt=function(x, t,...) x*t, control = coxph.control(timefix = FALSE))
# Obtain estimates
trial3.beta0.t <- fit.extend.l.c[[1]][[1]]
trial3.beta1.t <- fit.extend.l.c[[1]][[2]]
trial4.beta0.t <- fit.extend.s.c[[1]][[1]]
trial4.beta1.t <- fit.extend.s.c[[1]][[2]]

# Calculate PE from AUC
f.trial3.t <- function(t) t-exp(trial3.beta1.t*t+trial3.beta0.t)/trial3.beta1.t
f.trial4.t <- function(t) t-exp(trial4.beta1.t*t+trial4.beta0.t)/trial4.beta1.t
trial3.auc.t = (f.trial3.t(12)-f.trial3.t(0))/12
trial4.auc.t = (f.trial4.t(12)-f.trial4.t(0))/12
trial3.auc.t.10 = (f.trial3.t(10)-f.trial3.t(0))/10
trial4.auc.t.10 = (f.trial4.t(10)-f.trial4.t(0))/10

#################### Fit model with time-varying intervention effect x*log(t) #################
fit.extend.l.c <- coxph(Y.l.c ~ Z + tt(Z) + cluster(id), data=data.l.c, tt=function(x, t,...) x*log(t), control = coxph.control(timefix = FALSE))
fit.extend.s.c <- coxph(Y.s.c ~ Z + tt(Z) + cluster(id), data=data.s.c, tt=function(x, t,...) x*log(t), control = coxph.control(timefix = FALSE))
# Obtain estimates
trial3.beta0.logt <- fit.extend.l.c[[1]][[1]]
trial3.beta1.logt <- fit.extend.l.c[[1]][[2]]
trial4.beta0.logt <- fit.extend.s.c[[1]][[1]]
trial4.beta1.logt <- fit.extend.s.c[[1]][[2]]

# Calculate PE from AUC
f.trial3.logt <- function(t) t-(exp(trial3.beta0.logt)*t^(trial3.beta1.logt+1))/(trial3.beta1.logt+1)
f.trial4.logt <- function(t) t-(exp(trial4.beta0.logt)*t^(trial4.beta1.logt+1))/(trial4.beta1.logt+1)
trial3.auc.logt = (f.trial3.logt(12)-f.trial3.logt(0))/12
trial4.auc.logt = (f.trial4.logt(12)-f.trial4.logt(0))/12
trial3.auc.logt.10 = (f.trial3.logt(10)-f.trial3.logt(0))/10
trial4.auc.logt.10 = (f.trial4.logt(10)-f.trial4.logt(0))/10

#################### Fit model with time-varying intervention effect x*sqrt(t) #################
fit.extend.l.c <- coxph(Y.l.c ~ Z + tt(Z) + cluster(id), data=data.l.c, tt=function(x, t,...) x*sqrt(t), control = coxph.control(timefix = FALSE))
fit.extend.s.c <- coxph(Y.s.c ~ Z + tt(Z) + cluster(id), data=data.s.c, tt=function(x, t,...) x*sqrt(t), control = coxph.control(timefix = FALSE))
# Obtain estimates
trial3.beta0.sqrt <- fit.extend.l.c[[1]][[1]]
trial3.beta1.sqrt <- fit.extend.l.c[[1]][[2]]
trial4.beta0.sqrt <- fit.extend.s.c[[1]][[1]]
trial4.beta1.sqrt <- fit.extend.s.c[[1]][[2]]

# Calculate PE from AUC
f.trial3.sqrt <- function(t) t-2*(trial3.beta1.sqrt*sqrt(t)-1)*exp(trial3.beta1.sqrt*sqrt(t)+trial3.beta0.sqrt)/(trial3.beta1.sqrt**2)
f.trial4.sqrt <- function(t) t-2*(trial4.beta1.sqrt*sqrt(t)-1)*exp(trial4.beta1.sqrt*sqrt(t)+trial4.beta0.sqrt)/(trial4.beta1.sqrt**2)
trial3.auc.sqrt = (f.trial3.sqrt(12)-f.trial3.sqrt(0))/12
trial4.auc.sqrt = (f.trial4.sqrt(12)-f.trial4.sqrt(0))/12
trial3.auc.sqrt.10 = (f.trial3.sqrt(10)-f.trial3.sqrt(0))/10
trial4.auc.sqrt.10 = (f.trial4.sqrt(10)-f.trial4.sqrt(0))/10



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

#################### Fit model with time-varying intervention effect x*t #################
fit.extend.s1 <- coxph(Y.s1 ~ Z + tt(Z) + cluster(id), data=data.s1.nc, tt=function(x, t,...) x*t, control = coxph.control(timefix = FALSE))
fit.extend.s2 <- coxph(Y.s2 ~ Z + tt(Z) + cluster(id), data=data.s2.nc, tt=function(x, t,...) x*t, control = coxph.control(timefix = FALSE))

# Obtain estimates
trial5.beta0.t <- fit.extend.s1[[1]][[1]]
trial5.beta1.t <- fit.extend.s1[[1]][[2]]
trial6.beta0.t <- fit.extend.s2[[1]][[1]]
trial6.beta1.t <- fit.extend.s2[[1]][[2]]

# Calculate PE from AUC
f.trial5.t <- function(t) t-exp(trial5.beta1.t*t+trial5.beta0.t)/trial5.beta1.t
f.trial6.t <- function(t) t-exp(trial6.beta1.t*t+trial6.beta0.t)/trial6.beta1.t
trial5.auc.t = (f.trial5.t(12)-f.trial5.t(0))/12
trial6.auc.t = (f.trial6.t(12)-f.trial6.t(0))/12
trial5.auc.t.10 = (f.trial5.t(10)-f.trial5.t(0))/10
trial6.auc.t.10 = (f.trial6.t(10)-f.trial6.t(0))/10

#################### Fit model with time-varying intervention effect x*log(t) #################
fit.extend.s1 <- coxph(Y.s1 ~ Z + tt(Z) + cluster(id), data=data.s1.nc, tt=function(x, t,...) x*log(t), control = coxph.control(timefix = FALSE))
fit.extend.s2 <- coxph(Y.s2 ~ Z + tt(Z) + cluster(id), data=data.s2.nc, tt=function(x, t,...) x*log(t), control = coxph.control(timefix = FALSE))

# Obtain estimates
trial5.beta0.logt <- fit.extend.s1[[1]][[1]]
trial5.beta1.logt <- fit.extend.s1[[1]][[2]]
trial6.beta0.logt <- fit.extend.s2[[1]][[1]]
trial6.beta1.logt <- fit.extend.s2[[1]][[2]]

# Calculate PE from AUC
f.trial5.logt <- function(t) t-(exp(trial5.beta0.logt)*t^(trial5.beta1.logt+1))/(trial5.beta1.logt+1)
f.trial6.logt <- function(t) t-(exp(trial6.beta0.logt)*t^(trial6.beta1.logt+1))/(trial6.beta1.logt+1)
trial5.auc.logt = (f.trial5.logt(12)-f.trial5.logt(0))/12
trial6.auc.logt = (f.trial6.logt(12)-f.trial6.logt(0))/12
trial5.auc.logt.10 =  (f.trial5.logt(10)-f.trial5.logt(0))/10
trial6.auc.logt.10 =  (f.trial6.logt(10)-f.trial6.logt(0))/10

#################### Fit model with time-varying intervention effect x*sqrt(t) #################
fit.extend.s1 <- coxph(Y.s1 ~ Z + tt(Z) + cluster(id), data=data.s1.nc, tt=function(x, t,...) x*sqrt(t), control = coxph.control(timefix = FALSE))
fit.extend.s2 <- coxph(Y.s2 ~ Z + tt(Z) + cluster(id), data=data.s2.nc, tt=function(x, t,...) x*sqrt(t), control = coxph.control(timefix = FALSE))

# Obtain estimates
trial5.beta0.sqrt <- fit.extend.s1[[1]][[1]]
trial5.beta1.sqrt <- fit.extend.s1[[1]][[2]]
trial6.beta0.sqrt <- fit.extend.s2[[1]][[1]]
trial6.beta1.sqrt <- fit.extend.s2[[1]][[2]]

# Calculate PE from AUC
f.trial5.sqrt <- function(t) t-2*(trial5.beta1.sqrt*sqrt(t)-1)*exp(trial5.beta1.sqrt*sqrt(t)+trial5.beta0.sqrt)/(trial5.beta1.sqrt**2)
f.trial6.sqrt <- function(t) t-2*(trial6.beta1.sqrt*sqrt(t)-1)*exp(trial6.beta1.sqrt*sqrt(t)+trial6.beta0.sqrt)/(trial6.beta1.sqrt**2)
trial5.auc.sqrt = (f.trial5.sqrt(12)-f.trial5.sqrt(0))/12
trial6.auc.sqrt = (f.trial6.sqrt(12)-f.trial6.sqrt(0))/12
trial5.auc.sqrt.10 =  (f.trial5.sqrt(10)-f.trial5.sqrt(0))/10
trial6.auc.sqrt.10 =  (f.trial6.sqrt(10)-f.trial6.sqrt(0))/10



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

#################### Fit model with time-varying intervention effect x*t #################
fit.extend.s1.c <- coxph(Y.s1.c ~ Z + tt(Z) + cluster(id), data=data.s1.c, tt=function(x, t,...) x*t, control = coxph.control(timefix = FALSE))
fit.extend.s2.c <- coxph(Y.s2.c ~ Z + tt(Z) + cluster(id), data=data.s2.c, tt=function(x, t,...) x*t, control = coxph.control(timefix = FALSE))

# Obtain estimates
trial7.beta0.t <- fit.extend.s1.c[[1]][[1]]
trial7.beta1.t <- fit.extend.s1.c[[1]][[2]]
trial8.beta0.t <- fit.extend.s2.c[[1]][[1]]
trial8.beta1.t <- fit.extend.s2.c[[1]][[2]]

# Calculate PE from AUC
f.trial7.t <- function(t) t-exp(trial7.beta1.t*t+trial7.beta0.t)/trial7.beta1.t
f.trial8.t <- function(t) t-exp(trial8.beta1.t*t+trial8.beta0.t)/trial8.beta1.t
trial7.auc.t = (f.trial7.t(12)-f.trial7.t(0))/12
trial8.auc.t = (f.trial8.t(12)-f.trial8.t(0))/12
trial7.auc.t.10 = (f.trial7.t(10)-f.trial7.t(0))/10
trial8.auc.t.10 = (f.trial8.t(10)-f.trial8.t(0))/10

#################### Fit model with time-varying intervention effect x*log(t) #################
fit.extend.s1.c <- coxph(Y.s1.c ~ Z + tt(Z) + cluster(id), data=data.s1.c, tt=function(x, t,...) x*log(t), control = coxph.control(timefix = FALSE))
fit.extend.s2.c <- coxph(Y.s2.c ~ Z + tt(Z) + cluster(id), data=data.s2.c, tt=function(x, t,...) x*log(t), control = coxph.control(timefix = FALSE))
# Obtain estimates
trial7.beta0.logt <- fit.extend.s1.c[[1]][[1]]
trial7.beta1.logt <- fit.extend.s1.c[[1]][[2]]
trial8.beta0.logt <- fit.extend.s2.c[[1]][[1]]
trial8.beta1.logt <- fit.extend.s2.c[[1]][[2]]

# Calculate PE from AUC
f.trial7.logt <- function(t) t-(exp(trial7.beta0.logt)*t^(trial7.beta1.logt+1))/(trial7.beta1.logt+1)
f.trial8.logt <- function(t) t-(exp(trial8.beta0.logt)*t^(trial8.beta1.logt+1))/(trial8.beta1.logt+1)
trial7.auc.logt = (f.trial7.logt(12)-f.trial7.logt(0))/12
trial8.auc.logt = (f.trial8.logt(12)-f.trial8.logt(0))/12
trial7.auc.logt.10 = (f.trial7.logt(10)-f.trial7.logt(0))/10
trial8.auc.logt.10 = (f.trial8.logt(10)-f.trial8.logt(0))/10

#################### Fit model with time-varying intervention effect x*sqrt(t) #################
fit.extend.s1.c <- coxph(Y.s1.c ~ Z + tt(Z) + cluster(id), data=data.s1.c, tt=function(x, t,...) x*sqrt(t), control = coxph.control(timefix = FALSE))
fit.extend.s2.c <- coxph(Y.s2.c ~ Z + tt(Z) + cluster(id), data=data.s2.c, tt=function(x, t,...) x*sqrt(t), control = coxph.control(timefix = FALSE))
# Obtain estimates
trial7.beta0.sqrt <- fit.extend.s1.c[[1]][[1]]
trial7.beta1.sqrt <- fit.extend.s1.c[[1]][[2]]
trial8.beta0.sqrt <- fit.extend.s2.c[[1]][[1]]
trial8.beta1.sqrt <- fit.extend.s2.c[[1]][[2]]

# Calculate PE from AUC
f.trial7.sqrt <- function(t) t-2*(trial7.beta1.sqrt*sqrt(t)-1)*exp(trial7.beta1.sqrt*sqrt(t)+trial7.beta0.sqrt)/(trial7.beta1.sqrt**2)
f.trial8.sqrt <- function(t) t-2*(trial8.beta1.sqrt*sqrt(t)-1)*exp(trial8.beta1.sqrt*sqrt(t)+trial8.beta0.sqrt)/(trial8.beta1.sqrt**2)
trial7.auc.sqrt = (f.trial7.sqrt(12)-f.trial7.sqrt(0))/12
trial8.auc.sqrt = (f.trial8.sqrt(12)-f.trial8.sqrt(0))/12
trial7.auc.sqrt.10 = (f.trial7.sqrt(10)-f.trial7.sqrt(0))/10
trial8.auc.sqrt.10 = (f.trial8.sqrt(10)-f.trial8.sqrt(0))/10




####### Summary table #######
table1 <-
  tribble(
    ~trial,   ~beta,        ~PE.ph,    ~beta0.t,       ~beta1.t,       ~PE.auc.t,    ~PE.auc.10.t,       
    "trial1", trial1.beta,  trial1.PE, trial1.beta0.t, trial1.beta1.t, trial1.auc.t, trial1.auc.t.10,
    "trial2", trial2.beta,  trial2.PE, trial2.beta0.t, trial2.beta1.t, trial2.auc.t, trial2.auc.t.10,
    "trial3", trial3.beta,  trial3.PE, trial3.beta0.t, trial3.beta1.t, trial3.auc.t, trial3.auc.t.10,
    "trial4", trial4.beta,  trial4.PE, trial4.beta0.t, trial4.beta1.t, trial4.auc.t, trial4.auc.t.10,
    "trial5", trial5.beta,  trial5.PE, trial5.beta0.t, trial5.beta1.t, trial5.auc.t, trial5.auc.t.10,
    "trial6", trial6.beta,  trial6.PE, trial6.beta0.t, trial6.beta1.t, trial6.auc.t, trial6.auc.t.10,
    "trial7", trial7.beta,  trial7.PE, trial7.beta0.t, trial7.beta1.t, trial7.auc.t, trial7.auc.t.10,
    "trial8", trial8.beta,  trial8.PE, trial8.beta0.t, trial8.beta1.t, trial8.auc.t, trial8.auc.t.10
  )

table2 <-
  tribble(
    ~beta0.logt,           ~beta1.logt,         ~PE.auc.logt,       ~PE.auc.10.logt,
    trial1.beta0.logt,    trial1.beta1.logt,    trial1.auc.logt,    trial1.auc.logt.10,
    trial2.beta0.logt,    trial2.beta1.logt,    trial2.auc.logt,    trial2.auc.logt.10,
    trial3.beta0.logt,    trial3.beta1.logt,    trial3.auc.logt,    trial3.auc.logt.10,
    trial4.beta0.logt,    trial4.beta1.logt,    trial4.auc.logt,    trial4.auc.logt.10,
    trial5.beta0.logt,    trial5.beta1.logt,    trial5.auc.logt,    trial5.auc.logt.10,
    trial6.beta0.logt,    trial6.beta1.logt,    trial6.auc.logt,    trial6.auc.logt.10,
    trial7.beta0.logt,    trial7.beta1.logt,    trial7.auc.logt,    trial7.auc.logt.10,
    trial8.beta0.logt,    trial8.beta1.logt,    trial8.auc.logt,    trial8.auc.logt.10
  )

table3 <-
  tribble(
    ~beta0.sqrt,           ~beta1.sqrt,         ~PE.auc.sqrt,       ~PE.auc.10.sqrt,
    trial1.beta0.sqrt,    trial1.beta1.sqrt,    trial1.auc.sqrt,    trial1.auc.sqrt.10,
    trial2.beta0.sqrt,    trial2.beta1.sqrt,    trial2.auc.sqrt,    trial2.auc.sqrt.10,
    trial3.beta0.sqrt,    trial3.beta1.sqrt,    trial3.auc.sqrt,    trial3.auc.sqrt.10,
    trial4.beta0.sqrt,    trial4.beta1.sqrt,    trial4.auc.sqrt,    trial4.auc.sqrt.10,
    trial5.beta0.sqrt,    trial5.beta1.sqrt,    trial5.auc.sqrt,    trial5.auc.sqrt.10,
    trial6.beta0.sqrt,    trial6.beta1.sqrt,    trial6.auc.sqrt,    trial6.auc.sqrt.10,
    trial7.beta0.sqrt,    trial7.beta1.sqrt,    trial7.auc.sqrt,    trial7.auc.sqrt.10,
    trial8.beta0.sqrt,    trial8.beta1.sqrt,    trial8.auc.sqrt,    trial8.auc.sqrt.10
  )


summary <- cbind(table1, table2, table3)

################################################# Calculate number of cases averted ####################################################


ptm1 = proc.time() - ptm0
jnk = as.numeric(ptm1[3])
cat('\n', 'It took ', jnk, "seconds to do iteration", q)

return(summary)
}

NUM_REPS <- 500 # replications

plan(multisession, workers = 25)

List_t <- future_map(
  1:NUM_REPS,
  one_simulation,
  .progress = TRUE,
  .options = furrr_options(seed = TRUE)
)

save(List_t, file = "./500 sensitivity t.RData")





