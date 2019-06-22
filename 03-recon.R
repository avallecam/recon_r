
# topic -------------------------------------------------------------------

#vector-borne disease
#https://www.reconlearn.org/post/practical-vbd.html

#todo
#- shiny app of the change in the intervention parameter

# librerias ---------------------------------------------------------------

library(tidyverse)
library(deSolve)
library(ggplot2)
library(gridExtra)


# set parameters ----------------------------------------------------------

muv      <- 0.2           # mortality of mosquitos ***(adult not larval)
musp     <- 0.1         # spray effectiveness
Lv       <- 1/muv         # life span of mosquitos (in days)
Lh       <- 50*365        # life span of humans (in days) ***convinience
Iph      <- 6             # Infectious period in humans (in days)
IP       <- 7             # Infectious period in vectors (in days)                      ******entomologia ******REVISAR!!!!
EIP      <- 8.4           # Extrinsic incubation period in adult mosquitos              ******entomologia
muh      <- 1/Lh          # mortality of humans
gamma    <- 1/6           # recovery rate in humans ***(1/infectious period in humans)
delta    <- 1/EIP         # extrinsic incubation rate
b        <- 0.5           # Biting Rate ***(there is other b at the end)
betah    <- 0.7           # Probability of transmission from vector to host
betav    <- 0.7           # Probability of transmission from host to vector ***
d <- 0.3                # bednet efficiency
Nh       <- 2400000       # Number of humans (Population of Cali 2.4 million)
m        <- 1.5           # Vector to human ratio
Nv       <- Nh*m          # Number of vectors
R0       <- 2.69          # Reproductive number ***(of vectors)
b        <- sqrt((R0 ^2 * muv*(muv+delta) * (muh+gamma)) / 
                   (m * betah * betav * delta)) # biting rate ***(using the R0)

TIME     <- 100           # Number of years to run the simulation for 

# set function ------------------------------------------------------------

#arguments
#t=time
#x=variables
#p=local environment of parameters
arbovmodel <- function(t, x, params) {
  
  Sh <- x[1]    # Susceptible humans
  Ih <- x[2]    # Infected humans
  Rh <- x[3]    # Recovered humans
  Sv <- x[4]    # Susceptible vectors
  Ev <- x[5]    # Susceptible vectors
  Iv <- x[6]    # Infected vectors
  
  with(as.list(params), # local environment to evaluate derivatives
       {
         # Humans
         dSh   <-  muh*Nh - (betah * b/Nh) * Sh * Iv - muh * Sh   
         dIh   <-  (betah * b/Nh) * Sh * Iv  - (gamma + muh) * Ih
         dRh   <-  gamma * Ih  - muh * Rh
         
         # Vectors
         dSv  <-  muv * Nv - (betav * b/Nh) * Ih * Sv - muv * Sv 
         dEv  <-  (betav * b/Nh) * Ih * Sv - (muv + delta)* Ev
         dIv  <-  delta * Ev - muv * Iv
         
         dx   <- c(dSh, dIh, dRh, dSv, dEv, dIv)
         list(dx)
       }
  )
}


# (1) solve system ------------------------------------------------------------

# Time 
times  <- seq(1, 365 * TIME , by = 1)

# Specifying parameters
params <- c(
  muv      = muv,     
  muh      = muh,     
  gamma    = gamma,   
  delta    = delta,   
  b        = b,       
  betah    = betah,   
  betav    = betav,   
  Nh       = Nh,      
  Nv       = Nv
  
)

# Initial conditions of the system
xstart <- c(Sh = Nh,       # suceptible population: assuming that this is a naive population!!
            Ih = 0,        # infected: based on the same assumption
            Rh = 0,        # recovered: the same 
            Sv = Nv-1,     # suceptible mosquitos 
            Ev = 1,        # only one exposed to start outbrake
            Iv = 0)        # no infectious yet
#by reducing Suceptible population, you simulate vaccination

# Solving the equations
out <- as.data.frame(ode(y      = xstart,     # initial conditions  
                         times  = times,      # time units vector
                         fun    = arbovmodel, # derivative function  
                         parms  = params      # set of parameters   
                         )
                     )


# the results -------------------------------------------------------------

# Creating time options to display
out$years <- out$time/365
out$weeks <- out$time/7

# tidy dynamics plot ---------------------------------------------------------------

out_tidy <- 
  out %>% 
  as_tibble() %>% 
  gather(key,value,Sh:Iv) %>% #count(key)
  mutate(key=fct_relevel(key,"Sh","Ih","Rh","Sv","Ev")) %>% 
  mutate(specie= if_else(key %in% c("Sh","Ih","Rh"),"human","vector")) %>% 
  group_by(specie,time) %>% 
  mutate(prop=value/sum(value)) %>% 
  ungroup()

out_tidy %>% 
  filter(key %in% c("Ih","Ev","Iv")) %>% 
  group_by(key) %>% 
  mutate(max=max(prop)) %>% 
  filter(max==prop) %>% 
  #summarise(max=max(prop)) %>% 
  ungroup()

out_tidy %>% #count(specie)
  ggplot(aes(y=prop,x=weeks,color=key)) +
  geom_line(size = 1) +
  xlim(0,52) +
  theme_bw() +
  facet_grid(specie~.) +
  ggtitle('Population dynamics',
          subtitle = "Naive human population") +
  theme_bw() + ylab('Proportion')
ggsave("figure/0301-zika-popdyn-naivehost.png",width = 8,height = 5)


# eval - behavior human ----------------------------------------------------------

# Check the general behavior of the model for the whole 100 years

p1h <- ggplot(data = out, 
              aes(y = (Rh + Ih + Sh)/10000, 
                  x = years)) +
  geom_line(color = 'grey68', size = 1) +
  ggtitle('Total human population') +
  theme_bw() + ylab('number per 10,000')

p2h <- ggplot(data = out, 
              aes(y = Sh/10000, 
                  x = years)) +
  geom_line(color = 'royalblue', size = 1) +
  ggtitle('Susceptible human population') +
  theme_bw() + ylab('number per 10,000')

p3h <- ggplot(data = out, 
              aes(y = Ih/10000, 
                  x = years)) +
  geom_line(color = 'firebrick', size = 1) +
  ggtitle('Infected human population') +
  theme_bw() + ylab('number per 10,000')

p4h <- ggplot(data = out, 
              aes(y = Rh/10000,
                  x = years)) +
  geom_line(color = 'olivedrab', size = 1) +
  ggtitle('Recovered human population') +
  theme_bw() + ylab('number per 10,000')


grid.arrange(p1h, p2h, p3h, p4h, ncol = 2)

# eval - behaviour mosquito ------------------------------------------------------

# Check the general behavior of the model 

p1v <- ggplot(data = out, aes(y = (Sv + Ev + Iv), x = years)) +
  geom_line(color = 'grey68', size = 1) +
  ggtitle('Total mosquitio population') +
  theme_bw() + ylab('number')

p2v <- ggplot(data = out, aes(y = Sv, x = years)) +
  geom_line(color = 'royalblue', size = 1) +
  ggtitle('Susceptible mosquito population') +
  theme_bw() + ylab('number')

p3v <- ggplot(data = out, aes(y = Ev, x = years)) +
  geom_line(color = 'orchid', size = 1) +
  ggtitle('Exposed mosquito population') +
  theme_bw() + ylab('number')

p4v <- ggplot(data = out, aes(y = Iv, x = years)) +
  geom_line(color = 'firebrick', size = 1) +
  ggtitle('Infected mosquito population') +
  theme_bw() + ylab('number')

grid.arrange(p1v, p2v, p3v, p4v, ncol = 2)


# eval - look at the proportions -------------------------------------------------

p1 <- ggplot(data = out, aes(y = Sh/(Sh+Ih+Rh), x = years)) +
  geom_line(color = 'royalblue', size = 1) +
  ggtitle('Susceptible human population') +
  theme_bw() + ylab('proportion')

p2 <- ggplot(data = out, aes(y = Ih/(Sh+Ih+Rh), x = years)) +
  geom_line(color = 'firebrick', size = 1) +
  ggtitle('Infected human population') +
  theme_bw() + ylab('proportion')

p3 <- ggplot(data = out, aes(y = Rh/(Sh+Ih+Rh), x = years)) +
  geom_line(color = 'olivedrab', size = 1) +
  ggtitle('Recovered human population') +
  theme_bw() + ylab('proportion')

grid.arrange(p1, p2, p3, ncol = 2)
grid.arrange(p1+xlim(0,1), 
             p2+xlim(0,1), 
             p3+xlim(0,1), ncol = 2)

out %>% 
  as_tibble() %>% 
  ggplot(aes(x=years)) +
  geom_line(aes(y=Sh/(Sh+Ih+Rh)),color = 'royalblue', size = 1) +
  geom_line(aes(y=Ih/(Sh+Ih+Rh)),color = 'firebrick', size = 1) +
  geom_line(aes(y=Rh/(Sh+Ih+Rh)),color = 'olivedrab', size = 1) +
  xlim(0,1) +
  theme_bw() +
  ggtitle('Human population dynamics') +
  theme_bw() + ylab('Proportion')


# (2) solve system ----------------------------------------------------

# Initial conditions of the system
xstart <- c(Sh = Nh-1,       # suceptible population: assuming that this is a naive population!!
            Ih = 1,        # infected: based on the same assumption
            Rh = 0,        # recovered: the same 
            Sv = Nv,     # suceptible mosquitos 
            Ev = 0,        # only one exposed to start outbrake
            Iv = 0)        # no infectious yet
#by reducing Suceptible population, you simulate vaccination

# Solving the equations
out <- as.data.frame(ode(y      = xstart,     # initial conditions  
                         times  = times,      # time units vector
                         fun    = arbovmodel, # derivative function  
                         parms  = params      # set of parameters   
)
)


# the results -------------------------------------------------------------

# Creating time options to display
out$years <- out$time/365
out$weeks <- out$time/7

# tidy dynamics plot ---------------------------------------------------------------

out_tidy <- 
  out %>% 
  as_tibble() %>% 
  gather(key,value,Sh:Iv) %>% #count(key)
  mutate(key=fct_relevel(key,"Sh","Ih","Rh","Sv","Ev")) %>% 
  mutate(specie= if_else(key %in% c("Sh","Ih","Rh"),"human","vector")) %>% 
  group_by(specie,time) %>% 
  mutate(prop=value/sum(value)) %>% 
  ungroup()

out_tidy %>% 
  filter(key %in% c("Ih","Ev","Iv")) %>% 
  group_by(key) %>% 
  mutate(max=max(prop)) %>% 
  filter(max==prop) %>% 
  #summarise(max=max(prop)) %>% 
  ungroup()

out_tidy %>% #count(specie)
  ggplot(aes(y=prop,x=weeks,color=key)) +
  geom_line(size = 1) +
  xlim(0,52) +
  theme_bw() +
  facet_grid(specie~.) +
  ggtitle('Population dynamics',
          subtitle = "Naive vector population") +
  theme_bw() + ylab('Proportion')
ggsave("figure/0301-zika-popdyn-naivevector.png",width = 8,height = 5)



# INTERVENTIONS -----------------------------------------------------------

#mosquito spray
#incrementar tasa mortalidad
#agregar un valor a la tasa de mortalidad
#musp <- 

#vaccination
#una solución es agregar una casilla más de vacunados
#y plantear un nuevo sistema de ecuaciones


# IRS ---------------------------------------------------------------------
# indoor residual spray

intervmod1 <- function(t, x, params) {
  
  Sh <- x[1]    # Susceptible humans
  Ih <- x[2]    # Infected humans
  Rh <- x[3]    # Recovered humans
  Sv <- x[4]    # Susceptible vectors
  Ev <- x[5]    # Susceptible vectors
  Iv <- x[6]    # Infected vectors
  
  with(as.list(params), # local environment to evaluate derivatives
       {
         # Humans
         dSh   <-  muh*Nh - (betah * b/Nh) * Sh * Iv - muh * Sh   
         dIh   <-  (betah * b/Nh) * Sh * Iv  - (gamma + muh) * Ih
         dRh   <-  gamma * Ih  - muh * Rh
         
         # Vectors
         dSv  <-  muv * Nv - (betav * b/Nh) * Ih * Sv - (muv+musp) * Sv 
         dEv  <-  (betav * b/Nh) * Ih * Sv - ((muv+musp) + delta)* Ev
         dIv  <-  delta * Ev - (muv+musp) * Iv
         
         dx   <- c(dSh, dIh, dRh, dSv, dEv, dIv)
         list(dx)
       }
  )
}

# (3) solve system ------------------------------------------------------------

# Time 
times  <- seq(1, 365 * TIME , by = 1)

# Specifying parameters
params <- c(
  muv      = muv,
  musp     = musp,
  muh      = muh,     
  gamma    = gamma,   
  delta    = delta,   
  b        = b,       
  betah    = betah,   
  betav    = betav,   
  Nh       = Nh,      
  Nv       = Nv
  
)

# Initial conditions of the system
#naive population
xstart <- c(Sh = Nh,       # suceptible population: assuming that this is a naive population!!
            Ih = 0,        # infected: based on the same assumption
            Rh = 0,        # recovered: the same 
            Sv = Nv-1,     # suceptible mosquitos 
            Ev = 1,        # only one exposed to start outbrake
            Iv = 0)        # no infectious yet
#by reducing Suceptible population, you simulate vaccination

# Solving the equations
out <- as.data.frame(ode(y      = xstart,     # initial conditions  
                         times  = times,      # time units vector
                         fun    = intervmod1, # derivative function  
                         parms  = params      # set of parameters   
)
)


# the results -------------------------------------------------------------

# Creating time options to display
out$years <- out$time/365
out$weeks <- out$time/7

# tidy dynamics plot ---------------------------------------------------------------

out_tidy <- 
  out %>% 
  as_tibble() %>% 
  gather(key,value,Sh:Iv) %>% #count(key)
  mutate(key=fct_relevel(key,"Sh","Ih","Rh","Sv","Ev")) %>% 
  mutate(specie= if_else(key %in% c("Sh","Ih","Rh"),"human","vector")) %>% 
  group_by(specie,time) %>% 
  mutate(prop=value/sum(value)) %>% 
  ungroup()

out_tidy %>% 
  filter(key %in% c("Ih","Ev","Iv")) %>% 
  group_by(key) %>% 
  mutate(max=max(prop)) %>% 
  filter(max==prop) %>% 
  #summarise(max=max(prop)) %>% 
  ungroup()

out_tidy %>% #count(specie)
  ggplot(aes(y=prop,x=weeks,color=key)) +
  geom_line(size = 1) +
  xlim(0,52) +
  theme_bw() +
  facet_grid(specie~.) +
  ggtitle('Population dynamics',
          subtitle = "Intervention: IRS - indoor residual spray") +
  theme_bw() + ylab('Proportion')
ggsave("figure/0302-zika-interv-irspray.png",width = 8,height = 5)


# bednet use ------------------------------------------------------------------


# vaccination -------------------------------------------------------------

# LSM ---------------------------------------------------------------------
# larval source management

