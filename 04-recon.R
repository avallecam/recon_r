
# topic -------------------------------------------------------------------

#contact-disease models

# libraries ---------------------------------------------------------------

library(deSolve)
library(gridExtra)
library(ggplot2)
#library(reshape)

# set parameters ----------------------------------------------------------

T.lfx    <- 72              # Life expectancy
T.dur    <- 3                # Duration of infectious period
beta     <- 4                # Transmission rate per capita ***(range 4-18)
break_in <- 1/T.lfx*(0.1)                # Transition rate from latent to active disease
selfcure <- (1/T.dur)*(0.5)                # Rate of spontaneous cure
mu       <- 1/T.lfx         # Background mortality rate
mu_tb    <- (1/T.dur)*(0.5)                # TB mortality rate
fast     <- 0.1                # Fraction fast progressing to active TB
imm      <- 0.5                # Infectiousness decline (partial immunity)
relapse  <- 0.005                # Relapse rate

# equations ---------------------------------------------------------------

TB.Basic <- function (t, state, parameters) {
  with(as.list(c(state, parameters)),             
       {
         N      <- U + L + I + R            # Total population
         births <- I * mu_tb + N * mu       # Births (for stable population)
         lambda <- beta * I/N               # Force of Infection
         
         # Uninfected 
         dU <- births - U * (lambda + mu)                              
         
         # Latent 
         dL <- U * lambda * (1-fast) + R * (lambda * (1-fast) * imm) - L * (mu + break_in) 
         
         # Active TB
         dI <- U * lambda * fast + R * (lambda * fast * imm) +  L * break_in + R * relapse -
           I * (mu + mu_tb + selfcure)
         
         # Recovered
         dR <- I * selfcure - R * (lambda * imm + relapse + mu)      
         
         # Model outcomes
         dIncidence <- U * (lambda * fast) + R * (lambda * fast * imm) + L * break_in + R * relapse 
         
         # ::::::::: TRY  TO COMPLETE THIS EQUATIONS ***(tip: JUST DESINTEGER THE INCIDENCE)
         
         dIrecent   <- U * lambda * fast + R * (lambda*fast*imm) + R * relapse
           
         dIremote   <- L * break_in
           
         # ::::::::::::::::::::::::::::::::::::::::::::
           
         # wrap-up 
         dx <- c(dU, dL, dI, dR, dIncidence, dIrecent, dIremote)
         list(dx)
       }
  )
}


# function ----------------------------------------------------------------

get_intervention <- function(sfin, params_new, params_old, times_new,
                             t.interv, fx_scale, fx_basic, int_name, data_stub) {
  
  # Starting conditions
  xstart <- c(U = sfin$U,
              L = sfin$L,
              I = sfin$I,
              R = sfin$R,
              Incidence = sfin$Incidence,
              Irecent   = sfin$Irecent,
              Iremote   = sfin$Iremote)
  
  # Select type of function
  if (is.na(t.interv)) {
    fx <- fx_basic
  }  else {
    fx <- fx_scale
  }
  
  # Run the model
  out <- as.data.frame(ode(y = xstart, times = times_new,
                           func = fx, parms = params_new)) 
  # Model output
  N           <- out$U + out$L + out$I + out$R
  rate.inc    <- 1e5 * (diff(out$Incidence) / N[1:length(N) - 1])
  fr.remo     <- diff(out$Iremote) / diff(out$Incidence)
  time        <- out$time[1:length(out$time) - 1]
  dat         <- data.frame(Years = time + (2019 - 400), Incidence = rate.inc)
  dat$Sim     <- int_name
  
  # If it is a first run, nothing to append 
  if (is.na(data_stub)) {
    data <- dat
  }
  else # Append previous runs
  {
    data <- rbind(data_stub, dat)
  }
  
  remote <- fr.remo  # rename a variable with the fraction of remote incidence
  titl   <- int_name
  
  p <- ggplot(data = data, mapping = aes(x = Years, y = Incidence, col = Sim))
  p1 <- p +
    geom_line(size = 1.2) +
    ggtitle ("TB Incidence") +
    theme_bw() + ylab("Rate per 100, 000 pop") +
    ylim(0, max(data$Incidence))
  
  df <- data.frame(
    Source = c("Recent", "Remote"),
    value  = c(1 - tail(remote, 1), tail(remote, 1))
  )
  
  # Pie chart of remote vs recent incidence 
  mycols <- c("#0073C2FF", "#EFC000FF")
  pie <- ggplot(df, aes(x = "", y = value, fill = Source)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = mycols) +
    theme_void() +
    ggtitle(titl)
  
  output <- list("out" = out, "lines" = p1, "pie" = pie, "data" = data)
  
  # Return the results
  return(output)
}


# sim parameters ----------------------------------------------------------

t.intervention <- 400      # years of simulation
t.scale        <- 3        # Scaling up time of interventions
times <- seq(0, t.intervention, by = 1)          # time scale


# start values ------------------------------------------------------------

# Prepare to run
params <- c(break_in = break_in, selfcure = selfcure, mu = mu, mu_tb = mu_tb,
            beta = beta, fast = fast, imm = imm, relapse = relapse) # running parameters

# Initial Conditions
N  <- 1                  # Total population equal to unity 
I0 <- 1e-6               # TB seed at time 0

xstart <- data.frame(U = N-I0,
                     L = 0,
                     I = I0,  
                     R = 0,
                     Incidence = 0, 
                     Irecent = 0, 
                     Iremote = 0)                


# solve -------------------------------------------------------------------

# run the model
out  <- get_intervention(xstart, params, NA, times, NA, NA, TB.Basic,
                         "Initial", NA) 
# plot
out$lines

# part II -----------------------------------------------------------------

# :::::: FILL IN THE MISSING VALUES HERE 

Inc.country    <- 221 # TB incidence per 100,000 (including TB/HIV) 
country.name   <- "Bangladesh" # e.g., "Sierra Leone"
params["beta"] <- 6.1  # Transmission rate per capita per year ***(EVALUATE FROM WHERE!)
# ::::::
  
  
# run the model
out0 <- get_intervention(xstart, params, NA, times, NA, NA, TB.Basic,
                           "Initial", NA) 

# plot
dot <- data.frame(Data = country.name, Years = 2017, incidence = Inc.country)

p1 <- out0$lines +
  geom_point(dot, mapping = aes(x = Years, y = incidence, col = Data), 
             size = 6, shape = 18) 

# Arrange plots in a grid
grid.arrange(p1, out0$pie)


# scale up intervention ---------------------------------------------------

# Intervention scaling function
scale_up <- function(t, state, parameters, t.interv, parameters_old, fx) {
  
  scale <- min((t - t.interv[1])/(t.interv[2] - t.interv[1]), 1) 
  if (scale < 0) 
  {
    scale <- 0
  }
  
  pars_scaled <- parameters;
  
  pars_scaled <- parameters_old + scale * (parameters - parameters_old)
  
  return(fx(t, state, pars_scaled))
}

# Function handles

fx_basic <- TB.Basic
fx_scale <- function(t, state, parameters) scale_up(t, state, parameters, t.interv, params, fx_basic)

## Simulation 0
# Project at baseline
int_name    <- "Baseline"

# Initial conditions (starting in 2019)
sfin        <- tail(out0$out, 1)                             
params_base <- params
times_new   <- seq(t.intervention, t.intervention + 25, by = 1)
t.interv    <- c(times_new[2], times_new[2] + t.scale)

# Run model
data0 <- get_intervention(sfin, params_base, params_base, times_new, NA,
                          fx_scale, fx_basic, "Baseline", NA) 
# Plot
grid.arrange(data0$lines, data0$pie)


# TB treatment ------------------------------------------------------------

## Simulation 1
# An Intervention simulating introduction of treatment
int_name  <- "Treatment"

# Update parameter results data frame to append new results
params_1  <- params_base
data_stub <- data0$data

# :::: COMPLETE THE MISSING VALUES AND WRITE A TERM FOR RATE Tx

#AIM: modificar tasa de recuperacion

# Change parameters for intervention
T.cs  <- 1       # Time delay (yrs) between developing symptoms and seeking care
pDx   <- 0.95       # Probability of being diagnosed once care sought
pTx   <- 0.95       # probability of recieving correct Tx if diagnosed
T.rTx <- 0.5   # 6 months treatment duration
Tx    <- (1/(T.cs+T.rTx)*pDx*pTx)
#
  
# ::::
  
params_1["selfcure"] <- selfcure + Tx

data1 <- get_intervention(sfin, params_1, params_base, times_new, t.interv,
                          fx_scale, fx_basic, int_name, data_stub) 

p1 <- data1$lines +
  # EndTb limit
  geom_hline(yintercept = Inc.country*0.1, linetype = "dashed", color = "black", size = 1) +
  # Elimination limit
  geom_hline(yintercept = 0.1/1e5, linetype = "dashed", color = "red", size = 1)


grid.arrange(p1, data1$pie)


# increace demand of TB services ------------------------------------------

#reducir el tiempo en el que alguien accede a servicios de salud
#una reducción del 75%
#equivale a el 25% de cada parámetro


# transmission control ----------------------------------------------------

#cut-off the transmission!!!
#no beta!


# focus on the latent population ------------------------------------------
# LTBI treatment

# medida de prevención
# 1- efectividad
# efectividad = 0.9
# 0.1
# affects brake_in -> (1/duracion)*contacto*(1-effectividad)