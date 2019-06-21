
# topic -------------------------------------------------------------------

# R0, CFR, projection

# libraries ---------------------------------------------------------------

library(readxl)
library(outbreaks)
library(incidence)
library(epicontacts)
library(distcrete)
library(epitrix)
library(EpiEstim)
library(projections)
library(ggplot2)
library(magrittr)
library(binom)
library(ape)
library(outbreaker2)
library(tidyverse)
#library(here)


# _ PART 1 _ ---------------------------------------------------------------


# importa -----------------------------------------------------------------

linelist <- read_excel("data-raw/linelist_20140701.xlsx", 
                       na = c("", "NA"))
contacts <- read_excel("data-raw/contacts_20140701.xlsx", 
                       na = c("", "NA"))

# explorar ----------------------------------------------------------------

linelist
#more data to recolect
#age?
linelist <- linelist %>% 
  #select(case_id,date_of_infection:date_of_outcome) %>% 
  gather(key,value,#-case_id,
         date_of_infection:date_of_outcome
         ) %>% 
  mutate(value=as.Date(value,format = "%Y-%m-%d")) %>% #change to lubridate
  spread(key,value)
linelist
linelist %>% glimpse()
naniar::miss_var_summary(linelist)

contacts
contacts %>% glimpse()
naniar::miss_var_summary(contacts)


# incubation period --------------------------------------------------------------------

#what is happening here?
linelist %>% 
  mutate(mistake=date_of_onset-date_of_infection) %>% 
  #values 0 or less
  filter(mistake>0)

#check for inconsistencies
linelist_mist <- linelist %>% 
  mutate(mistake=date_of_onset-date_of_infection) %>% 
  #values 0 or less
  filter(mistake<=0) %>% 
  select(case_id) %>% pull()

linelist_clean <- linelist %>% 
  filter(!(case_id %in% linelist_mist))


# case fatality ratio -----------------------------------------------------

#cfr = 60/(60+43) #known outcomes -> ok
#cfr = 60/(60+43+63) #unknown outcomes -> wrong!
table(linelist_clean$outcome, useNA = "ifany")

linelist_clean_crf <- linelist_clean %>% 
  count(outcome) %>% 
  spread(outcome,n) %>% 
  rename_all(.funs = list(make.names)) %>% 
  rename_all(.funs = list(str_to_lower)) %>% 
  mutate(n_known_outcome=death+recover,
         n_all=death+recover+x.na.,
         crf_ok=death/(n_known_outcome),
         crf_wrong=death/(n_all))

binom.confint(linelist_clean_crf$death,linelist_clean_crf$n_known_outcome,methods = "exact")
#en la poblacion
#en promedio
#the case fatality ratio is 0.58 with a 95% CI 
#from 0.48 to 0.68


# incidence ---------------------------------------------------------------

linelist_clean %>% glimpse()
#incidence of observed: onset!

#daily
i_daily <- incidence(linelist_clean$date_of_onset)
i_daily
plot(i_daily)

#weekly
i_weekly <- incidence(linelist_clean$date_of_onset,
                      interval = 7,
                      last_date = max(linelist_clean$date_of_hospitalisation,na.rm = T))
i_weekly
plot(i_weekly,border = "black")


# save --------------------------------------------------------------------

write_rds(linelist_clean,"data/linelist_clean.rds")
write_rds(i_daily,"data/i_daily.rds")
write_rds(i_weekly,"data/i_weekly.rds")
write_rds(contacts,"data/contacts.rds")


# _ PART 2 _ ---------------------------------------------------------------


# growth rate w/ log-linear model -----------------------------------------

linelist_clean

class(i_weekly)
str(i_weekly)

#try to add a transform to undo the log transformation!
i_weekly %>% 
  as_tibble() %>% 
  #filter(counts>0) %>% 
  ggplot(aes(dates,log(counts))) +
  geom_point() +
  geom_smooth(method = "lm") + 
  xlab("date") + ylab("log weekly incidence")


# fit log linear model ----------------------------------------------------

f <- incidence::fit(i_weekly)
f
f %>% str()
f[[1]] %>% class()
f[[1]] %>% broom::tidy()
f[[1]] %>% broom::glance()

plot(i_weekly,fit = f)


# threshold ---------------------------------------------------------------

linelist_clean %>% glimpse()
linelist_clean %>% 
  mutate(diff_hosp_onset= date_of_hospitalisation - date_of_onset,
         diff_hosp_onset= as.numeric(diff_hosp_onset)) %>% 
  skimr::skim(diff_hosp_onset)

#BIAS!!!!
#we identify 22 days of maximum hospitalization
#correct by this time to avoid a substimation of the weekly incidence
#equivalent to ~3 weeks
#care must be taken to only fit to the point that there is epidemic growth
#USUALLY
#in analysis you adjust data to a gamma distribuiton
#an restrict to the 95% CI under that distribution

n_weeks_to_discard <- 3

min_date <- min(i_daily$dates)
max_date <- max(i_daily$dates) - n_weeks_to_discard * 7
# weekly truncated incidence
i_weekly_trunc <- subset(i_weekly, 
                         from = min_date, 
                         to = max_date) # discard last few weeks of data
# daily truncated incidence (not used for the linear regression but may be used later)
i_daily_trunc <- subset(i_daily, 
                        from = min_date, 
                        to = max_date) # remove last two weeks of data

# re-fit log linear model ----------------------------------------------------

f2 <- incidence::fit(i_weekly_trunc)
f2
#f %>% str()
f2[[1]] %>% class()
f2[[1]] %>% broom::tidy()
f2[[1]] %>% broom::glance()

plot(i_weekly,fit = f2)


# summarize both regressions ----------------------------------------------

#compare fitting
f[[1]] %>% broom::glance()
f2[[1]] %>% broom::glance()

#explore parameters
f2[[2]]$r
f2[[2]]$r.conf
f2[[2]]$doubling
f2[[2]]$doubling.conf

#doit it manually
log(2)/f2[[2]]$r
log(2)/rev(f2[[2]]$r.conf)



# contact tracing ----------------------------------------------------------------

make_epicontacts(linelist = linelist,
                 contacts = contacts,from = infector,to = case_id)



