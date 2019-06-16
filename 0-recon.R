p_list <- c("incidence",
            "epitrix",
            "deSolve",
            "outbreaker",
            "outbreaker2",
            "projections")
#install.packages(p_list)
#devtools::install_github("annecori/EpiEstim")

for (i in 1:length(p_list)) {
  library(p_list[i],character.only = T)
}

devtools::session_info(include_base = F)

#library(incidence)
#library(epitrix)
#library(deSolve)
#library(outbreaker)
#library(outbreaker2)
#library(projections)
#library(EpiEstim)
