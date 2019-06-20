p_list <- c("tidyverse",
            "here",
			      "devtools",
			      "incidence",
            "epitrix",
            "deSolve",
			      "binom",
			      #"projections"
			      "outbreaks",
            "outbreaker",
            "outbreaker2")
install.packages(p_list)
devtools::install_github("annecori/EpiEstim")
devtools::install_github("reconhub/epicontacts@ttree")
devtools::install_github("reconhub/projections")

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