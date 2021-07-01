if(!require("remotes")) install.packages("remotes")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("outbreaks")) install.packages("outbreaks")
if(!require("incidence")) install.packages("incidence")
if(!require("distcrete")) install.packages("distcrete")
if(!require("epitrix")) install.packages("epitrix")
if(!require("deSolve")) install.packages("deSolve")
if(!require("binom")) install.packages("binom")
if(!require("outbreaker2")) install.packages("outbreaker2")
if(!require("EpiEstim")) remotes::install_github("mrc-ide/EpiEstim")
if(!require("epicontacts")) remotes::install_github("reconhub/epicontacts")
if(!require("projections")) remotes::install_github("reconhub/projections")
if(!require("rio")) install.packages("rio")
if(!require("knitr")) install.packages("knitr")
if(!require("shiny")) install.packages("shiny")
if(!require("flexdashboard")) install.packages("flexdashboard")
if(!require("remotes")) install.packages("remotes")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("sf")) install.packages("sf")
if(!require("mapview")) install.packages("mapview")
if(!require("GADMTools")) install.packages("GADMTools")
if(!require("ggspatial")) remotes::install_github("paleolimbot/ggspatial")
if(!require("leaflet")) install.packages("leaflet")
if(!require("leaflet.extras2")) install.packages("leaflet.extras2")
if(!require("spdep")) install.packages("spdep")
if(!require("spatstat")) install.packages("spatstat")
if(!require("raster")) install.packages("raster")
if(!require("smacpod")) install.packages("smacpod")
if(!require("ggspatial")) install.packages("ggspatial")

p_list <- c("remotes",
            "tidyverse",
            "outbreaks",
            "incidence",
            "distcrete",
            "epitrix",
            "deSolve",
            "binom",
            "outbreaker2",
            "EpiEstim",
            "epicontacts",
            "projections",
            "rio",
            "knitr",
            "shiny",
            "flexdashboard",
            "sf",
            "mapview",
            "GADMTools",
            "ggspatial",
            "leaflet",
            "leaflet.extras2",
            "spdep",
            "spatstat",
            "raster",
            "smacpod",
            "ggspatial")

for (i in 1:length(p_list)) {
  # i <- 1
  library(p_list[i],character.only = T)
}
#' si aparece algún mensaje como:
#' 
#' Error in library(p_list[i], character.only = T) : 
#' there is no package called ‘outbreaker’
#' 
#' avisar

library("remotes")
library("tidyverse")
library("outbreaks")
library("incidence")
library("distcrete")
library("epitrix")
library("deSolve")
library("binom")
library("outbreaker2")
library("EpiEstim")
library("epicontacts")
library("projections")
library("rio")
library("knitr")
library("shiny")
library("flexdashboard")
library("sf")
library("mapview")
library("GADMTools")
library("ggspatial")
library("leaflet")
library("leaflet.extras2")
library("spdep")
library("spatstat")
library("raster")
library("smacpod")
library("ggspatial")