#' test how to translate datasets and documentation
#' https://github.com/edgararuiz-zz/datalang

library(datalang)

my_spec <- system.file("specs/thisweek.yml", package = "datalang")

readLines(my_spec, encoding = "UTF-8")

diamantes <- translate_data(my_spec) 

head(diamantes)
#>       dia manana tarde
#> 1 viernes     76    88
#> 2  sábado     71    85
#> 3 domingo     70    83

library(outbreaks)

outbreaks::ebola_sim_clean

system.file("specs/ebola_sim_clean.yml",package = "outbreaks")

translate_data()