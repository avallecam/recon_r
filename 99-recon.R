#' notas - introR
#' - ( ) suggest adding pipe demostration
#' - ( ) one exercise of tidyr verbs
#' 
#' 
#' notas - real-time 1
#' 
#' notas - vbd
#' - ( ) ojo con la identación dentro de funciones
#' - ( ) ¿qué valores iniciales usar? arbitrario?
#' - ( ) modelos corre en días, inicialmente, conversión a años y semanas dividiendo por 365 o 7
#' - ( ) reemplazar PAR por el parámetro correcto de la ecuación
#' - ( ) reemplazar VALUE por valores iniciales correctos para el contexto
#' - ( ) población susceptible es igual que población total, porque nunca antes ha estado expuesta
#' - ( ) recuperados 0
#' - ( ) mosquietos suceptibles todos menos uno infectado
#' - ( ) solo 1 mosquito infectado 
#' 
#' 
#' propuestas 
#' 
#' 
#' alternativa para una base más wide
library(tidyverse)

dengue_wide <- tibble(
  district = rep(letters[1:3], each = 2),
  gender = rep(c('f', 'm'), 3),
  denv1 = round(rnorm(6, 30, 10), 0),
  denv2 = round(rnorm(6, 30, 10), 0),
  denv3 = round(rnorm(6, 30, 10), 0),
  denv4 = round(rnorm(6, 30, 10), 0)
) 
dengue_wide

dengue_long <- dengue_wide %>% 
  pivot_longer(denv1:denv4, names_to = "serotype", values_to = "cases")
dengue_long

dengue_long %>% 
  pivot_wider(names_from = serotype, values_from = cases)
#' 
# malaria_wide <- tibble(
#   district = rep(letters[1:5],each = 2),
#   gender = rep(c('f', 'm'), 5),
#   falciparum = round(rnorm(10, 30, 10), 0),
#   vivax = round(rnorm(10, 30, 10), 0)
# ) 
# malaria_wide
# 
# malaria_long <- malaria_wide %>% 
#   pivot_longer(falciparum:vivax, names_to = "infection", values_to = "cases")
# malaria_long
# 
# malaria_long %>% 
#   pivot_wider(names_from = infection, values_from = cases)
# 
# malaria_wide %>% gather(key = "infection", value = "cases",falciparum:vivax)
# malaria_long %>% spread(key = infection, value = cases)

#' 
#' alternativa didactica para pipe
#' ref: https://evalsp21.classes.andrewheiss.com/projects/01_lab/slides/01_lab.html#116
# me %>%
#   wake_up(time = "8:00") %>%
#   get_out_of_bed(side = "correct") %>%
#   get_dressed(pants = TRUE, shirt = TRUE) %>%
#   leave_house(car = TRUE, bike = FALSE)
#' 
#' secuencia de acciones:
#' 
#' yo 
#' me despierto a las 8 horas
#' salgo de cama por el lado correcto
#' visto pantalones y polo
#' salgo de casa con carro sin bicicleta
#' 
#' ojo: no correr en R, solo es demostrativo
yo %>%
  despertar(tiempo = "8:00") %>%
  salir_de_cama(lado = "correcto") %>%
  vestir(pantalones = TRUE, polo = TRUE) %>%
  salir_de_casa(carro = TRUE, bicicleta = FALSE)

#' 
#' 

# # generar un vector con valores aleatorios para la distribucion Beta
# 
# # https://ben18785.shinyapps.io/distribution-zoo/
# 
library(tidyverse)
# 
# binomial distribution -----------------------------
# 
# generate x axis values
size <- 100
pdf <- tibble(x = 1:size,
              q = 1:size) %>%
  mutate(densidad = dbinom(x = x, size = size, prob = 0.1),
         probabilidad = pbinom(q = q, size = size, prob = 0.1))
# probability distribution function
pdf %>%
  ggplot(aes(x = x,y = densidad)) +
  geom_point() + 
  labs(y = "PDF")
# cumulative distribution function
pdf %>%
  ggplot(aes(x = x,y = probabilidad)) +
  geom_step() + 
  labs(y = "CDF")
# 
# beta distribution -----------------------------
# 
# generate x axis values
pdf <- tibble(x = seq(0,1,0.1)) %>%
  mutate(densidad = dbeta(x, shape1 = 1, shape2 = 3.8),
         probabilidad = pbeta(x, shape1 = 1, shape2 = 3.8))
# probability distribution function or density
pdf %>%
  ggplot(aes(x = x,y = densidad)) +
  geom_point() + 
  labs(y = "PDF or density")
# cumulative distribution function or CDF or probability
pdf %>%
  ggplot(aes(x = x,y = probabilidad)) +
  geom_step() + 
  labs(y = "CDF or probability")
# 
# beta <- rbeta(n=5e3, shape1=1, shape2=3.8)
# beta %>%
#   enframe() %>%
#   ggplot(aes(x = value,y = ..density..)) +
#   geom_histogram(binwidth = 0.01)




