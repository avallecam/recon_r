
# topic -------------------------------------------------------------------

#learn R
#https://www.reconlearn.org/post/practical-intror.html

# atomic vectors ----------------------------------------------------------

vector_double <- c(1, 2, 3, 4)  
vector_logic <- c(TRUE, FALSE, FALSE, TRUE)
vector_character <- c("A", "B", "C", "D")
vector_integer <- c(1L, 2L, 3L, 4L)

#diferencia
class(vector_double)
typeof(vector_double)

#equivalencia
class(vector_logic)
typeof(vector_logic)
class(vector_character)
typeof(vector_character)
class(vector_integer)
typeof(vector_integer)

# matrix ------------------------------------------------------------------
# 2 dim vectors
# same type of vectors

#filled by column
matrix_of_doub <-  matrix(data = vector_double, 
                          nrow = 2, 
                          ncol = 2)
matrix_of_doub
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
dim(matrix_of_doub)
## [1] 2 2

#if more dimensions than vector,
#then it will replicate the vector
# that is coercion
matrix_of_log <-  matrix(data = vector_logic, 
                         nrow = 4, 
                         ncol = 3)
matrix_of_log

matrix_of_char <- matrix(data = vector_character, 
                         nrow = 4, 
                         ncol = 4)
matrix_of_char

matrix_of_int <-  matrix(data = vector_integer, 
                         nrow = 4, 
                         ncol = 5)
matrix_of_int

# array -------------------------------------------------------------------
# n dim vectors
# special type of matrix
# dim arguments
# 1- number of rows
# 2- number of columns
# 3- number of dimension

vector_example <-1:18
array_example <- array(data = vector_example, 
                       dim = c(2, 3, 3))

dim(array_example)
## [1] 2 3 3
array_example

# dataframe ---------------------------------------------------------------
# similar to matrix,
# but with diferent type of vectors
# heterogeneous structure

data_example <- data.frame(vector_character, 
                           vector_double, 
                           vector_logic, 
                           vector_integer)
data_example
str(data_example)

#to access: data[row,col]
data_example[1,2]
data_example[2,1]

# list --------------------------------------------------------------------
# the most complex structure
# mix of any type of object

list_example <- list(vector_character,
                     matrix_of_doub,
                     data_example)
list_example

#access ot any component
list_example[2]
list_example[[1]]

#note the class
class(list_example[2])
class(list_example[[2]])


# function ----------------------------------------------------------------

myfun01 <- function(weight,height) {
  bmi <- weight/(height^2)
  return(bmi)
}

myfun01(70,1.79)
myfun01(height = 1.79,weight = 70)

#components of a function
formals(myfun01)
body(myfun01)
environment(myfun01)

#add a defult argument
myfun02 <- function(weight,height,bmi_units="kg/m2") {
  bmi <- weight/(height^2)
  out <- paste(round(bmi,1), bmi_units)
  return(out)
}

myfun02(70,1.79)
myfun02(70*100,179,bmi_units = "g/cm2")

# packages ----------------------------------------------------------------

lapply(.libPaths(), dir)
browseVignettes(package = "incidence")


# environments ------------------------------------------------------------

#carefull!
#if objects where not called inside the function
#it will search for them on the general environment
myfun03 <- function() {
  z = x + y
  return(z)
}

x <- 1
y <- 3
myfun03()

#even if arguments are partially defined
myfun04 <- function(xx) {
  zz = xx + yy
  return(zz)
}

yy <- 3
myfun04(xx = 4)

#be carefull, 
#this is good to know
#but strongly not recomended


# probability distributions -----------------------------------------------

# work more on distributions
x_ <- seq(-1,1,by = 0.05)
y_ <- dnorm(x = x_,
      mean = 0,sd = 1)

plot(x_,y_)

# tidyverse ---------------------------------------------------------------

library(tidyverse)
library(readxl)
#library(here)


# _ import dataset --------------------------------------------------------

dat <- read_xlsx("data-raw/PHM-EVD-linelist-2017-10-27.xlsx")

# _ explore with tidyverse ------------------------------------------------

glimpse(dat)
dat %>% arrange(age)
dat %>% 
  rename(fecha_inicio_sintoma=onset)

#count number of subjects per group
dat %>% 
  group_by(sex) %>% 
  summarise(number = n())

dat %>% 
  select(starts_with("on"))

dat %>% 
  slice(7:10)

dat %>% 
  filter(sex=="female", age<=30)
dat %>% 
  filter(sex=="female" & age<=30)
dat %>% 
  filter(sex=="female" | age<=30)


# _ explore data from package ---------------------------------------------

measles_dat <- outbreaks::measles_hagelloch_1861 %>% 
  as_tibble()

measles_dat
measles_dat %>% glimpse()


# _ use tidyr -------------------------------------------------------------

#cambiar ejemplo:
# entrar al contexto de tidy data
# mix between wide and long data

malaria <- tibble(
  name = letters[1:10],
  age = round(rnorm(10, 30, 10), 0),
  gender = rep(c('f', 'm'), 5),
  infection = rep(c('falciparum', 'vivax', 'vivax', 'vivax', 'vivax'), 2)
  ) 

glimpse(malaria)

malaria %>% 
  spread(key = infection,value = gender)

measles_dat %>% 
  naniar::miss_var_summary()

measles_dat_group <- measles_dat %>% 
  filter(!is.na(gender)) %>% 
  group_by(date_of_rash,gender) %>% 
  summarise(cases=n()) %>% 
  ungroup()

measles_dat_group