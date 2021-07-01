#' 
#' [distribuciones estadísticas]
#' 
#' fuente: https://www.apsnet.org/edcenter/disimpactmngmnt/topc/EcologyAndEpidemiologyInR/IntroductionToR/Pages/StatisticalDistributions.aspx
#' 
#' adicional no usado: https://statisticsglobe.com/beta-distribution-in-r-dbeta-pbeta-qbeta-rbeta
#' https://distribution-explorer.github.io/continuous/normal.html
#' https://distribution-explorer.github.io/continuous/normal.html
#' https://distribution-explorer.github.io/continuous/beta.html
#' https://distribution-explorer.github.io/continuous/gamma.html#
#' 
#' rnorm() ---------------------------------------------
#' 
#' podemos ilustrar una distribución normal 
#' con media 0 y desviación estandar 1
#' dibujando un histograma 
#' con 1000 observaciones aleatorias
#' a partir de esta distribución
hist(rnorm(n = 1000))
#' 
#' cada vez que corras esta última línea, generaras
#' un nuevo conjunto de datos aleatorios. ¡inténtalo!
#' 
#' dnorm() ---------------------------------------------
#' 
#' la densidad de la distribución aleatoria
#' a cualquier punto dado
#' indica qué tan probable son dichos valores en dicho rango
dnorm(x = 0)
dnorm(x = -1)
dnorm(x = 1)
#' 
#' tal y como vemos en la gráfica anterior
#' tanto 1 como -1 tienen una densidad muy similar
#' 
#' ¿cuál es el valor de la densidad para un
#' valor poco probable en el rango, por ejemplo 4?
dnorm(x = 4)
#' 
#' pnorm() ---------------------------------------------
#' 
#' los valores de densidad no proveen los valores de 
#' la probabildiad, pero dan una idea de su 
#' probabilidad relativa.
#' 
#' el percentil proporciona información sobre la 
#' probabilidad de que los valores de una 
#' distribución normal caigan por debajo de un 
#' valor determinado.
#' 
#' ¿cuál es la probabilidad de que los valores sean 
#' inferiores a -1?
pnorm(q = -1)
#' 
#' ¿cuál es la probabilidad de que los valores sean 
#' inferiores a 0?
#' o, ¿qué proporción de valores cae por debajo de 0?
pnorm(q = 0)
#' 
#' ¿qué proporción de valores cae por debajo de 1?
pnorm(q = 1)
#' 
#' en análisis estadísticos, una distribución normal
#' puede ser usada para representar potenciales 
#' observaciones debajo de una hipótesis nula. 
#' si una observación cae lejos en las colas 
#' de esta distribución, sugiere que
#' podemos rechazar la hipótesis nula.
#' 
#' para una distribución normal, 
#' ¿cuál es la probabilidad de que una observación
#' caiga debajo de -1.96?
pnorm(q = -1.96)
#' 
#' ¿cuál es la probabilidad de que una observación
#' caiga por arriba de 1.96?
1 - pnorm(q = 1.96)
#' 
#' note que estos valores
#' son frecuentemente usados como punto de corte,
#' de forma que la probabilidad de una observación
#' caiga debajo de -1.96 o arriba de 1.96, 
#' sumando aproximadamente 0.05.
pnorm(q = -1.96) + (1 - pnorm(q = 1.96))
#' 
#' 0.05 representa una probabilidad aceptablemente baja
#' de que un valor extremo pueda ser observado en una muestra
#' si la hipótesis nula es correcta.
#' 
#' qnorm() ---------------------------------------------
#' 
#' la función cuantil permite plantear
#' preguntas diferentes pero relacionadadas,
#' por ejemplo:
#' ¿por debajo de qué valor cae la mitad de la distribución?
qnorm(p = 0.5)
#' 
#' ¿por debajo de qué valor cae el 2.5% de la distribución?
qnorm(p = 0.025)
#' 
#' ¿por arriba de qué valor cae el 2.5% de la distribución?
qnorm(p = (1-0.025))
#' 
#' -----------------------------------------------------
#' 
#' intenta realizar lo mismo con el resto de distribuciones
#' usando las funciones de la tabla 
#' (https://deploy-preview-104--reconlearn.netlify.app/post/practical-intror-spanish.html#trabajar-con-distribuciones-de-probabilidad)
#' 
#' casi todas las distribuciones estadísticas 
#' emplean de parámetros diferentes, por lo que 
#' sus funciones también requieren de diferentes argumentos. 
#' 
#' por ejemplo, 
#' la distribución binomial tiene dos parámetros, 
#' la probabilidad de éxito, `prob`, y 
#' el número de ensayos, `size`. 
#' 
#' la distribución binomial se suele ilustrar con una 
#' serie de lanzamientos de una moneda, en la que el 
#' éxito puede definirse en términos de que el lanzamiento 
#' de la moneda dé "cara", y 
#' en la que prob=0.5 si la moneda es justa. 
#' 
#' La distribución binomial sólo puede tomar valores 
#' enteros no negativos, a diferencia de la distribución normal, 
#' que incluye cualquier número entre -∞ y ∞.
#' 
hist(rbinom(n=100000, size=10, prob=.5))
#' 
#' con el aplicativo "el zoológico de distribuciones"
#' puedes explorar de forma interactiva
#' por varias distribuciones y sus respectivos parámetros
#' https://ben18785.shinyapps.io/distribution-zoo/
#' 
#' -----------------------------------------------------
#' 
#' [uso de las distribuciones estadísticas en epidemiología]
#' 
#' ejemplo 1:
#' 
#' podemos usar la distribución binomial para describir
#' el número de personas infectadas en un área muestreada
#' por ejemplo, si muestreamos un área que contiene 
#' `size = 40` personas y cada persona tiene un probabilidad
#' `prob = 0.1` de estar infectada, 
#' entonces la distribución del número de personas infectadas 
#' dentro del área es
hist(rbinom(n = 100000, size = 40, prob = 0.1))
# hist(rbinom(n = 100000, size = 40, prob = 0.1),xlim = c(0,40))
#' 
#' también podemos expresarlo en términos de proporción 
#' de personas infectadas dividiendo por su tamaño `size` 
hist(rbinom(n = 100000, size = 40, prob = 0.1)/40)
# hist(rbinom(n = 100000, size = 40, prob = 0.1)/40,xlim = c(0,1))
#' 
#' en promedio, ¿qué porcentaje del área tendría un 
#' 10% o menos de personas infectadas (es decir, 4 de 40)?
pbinom(q = 4, size = 40, prob=0.1)
#' 
#' ejemplo 2:
#' 
#' podemos usar la distribución beta para describir la
#' proporción de personas vacunadas en una población 
#' durante un proceso de vacunación
hist(rbeta(n = 100000, shape1 = 1, shape2 = 3.8))
#' 
#' en promedio, ¿qué porcentaje de la población
#' tendría un 70% o más de personas vacunadas?
1- pbeta(q = 0.7, shape1 = 1, shape2 = 3.8)

