# [Estimación de Parámetros – Intervalos de Confianza]

## EJERCICIO 1
porcentaje_vitamina <- c(46.3, 48.4, 47.2, 44.1, 44.5, 43.9, 47.0, 47.7, 
                        46.4, 44.8, 43.6, 45.0, 43.1, 42.0, 43.5, 44.1)

# (A) Suponer  que  la  población  de  tales  valores  presenta una  distribución normal  
# con  un  desvío estándar poblacional igual a 2 y construir un 
# intervalo de confianza al 95% para la media de la población. 

alpha <- 0.05
varianza <- 2^2
n <- length(porcentaje_vitamina)
media <- mean(porcentaje_vitamina)
cuantil<- qnorm(1 - alpha/2)

lim_inf <- media - cuantil * sqrt(varianza) / sqrt(n)
lim_sup <- media + cuantil * sqrt(varianza) / sqrt(n)
lim_inf
lim_sup
# => (44.12002, 46.07998)


# (B) Ahora realizar el mismo intervalo pero con la varianza muestral

n = length(porcentaje_vitamina) 
sigma = var(porcentaje_vitamina) 
sem = sigma/sqrt(n);

E = qnorm(.95) * sem;
xbar = mean(porcentaje_vitamina)
xbar + c(-E, E)
# => (43.69858, 46.50142)


# (C) Hallar un intervalo de confianza para el desvío estándar poblacional

ds <- 2
n <- length(porcentaje_vitamina)
alfa <- 1 - 0.95
alfa_medios <- alfa / 2

limite_sup <- ( (n-1) * (ds)^2 ) / qchisq(alfa_medios, df=n-1)
limite_inf <- ( (n-1) * (ds)^2 ) / qchisq(1 - alfa_medios, df=n-1)
limite_inf
limite_sup
# => (2.182739, 9.581392)


## EJERCICIO 2

# (A)
cuadros <- 13
promedio_primer_corte <- 125 # tn/ha
ds <- 19 # tn/ha
alfa_medios <- (95/100)/2
alfa_medios

lim_inf <- promedio_primer_corte - ( (alfa_medios) * ds / sqrt(cuadros) )
lim_sup <- promedio_primer_corte + ( (alfa_medios) * ds / sqrt(cuadros) )
lim_inf
lim_sup
# => (122.4969, 127.5031)
# Con un 95% de confianza, el verdadero rendimiento promedio está entre 122.4 y 127.5


# (B)
zstar = qnorm(0.95)
sigma = 7 # tn/ha 
E = 1 # quiero precision del 1%
tam_muestra <- (zstar^2 * sigma^2) / E^2 
tam_muestra
# Requiero un tamaño de muestra de 530 con una precisión de error de 1%.
# Si calculo de nuevo el intervalo de confianza con 530 muestra se acerca mucho a los datos dados
# (124.2145 , 125.7855)


# EJERCICIO 3

machos <- 36
promedio_machos <- 3.1
desviacion_machos <- 0.4

hembras <- 28
promedio_hembras <- 2.8
desviacion_hembras <- 0.2

promedio_general <- (promedio_machos - promedio_hembras)
desviacion_general <- (desviacion_machos/machos) + (desviacion_hembras/hembras)

pnorm(0.48, mean = promedio_general, sd=desviacion_general, lower.tail = FALSE)
pnorm(0.43, mean = promedio_general, sd=desviacion_general, lower.tail = FALSE)



# [Pruebas de Hipótesis]

## EJERCICIO 1

xbar = 880             # sample mean 
mu0 = 800               # hypothesized value 
sigma = 80             # population standard deviation 
n = 15                 # sample size 

z = (xbar-mu0)/(sigma/sqrt(n)) 
z                      # test statistic 
# => z:3.872983

# como z es mayor a nivel de significancia, se rechaza la hipotesos nula y se 
# acepta que el aumento de peso alcanzado por los novillos es superior a los 800 grs/día



## EJERCICIO 2

# a) Analizar y explicar las diferencias entre las muestras a través de los gráficos de caja.
# Rta: 
#    - No hay valores atipicos en las muestras tomadas.
#    - Las plantulas de la localidad A crecen más o tienen mas altura que las de la localidad B
#    - La diferencia entre lo que crece las plantulas de la localidad A y B es de 5 mm

# b) Realizar el planteo de hipótesis correspondiente. Aplicar la prueba de “t”. Explique los resultados.

# H0: uA  = uB
# H1: uA != uB

media_loc_A <- 18.4
media_loc_B <- 15.8

s_A <- 1.20
s_B <- 1.12

N <- 20

SED <- sqrt( (s_A/N) + (s_B/N) )
SED

t <- (media_loc_A - media_loc_B) / SED
t

# Dado que t es mayor al valor critico, se rechaza la hipotesis nula, por lo que 
# las plantulas tienen medias de crecimiento diferentes y hay evidencia suficiente.

# c) 

# Localidad A
N_A <- 20
promedio_A <- 18.4
s_A <- 1.20
alfa_medios <- (95/100)/2

lim_infA <- promedio_A - ( (alfa_medios) * sqrt(s_A) / sqrt(N_A) )
lim_supA <- promedio_A + ( (alfa_medios) * sqrt(s_A) / sqrt(N_A) )
lim_infA
lim_supA
# => (18.28365, 18.51635)

# Localidad B
N_B <- 20
promedio_B <- 15.8
s_B <- 1.12
alfa_medios <- (95/100)/2

lim_infA <- promedio_B - ( (alfa_medios) * sqrt(s_B) / sqrt(N_B) )
lim_supA <- promedio_B + ( (alfa_medios) * sqrt(s_B) / sqrt(N_B) )
lim_infA
lim_supA
# => (15.68168, 15.91832)

# Los intervalos de confianza indican que las dos localidades de plantulas difieren en 
# su crecimiento. El intervalo para A difiere en un poco mas de 3mm/dia de la localidad B


## EJERCICIO 3: 

localidad_C <- c(15, 14, 14, 16, 16, 15, 17, 17, 16, 15)
localidad_D <- c(17, 16, 14, 16, 16, 17, 18, 18)

# H0: uA - uB  = 0
# H1: uA - uB != 0

media_loc_C <- mean(localidad_C)
media_loc_D <- mean(localidad_D)

s_C <- sqrt(var(localidad_C))
s_D <- sqrt(var(localidad_D))

N_C <- length(localidad_C)
N_D <- length(localidad_D)

SED <- sqrt( (s_C/N_C) + (s_D/N_D) )
SED

t <- abs(media_loc_C - media_loc_D) / SED
t

# Dado que t es menor al valor critico entonces se rechaza la hipotesis nula. Es decir
# que no hya evidencia suficiente para decir que las medias de los valores de crecimiento
# de las plantulas en  la localidades C y D son iguales.

t.test(localidad_C, localidad_D)

# Dado que el p-value es mayor a 0.05 se acepta la hipotesis alternativa por lo que e
# estamos seguros que las medias de crecimiento de las plantulas son diferentes.


## EJERCICIO 4

localidad_C <- c(15, 14, 14, 16, 16, 15, 17, 17, 16, 15)
localidad_D <- c(17, 16, 14, 16, 16, 17, 18, 18)

# H0: uA = uB
# H1: uA > uB

t.test(localidad_C, y = localidad_D, alternative = c("greater"), conf.level = 0.95)

# Ya que el p-value es mayor a 0.05 se rechaza la hipotesis nula y se acepta
# que semillas de la localidad D presentan mayor crecimiento que las de C con un 95% 
# de confianza.


# b) Comparar con el ejercicio anterior.
# Rta: De ejercicio anterior sabiamos que las medias son diferentes y la pruebas de 
# hipotesis de una cola lo confirma, tambien confirmamos que la localidad D tiene mayor
# crecimiento.

## EJERCICIO 5:
# Partiendo de la muestra 2 - 4 - 5 - 5 - 4 . Probar la hipótesis de que la variancia poblacional es 4.

# H0: sigma <= 4
# H1: sigma > 4

muestra <- c(2, 4, 5, 5, 4)
var_muestral <- var(muestra)
alfa <- 0.05
var_poblacional <- 4

Xp <- ( (length(muestra) - 1) *  var_muestral ^ 2 ) / var_poblacional
Xp

# Como Xp es menor al valor critico (9,4877) entonces se rechaza H0. ya que no hay evidencia para 
# afirmar que la varianza poblacional es 4. 

