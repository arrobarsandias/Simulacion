# TAREA 1

set.seed(3)

library(SuppDists)

valores_asimetria <- seq(-2,2,0.01)
valores_kurtosis <- seq(0,6,0.01)

matriz <- matrix(0, nrow=length(valores_asimetria), ncol=length(valores_kurtosis))

for (i in 1:length(valores_kurtosis)){
  
  for (j in 1:length(valores_asimetria)){
    
    matriz[j,i] <- tryCatch({
      JohnsonFit(c(0,1,valores_asimetria[i],valores_kurtosis[j]), moment="use")
      1},
      
      error=function(e){
        
        return(0)})
    
  }
}

#Uso de índices: j se utiliza para recorrer valores_asimetria y i para valores_kurtosis. Así que la asignación matriz[j, i] debe ser correcta.

#head(matriz)


library(ggplot2)

library(reshape2)

matriz_larga <- melt(matriz)

# Asignar nombres a las columnas
colnames(matriz_larga) <- c("Asimetría", "Kurtosis", "Valor")

# Crear el gráfico de calor con ggplot2
ggplot(matriz_larga, aes(x = Kurtosis, y = Asimetría, fill = Valor)) +
  geom_tile() +
  scale_fill_gradient(low = "red", high = "green") +
  labs(title = "Gráfico de Calor de Asimetría vs Kurtosis",
       x = "Kurtosis", 
       y = "Asimetría") +
  theme_minimal()

# TAREA 2

# Una normal

set.seed(3)

datos_n <- tryCatch({JohnsonFit(c(0,1,0,3), moment="use")}, error=function(e){return(print(0))})

normal_johnson <- rJohnson(1000,datos_n)
sJohnson(normal_johnson)

shapiro.test(normal_johnson)
plot(density(normal_johnson), main="Normal-Johnson")

#Una platicurtica = kurtosis<0

set.seed(3)

datos_p <- tryCatch({JohnsonFit(c(0,1.5,0,4), moment="use")}, error=function(e){return(print(0))})

p_johnson <- rJohnson(1000,datos_p)
sJohnson(p_johnson)

plot(density(p_johnson), main="Platicúrtica-Johnson")

#Una mesocúrtica. La normal es mesocurtica

set.seed(3)

datos_m <- tryCatch({JohnsonFit(c(0,1,0,3), moment="use")}, error=function(e){return(print(0))})

m_johnson <- rJohnson(1000,datos_m)
sJohnson(m_johnson)

plot(density(m_johnson), main="Mesocúrtica-Johnson")

#Una asimétrica positiva

set.seed(3)

datos_asiN <- tryCatch({JohnsonFit(c(0,1,.8,4), moment="use")}, error=function(e){return(print(0))})

asiN_johnson <- rJohnson(1000,datos_asiN)
sJohnson(asiN_johnson)

plot(density(asiN_johnson), main="Asimétrica positiva-Johnson")

#Una asimétrica negativa

set.seed(3)

datos_asiP <- tryCatch({JohnsonFit(c(0,1,-.8,4), moment="use")}, error=function(e){return(print(0))})

asiP_johnson <- rJohnson(1000,datos_asiP)
sJohnson(asiP_johnson)

plot(density(asiP_johnson), main="Asimétrica negativa-Johnson")


# TAREA 3

set.seed(3)

#distribucion uniforme

N=1000
k=25000
n=25

poblacion_unif <- runif(N,0,1)

mu_pob_u <- mean(poblacion_unif)

sd_pob_u <- sd(poblacion_unif)

medias <- c()

for (i in 1:k){
  
  muestra <- sample(poblacion_unif,n, replace=T)
  
  medias[i] <- mean(muestra)
  
}

hist(medias)

plot(density(medias))

#Comprobacion teórica vs empirica

mu_teo_u <- mu_pob_u

sd_teo_u <- sd_pob_u/sqrt(n)


mu_emp_u <- mean(medias)
sd_emp_u <- sd(medias)

sprintf("Media teórica: %.2f -- Media empírica: %.2f", mu_teo_u, mu_emp_u)

sprintf("Sd teórica: %.2f -- Sd empírica: %.2f", sd_teo_u, sd_emp_u)


# Distribucion chi-cuadrado (2 grados de libertad)

set.seed(3)

N=1000
k=25000
n=25

poblacion_chi <- rchisq(1000,2)

mu_pob_chi <- mean(poblacion_chi)
sd_pob_chi <- sd(poblacion_chi)

medias <- c()

for (i in 1:k){
  
  muestras <- sample(poblacion_chi,n,replace=T)
  medias[i] <- mean(muestras)
}

hist(medias)
plot(density(medias))


#Comprobación teórica vs empírica

mu_teo_chi <- mu_pob_chi
sd_teo_chi <- sd_pob_chi/sqrt(n)

mu_emp_chi <- mean(medias)
sd_emp_chi <- sd(medias)

sprintf("Media teórica: %.2f -- Media empírica: %.2f", mu_teo_chi, mu_emp_chi)

sprintf("Sd teórica: %.2f -- Sd empírica: %.2f", sd_teo_chi, sd_emp_chi)


# TAREA 4

set.seed(3)

N=1000
k=25000

#con n>30

n1=40

x <- seq(0,100,0.01)

poblacion_n1 <- rbinom(x,1,0.5)

mu_pob_binom <- mean(poblacion_n1)
sd_pob_binom <- sd(poblacion_n1)

medias <- c()

for (i in 1:k){
  
  muestras <- sample(poblacion_n1,n1,replace = T)
  medias[i] <- mean(muestras)
  
}

hist(medias)
plot(density(medias))

#Comprobación teórica vs empírica

mu_teo_binom <- mu_pob_binom
sd_teo_binom <- sd_pob_binom/sqrt(n1)

mu_emp_binom <- mean(medias)
sd_emp_binom <- sd(medias)

sprintf("Media teórica: %.2f -- Media empírica: %.2f", mu_teo_binom, mu_emp_binom)

sprintf("Sd teórica: %.2f -- Sd empírica: %.2f", sd_teo_binom, sd_emp_binom)


set.seed(3)

#con n<5

n2 <- 4

x <- seq(0,100,0.01)

poblacion_n2 <- rbinom(x,1,0.5)

mu_pob <- mean(poblacion_n2)
sd_pob <- sd(poblacion_n2)

medias <- c()

for (i in 1:k){
  
  muestras <- sample(poblacion_n2,n2,replace = T)
  
  medias[i] <- mean(muestras)
}

hist(medias)
plot(density(medias))

#Comprobacion teórica vs empírica

mu_teo <- mu_pob
sd_teo <- sd_pob/sqrt(n2)

mu_emp <- mean(medias)
sd_emp <- sd(medias)

sprintf("Media teórica: %.2f -- Media empírica: %.2f", mu_teo, mu_emp)

sprintf("Sd teórica: %.2f -- Sd empírica: %.2f", sd_teo, sd_emp)


# TAREA 5

set.seed(3)


#Esperanza de la varianza

N=10000
n <- 30
sigma <- 4
mu <- 0

varianzas <- c()

for (i in 1:N){
  
  muestra <- rnorm(n,mu,sd=sqrt(sigma))
  
  varianzas[i] <- var(muestra)
  
}

esperanza_emp <- mean(varianzas)

esperanza_teo <- ((n-1)/n)* sigma

cat("Esperanza empírica de la varianza muestral:", esperanza_emp, "\n")
cat("Esperanza teórica de la varianza muestral:", esperanza_teo, "\n")


# Esperanza varianza insesgada

# Calculamos la varianza insesgada (por defecto en R) de una muestra aleatoria
muestra <- rnorm(n, mean = mu, sd = sqrt(sigma))
varianza_insesgada <- var(muestra)

# Comparamos con la fórmula manual
varianza_manual <- sum((muestra - mean(muestra))^2) / (n-1)

cat("Varianza insesgada calculada con var():", varianza_insesgada, "\n")
cat("Varianza insesgada calculada manualmente:", varianza_manual, "\n")


#Varianza (n * S^2 / sigma^2)

# Calculamos la variable escalada
chi_squared_vals <- (n * varianzas) / sigma

# Calculamos la varianza de esta variable
var_chi_squared_empirica <- var(chi_squared_vals)

# Varianza teórica
var_chi_squared_teorica <- 2 * (n-1)

cat("Varianza empírica de (n * S^2 / sigma^2):", var_chi_squared_empirica, "\n")
cat("Varianza teórica de (n * S^2 / sigma^2):", var_chi_squared_teorica, "\n")

#Comprobacion distribucion chi-cuadrado con n-1 grados de libertad

# Graficamos el histograma de los valores simulados
hist(chi_squared_vals, breaks = 30, probability = TRUE, main = "Distribución de (n * S^2 / sigma^2)",
     xlab = "Valores simulados")

# Superponemos la distribución teórica chi-cuadrado
curve(dchisq(x, df = n-1), col = "red", lwd = 2, add = TRUE)


# TAREA 6

set.seed(3)


n <- 1000

poblacion <- rnorm(n,0,1)

m_poblacion <- mean(poblacion)
sd_poblacion <- sd(poblacion)

#Intervalos teoricos
#0.95

sup_poblacion95 <- m_poblacion+1.96*(sd_poblacion/sqrt(n))
inf_poblacion95 <- m_poblacion-1.96*(sd_poblacion/sqrt(n))

#0.99

sup_poblacion99 <- m_poblacion+2.58*(sd_poblacion/sqrt(n))
inf_poblacion99 <- m_poblacion-2.58*(sd_poblacion/sqrt(n))



calcular_ic <- function(datos,media,desviacion,alfa){
  
  z <- qnorm(1-alfa/2)
  
  error_tipico <- z*desviacion/sqrt(length(datos))
  
  inf <- media-error_tipico
  sup <- media+error_tipico
  
  return(c(inf,sup))
}


muestras <- matrix(0, nrow=25000, ncol=length(poblacion))
medias <- numeric(25000)
desviaciones <- numeric(25000)


for (i in 1:25000){  
  
  muestras[i,] <- rnorm(length(poblacion), 0, 1)
  
  medias[i] <- mean(muestras[i,])
  
  desviaciones[i] <- sd(muestras[i,])}


intervalos99 <- matrix(0,25000,3)

for (i in 1:25000){
  
  intervalos99[i,1] <- calcular_ic(poblacion, medias[i],desviaciones[i],0.01)[1]
  
  intervalos99[i,2] <- calcular_ic(poblacion,medias[i], desviaciones[i], 0.01)[2]
  
  intervalos99[i,3] <- ifelse(intervalos99[i,1]>inf_poblacion99 | intervalos99[i,2]<sup_poblacion99,1,0)
  
}

intervalos95 <- matrix(0,25000,3)

for (i in 1:25000){
  
  intervalos95[i,1] <- calcular_ic(poblacion,medias[i],desviaciones[i], 0.05)[1]
  intervalos95[i,2] <- calcular_ic(poblacion,medias[i],desviaciones[i],0.05)[2]
  
  intervalos95[i,3] <- ifelse(intervalos95[i,1]>inf_poblacion95 | intervalos95[i,2]<sup_poblacion95,1,0)
  
}

table(intervalos95[,3])
table(intervalos99[,3])

porcentaje95 <- mean(intervalos95[,3]==1)*100
porcentaje99 <- mean(intervalos99[,3]==1)*100

cat("Con alfa = 0.05, el", porcentaje95, "% de las medias caen dentro del intervalo.\n")
cat("Con alfa = 0.01, el", porcentaje99, "% de las medias caen dentro del intervalo.\n")
