# TAREA 1

set.seed(4)

expo1 <- function(N,n,k){
  
  poblacion <- sample(1:2,N,replace = T) #distribucion no normal
  mu_pob <- mean(poblacion)
  sd_pob <- sd(poblacion)
  
  p <- c()
  
  for (i in 1:k){
    
    muestra <- poblacion[sample(1:N,n)]
    p[i] <- t.test(muestra,mu=mu_pob)$p.value
  }
  
  a_teo <- 0.05
  a_emp <- length(p[p<a_teo])/k
  
  return(a_emp)
  
}

#500 repeticiones, tamaño poblacional=1000, tamaño meustral=25

rep <- 500

res <- c()

for (i in 1:rep){
  
  res[i] <- expo1(1000,25,500)
}

hist(res)

grafico <- function(longitud) {
  
  rep <- 100
  res <- numeric(rep)
  
  # Inicializar los vectores
  datos_1 <- numeric(longitud)
  des_1 <- numeric(longitud)
  lim_sup <- numeric(longitud)
  lim_inf <- numeric(longitud)
  
  # Loop para calcular el alfa empírico y los intervalos de confianza
  for (i in 1:longitud) {
    for (j in 1:rep) {
      res[j] <- expo1(1000, 25, 500)  # Usar la función correcta (expo1)
    }
    
    # Promedio y desviación estándar de las repeticiones
    datos_1[i] <- mean(res)
    des_1[i] <- sd(res)
    
    # Intervalos de confianza al 95%
    lim_sup[i] <- datos_1[i] + 1.96 * (des_1[i] / sqrt(rep))
    lim_inf[i] <- datos_1[i] - 1.96 * (des_1[i] / sqrt(rep))
  }
  
  # Crear el rango para el eje x
  rangos <- 1:longitud
  
  # Gráfico
  plot(rangos, datos_1, ylim = c(0.042, 0.051), type = "l", lwd = 2, col = "red",
       xlab = "Valores de n", ylab = "Alfa empírico",
       main = "Alfa empírico y sus intervalos de confianza")
  
  # Añadir las zonas de los intervalos de confianza
  polygon(c(rangos, rev(rangos)), c(lim_sup, rev(lim_inf)),
          col = "lavender", border = NA)
  
  # Añadir las líneas de los valores de alfa empírico y los límites superior e inferior
  lines(rangos, datos_1, type = "l", lwd = 2, col = "red")
  lines(rangos, lim_sup, type = "l", lwd = 0.5, col = "blue")
  lines(rangos, lim_inf, type = "l", lwd = 0.5, col = "blue")
}

# Ejecutar la función con un rango de 5 (incluye 2,3,4,5)
rango <- 10
grafico(rango)



# TAREA 2

set.seed(4)

a_teo <- 0.05


expo1 <- function(N,n,k){
  
  poblacion <- rnorm(N,0,1) #distribucion normal
  mu_pob <- mean(poblacion)
  sd_pob <- sd(poblacion)
  
  p <- c()
  
  for (i in 1:k){
    
    muestra <- poblacion[sample(1:N,n)]
    p[i] <- t.test(muestra,mu=mu_pob)$p.value
  }
  
  a_emp <- length(p[p<a_teo])/k
  
  return(a_emp)
  
}

rep <- 500

tamaños <- c(2,10,15,20)

res <- c()
res_sd <- c()
lim_sup <- c()
lim_inf <- c()

for (i in 1:length(tamaños)){
  
  res_in <- c()
  
  for (j in 1:rep){
    
    res_in[j] <- expo1(1000,tamaños[i], 500)
    res[i] <- mean(res_in)
    res_sd <- sd(res_in)
    lim_sup[i] <- res[i]+1.96*(res_sd[i]/sqrt(rep))
    lim_inf[i] <- res[i]-1.96*(res_sd[i]/sqrt(rep))
  }
}

plot(tamaños, res, ylim=c(0.040,0.060), type="l")
polygon(c(tamaños,rev(tamaños)), c(lim_sup, rev(res)),
        col="lavender", border=NA)

polygon(c(tamaños, rev(tamaños)), c(lim_inf, rev(res)),
        col="lavender", border=NA)
lines(tamaños, res, type="l", lwd=1, col="red")
lines(tamaños, lim_sup, type="l", lwd=0.5, lty=2)
lines(tamaños, lim_inf,type="l", lwd=0.5, lty=2)
abline(a_teo, 0, col="grey")


# TAREA 3

set.seed(4)

library(SuppDists)

expo9 <- function(N,n,k,s){
  
  parms <- JohnsonFit(c(0,2,s,20), moment="use")
  sJohnson(parms)
  poblacion <- rJohnson(N,parms)
  
  mu_pob <- mean(poblacion)
  sd_pob <- sd(poblacion)
  
  p <- c()
  
  for(i in 1:k){
    
    muestra <- poblacion[sample(1:N, n)]
    pi[i] <- t.test(muestra,mu=mu_pob)$p.value
    
  }
  
  a_teo <- 0.05
  a_emp <- length(p[p<a_teo])/k
  
  return(a_emp)
  
}

#Diferentes niveles de sesgo

sesgo <- c(0,3,0.5)

errorI <- c()

c=1

res_sd <- c()
lim_sup <-c()
lim_inf <- c()

for(j in sesgo){
  rep <- 500
  res <- c()
  
  for (i in 1:rep){res[i] <- expo9(1000,10,500,j)}
  
  errorI[c] <- mean(res)
  res_sd[c] <- sd(res)
  lim_sup[c] <- errorI[c]+1.96*(res_sd[c]/sqrt(rep))
  lim_inf[c] <- errorI[c]-1.96*(res_sd[c]/sqrt(rep))
  
  c=c+1
  
  sd(res)
}

plot(sesgo, errorI, type="l")
polygon(c(sesgo, rev(sesgo)), c(lim_sup,rev(errorI)),
        col="lavender", border=NA)
polygon(c(sesgo, rev(sesgo)), c(lim_inf,rev(errorI)),
        col="lavender", border=NA)

lines(sesgo, errorI, type="l", lwd=1, col="red")
lines(sesgo, errorI, type="l", lwd=0.05, lty=2)
lines(sesgo, errorI, type="l", lwd=0.05, lty=2)
abline(0.05,0,col="grey")

# TAREA 4

set.seed(4)

library(pwr)

#Se elige un valor cualquiera de mu pero que genere valores razonables de potencia
#Si esta muy proximo a la media poblacional la potencia será 0.
#Si esta muy lejos será 1 y no nos interesan esos caso
#Una estrategia es hacer que mu=mean(poblacion)+0.8*sd(poblacion)

expo3 <- function(N,n,k,range_limit){
  
  poblacion <- sample(1:2,N,replace=T)
  mu_pob <- mean(poblacion)
  sd_pob <- sd(poblacion)
  
  p <- c()
  
  for (i in 1:k){
    
    muestra <- poblacion[sample(1:N,n)]
    
    p[i] <- t.test(muestra,mu=mu_pob+0.8*sd_pob)$p.value
  }
  
  beta_emp <- length(p[p>a_teo])/k
  
  d=(10-mu_pob)/sd_pob
  beta_teo <- 1-pwr.t.test(n,
                           d=d,
                           sig.level = a_teo,
                           type="one.sample")$power
  
  return(beta_teo-beta_emp)
  
}


grafico <- function(longitud){
  
  rep <- 500
  res <- c()
  datos <- c()
  desv <- c()
  lim_sup <- c()
  lim_inf <- c()
  
  for (i in 1:longitud){
    for (j in 1:rep){
      res[j] <- expo3(1000,25,500,i)
    }
    datos[i] <- mean(res)
    desv[i] <- sd(res)
    lim_sup[i] <- datos[i]+1.96*(desv[i]/sqrt(rep))
    lim_inf[i] <- datos[i]-1.96*(desv[i]/sqrt(rep))
    
  }
  
  datos <- datos[-1]
  desv <- desv[-1]
  lim_sup <- lim_sup[-1]
  lim_inf <- lim_inf[-1]
  
  rangos <- seq(2,longitud,1)
  
  plot(rangos,datos,type="l")
  polygon(c(rangos,rev(rangos)),c(lim_sup,rev(datos)),
          col="lavender", border=NA)
  polygon(c(rangos,rev(rangos)),c(lim_inf,rev(datos)),
          col="lavender",border=NA)
  
  lines(rangos,datos,type="l", lwd=1,col="red")
  lines(rangos,lim_sup,type="l",lwd=0.5)
  lines(rangos,lim_inf,type="l",lwd=0.5)
  
}

rango <- 10

grafico(rango)

# TAREA 5

set.seed(4)

a_teo <- 0.05

expo4 <- function(N,n,k){
  
  poblacion <- rnorm(N,10,10)
  mu_pob <- mean(poblacion)
  sd_pob <- sd(poblacion)
  p <- c()
  
  for (i in 1:k){
    
    muestra <- poblacion[sample(1:N,n)]
    p[i] <- t.test(muestra,mu=mu_pob+0.8*sd_pob)$p.value
    
  }
  
  beta_emp <- length(p[p>a_teo])/k
  
  d=(10-(mu_pob+0.8*sd_pob))/sd_pob
  beta_teo <- 1-pwr.t.test(n,
                           d=d,
                           sig.level = a_teo,
                           type="one.sample")$power
  
  return(beta_teo-beta_emp)
  
}


rep <- 250
tamaños <- c(2,10,15,20)
res <- c()
res_sd <- c()
lim_sup <- c()
lim_inf <- c()

for (i in 1:length(tamaños)){
  
  res_in <- c()
  
  for(j in 1:rep){
    res_in[j] <- expo4(1000,tamaños[i],250)
  }
  
  res[i] <- mean(res_in)
  res_sd[i] <- sd(res_in)
  lim_sup[i] <- res[i]+1.96*(res_sd[i]/sqrt(rep))
  lim_inf [i] <- res[i]-1.96*(res_sd[i]/sqrt(rep))
  
}

plot(tamaños,res, ylim=c(-0.02,0.02), type="l")
polygon(c(tamaños,rev(tamaños)),c(lim_sup,rev(res)),
        col="lavender",border=NA)
polygon(c(tamaños,rev(tamaños)), c(lim_inf,rev(res)),
        col="lavender",border=NA)

lines(tamaños,res,type="l",lwd=1,col="red")
lines(tamaños,lim_sup,type="l",lwd=0.5,lty=2)
lines(tamaños,lim_inf,type="l",lwd=0.05,lty=2)

# TAREA 6

set.seed(4)

expto12 <-function(N,n,k,s){
  parms<-JohnsonFit(c(0, 2, s, 20), moment="use")
  sJohnson(parms)
  poblacion <- rJohnson(N, parms)
  mu.pob <- mean(poblacion)
  sd.pob <- sd(poblacion)
  p <- vector(length=k)
  for (i in 1:k){
    muestra <- poblacion[sample(1:N, n)]
    p[i] <- t.test(muestra, mu=mu.pob+0.8*sd.pob)$p.value
  }
  a_teo = 0.05
  # calculamos beta empirica
  beta_emp <- length(p[p>a_teo])/k
  # calculamos beta teorica
  d=(10 - mu.pob+0.8*sd.pob) / sd.pob
  beta_teo <- 1 - pwr.t.test(n,
                             d=d,
                             sig.level=a_teo,
                             type="one.sample")$power
  return (beta_teo-beta_emp)
}


#Exploramos en diferentes niveles de sesgo
Sesgo = seq(0,3,0.5)
ErrorII=vector(length=length(seq(0,3,0.5)))
set.seed(1)
c=1
res_sd <- c()
lim_sup <- c()
lim_inf <- c()
for (j in seq(0,3,0.5)) {
  rep <- 500
  res <- vector(length=rep)
  for (i in 1:rep) {
    res[i] <- expto12(1000, 25, 500,j)
  }
  # hist(res)
  ErrorII[c]=mean (res)
  res_sd[c] = sd(res)
  lim_sup[c] = ErrorII [c] + 1.95 * (res_sd[c]/sqrt(rep))
  lim_inf[c] = ErrorII [c] - 1.95 * (res_sd[c]/sqrt(rep))
  c =c+1
  sd(res)
}


plot(Sesgo, ErrorII, type = "l")
polygon(c(Sesgo, rev(Sesgo)), c(lim_sup, rev(ErrorII)),
        col = "lavender", border = NA)
polygon(c(Sesgo, rev(Sesgo)), c(lim_inf, rev(ErrorII)),
        col = "lavender", border = NA)
lines(Sesgo,ErrorII, type = "l", lwd = 1, col = "red")
lines(Sesgo, lim_sup, type = "l", lwd = 0.5, lty = 2)
lines(Sesgo, lim_inf, type = "l", lwd = 0.5, lty = 2)
abline(0.05, 0, col = "grey")






