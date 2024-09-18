#1. Muestra que la función runif produce una distribución uniforme que coincide con la teórica
#2. Muestra que rt genera distribuciones t que coinciden con la teórica
#3. Muestra que rf genera distribuciones F que coinciden con la teórica
#4. Muestra que rchisq genera distribuciones chisq que coinciden con la teórica

#Para mostrarlo, simplemente dibuja las distribuciones y comprueba que el histograma se aproxima a su distribución teórica.

#### 1- DISTRIBUCION UNIFORME

set.seed(1)

#teorica

uniforme_manual <- function(x,max=1, min=0){
  
  rep(1/(max-min), length(x))
}

x <- seq(-10,10,0.1)

y_generador <- uniforme_manual(x,1,0)

plot(y_generador~x,type="l")


#empirica

uniforme_sim <- runif(10000,0,1)

d <- density(uniforme_sim)

plot(d)
lines(y_generador~x, col="green")

### 2- DISTRIBUCION T-STUDENT

set.seed(2)

#teorica

t_manual <- function(t,gl){
  t=(gamma((gl+1)/2)/(sqrt(gl*pi)*gamma(gl/2)))*(1+t**2/gl)^-((gl+1)/2)
}


df <- 10
t <- seq(-6,6,0.1)

y_t <- t_manual(t,df)

plot(y_t~t, type="l")

#empirica

t_sim <- rt(10000,df)
t_sim <- t_sim[t_sim>-6 & t_sim<6]

d_t <- density(t_sim)

plot(d_t)
lines(y_t~t, col="green")

### 3- DISTRIBUCION F-Snedecor

set.seed(3)

#teorica

snedecor_manual = function (x,m,n) {
  f = 1/(x*beta(m/2,n/2))*sqrt((m*x)^m*(n^n)/(m*x+n)^(m+n))
} #m y n son los grados de libertad

x <- seq(0,10,0.1)
m=5
n=5

y_snedecor <- snedecor_manual(x,m,n)

plot(y_snedecor~x, type="l")


#empirica

snedecor_sim <- rf(10000,m,n)
snedecor_sim <- snedecor_sim[snedecor_sim>-10 & snedecor_sim<10]

d_snedecor <- density(snedecor_sim)
plot(d_snedecor)

lines(y_snedecor~x, col="green")

### 4- DISTRIBUCION CHI-CUADRADO

set.seed(4)

#teorica

rchisq_manual = function(x, gl){
  n = (((1/2)^(gl/2)) / (gamma (gl/2))) * (x^((gl/2)-1)) * (exp(-x/2))
}

x <- seq(-10,10,0.1)
gl=3

y_chisq <- rchisq_manual(x,gl)

plot(y_chisq~x, type="l")


#empirica

chisq_sim <- rchisq(10000,gl)

chisq_sim <- chisq_sim[chisq_sim>-10 & chisq_sim<10]

d_chisq <- density(chisq_sim)

plot(d_chisq)
lines(y_chisq~x,col="green")

######################

# 5 dados, probabilidad que salga el 1 (distribucion multinomial??)

set.seed(5)

#teorica

k <- 5 #numero de dados

prob_ <- 5/6 #probabilidad de sacar cualquier numero que no sea 1

v_teorico <- 1-(prob_^k)  #probabilidad de que al menos un dado saque 1

v_teorico #(casi 0.6)



#empirica (n=400)

prob <- 1/6

x <- c()

for (n in 1:400) {
  
  # Simulamos lanzar 5 dados y verificamos si al menos uno muestra un 1
  
  resultados <- replicate(n, any(sample(1:6, 5, replace = TRUE) == 1))
  
  x[n] <- mean(resultados)
  # Calcula la proporción de veces que el resultado es TRUE, es decir, la proporción de simulaciones en las que al menos uno de los 5 dados mostró un 1.
}

v_empirico <- mean(resultados)
v_empirico #0.6

plot(x, type="l", xlab="Número de repeticiones (n)")

abline(h=v_empirico,col="green")

###### 3. EJERCICIOS SOBRE EL TEOREMA CENTRAL DEL LÍMITE

#Distribucion T


# DISTRIBUCION F

set.seed(5)

media_F = function(gl){
  return(gl / (gl-2))
}

sd_F = function(gl){
  sqrt((2*(gl**2)*(gl+gl-2))/(gl*((gl-2)**2)*(gl-4)))
}


x = seq(-100, 200, .1) # los valores estaran en este rango

y_fgl5 <- rf(1000,5,5)
y_fgl10 <- rf(1000,10,10)
y_fgl50 <- rf(1000,50,50)
y_fgl100 <- rf(1000,100,100)

#distribuciones normales según los grados de libertad

y.ecuacion5 = dnorm(x, media_F(5), sd_F(5))
y.ecuacion10 = dnorm(x, media_F(10), sd_F(10))
y.ecuacion50 = dnorm(x, media_F(50), sd_F(50))
y.ecuacion100 = dnorm(x, media_F(100), sd_F(100))


y_fgl5 <- recorta(y_fgl5,-10,10)
y_fgl10 <- recorta(y_fgl10,-10,10)
y_fgl50 <- recorta(y_fgl50,-10,10)
y_fgl100 <- recorta(y_fgl100,-10,10)


par(mfrow=c(2,2))

plot(density(y_fgl5), main="5 gl")
lines(y.ecuacion5~x, col="green")

plot(density(y_fgl10), main="10 gl")
lines(y.ecuacion10~x, col="green")

plot(density(y_fgl50), main="50 gl")
lines(y.ecuacion50~x, col="green")

plot(density(y_fgl100), main="100 gl")
lines(y.ecuacion100~x, col="green")


#DISTRIBUCION CHI-CUADRADO

set.seed(5)

x = seq(-100, 200, .1) 

y.chiqgl1 = rchisq(1000, 1)
y.chiqgl5 = rchisq(1000, 5) 
y.chiqgl10 = rchisq(1000, 10) 
y.chiqgl100 = rchisq(1000, 100) 

#distribuciones normales según los grados de libertad

y.ecuacion1 = dnorm(x, 1, sqrt (2*1))
y.ecuacion5 = dnorm(x, 5, sqrt (2*5))
y.ecuacion10 = dnorm(x, 10, sqrt (2*10))
y.ecuacion100 = dnorm(x, 100, sqrt (2*100))

y.chiqgl1 = recorta(y.chiqgl1, -2, 10)
y.chiqgl5 = recorta(y.chiqgl5, 0, 10)
y.chiqgl10 = recorta(y.chiqgl10, 0, 15)
y.chiqgl100 = recorta(y.chiqgl100, 0, 150)

par(mfrow=c(2,2))

plot(density(y.chiqgl1), main="1 gl")
lines(y.ecuacion1~x, col="green")

plot(density(y.chiqgl5), main="5 gl")
lines(y.ecuacion5~x, col="green")

plot(density(y.chiqgl10), main="10 gl")
lines(y.ecuacion10~x, col="green")

plot(density(y.chiqgl100), main="100 gl")
lines(y.ecuacion100~x, col="green")


