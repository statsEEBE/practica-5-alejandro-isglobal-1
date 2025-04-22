#distribucion poblacional (de un resultado)

curve(dnorm(x, 95.3, 5.7), 
      xlim=c(80, 120),
      col="red")

set.seed(84)
n <- 4
simul <- rnorm(n, 95.3, 5.7)
sum(simul)

#suma muestral
set.seed(84)
Y <- function(i){sum(rnorm(n, 95.3, 5.7))}
simul100000 <- sapply(1:100000, Y)
hist(simul100000)
mean(simul100000)

#teoria, valor esperado de Y: E(Y)=n*mu
4*95.3

#dibujemos el teorema
n <- 4
mu <- 95.4
sigma <- 5.7
  
hist(simul100000, freq = FALSE)
curve(dnorm(x, n*mu, sqrt(n)*sigma),
      col="red", add=TRUE)

#la varianza de la suma muestral de n=100
#teoria V(Y)=n*simga^2
n <- 100
n*simga^2
#simulacion (confirmar)
set.seed(84)
n <- 100
Y <- function(i){sum(rnorm(n, 95.3, 5.7))}
simul100000 <- sapply(1:100000, Y)
hist(simul100000)
var(simul100000)

#P(X>103)
mu <- 95.4
sigma <- 5.7
1-pnorm(103, mu, sigma)

#media de la media muestral para n=4
#E(X_bar)
set.seed(84)
n <- 4
X_bar <- function(i){mean(rnorm(n, 95.3, 5.7))}
simul100000 <- sapply(1:100000, X_bar)
hist(simul100000)
mean(simul100000)
mu

#prob media muestral 
#P(Xbar <98)
n <- 4
mu <- 95.4
sigma <- 5.7
pnorm(98, mu, sigma/sqrt(n))

##distribucion de las varianzas muestrales
set.seed(84)
n <- 100
SSq <- function(i){var(rnorm(n, 95.3, 5.7))}
simul100000 <- sapply(1:100000, SSq)
hist(simul100000)

#probabilidad estimada de que SSq>32
mean(as.numeric(simul100000>32))

#teoria P(SSq>32)=P(W>32*(n-1)/sigma^2), 
#W es una variable aleatoria Chi cuadrado
n <- 100
sigma <- 5.7
1-pchisq(32*(n-1)/sigma^2, n-1)

