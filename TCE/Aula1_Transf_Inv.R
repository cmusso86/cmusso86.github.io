if(!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse)

source("Aula1_NPA.R")
## CASO CONTÍNUO

# Para a exponencial 
r_exp_carol <- function(p, lambda = 5.2){
  -(log(1-p)/lambda)
}
lambda <- 5.2
us <- gerador_npa(n = 200)
t <- seq(0,10,.01)
hist(r_exp_carol(us), probability = T)
lines(t,lambda*exp(-lambda*(t)),col=c("red"))

## CASO DISCRETO

######################################
## Código R
######################################
n <- 1000
p <- c(.1, .2, .2, .2, .3)
cdf <- cumsum(p)
x <- numeric(n)
for (i in 1:n) x[i] <- sum(as.integer(runif(1) > cdf))
rbind(table(x) / n, p)

## usando a função sample
n <- 1000
p <- c(.1, .2, .2, .2, .3)
x <- sample(0:4, size=n, prob=p, replace=TRUE)
rbind(table(x) / n, p)


## Caso da geométrica agora
######################################
## Código R
######################################
# método TI para gerar uma amostra do modelo Geométrico(p=1/4)
n <- 1000
p <- 0.25
u <- runif(n)
k <- ceiling(log(1-u) / log(1-p)) - 1

# mais eficiente
k <- floor(log(u) / log(1-p))
mean(k) #E[X] = (1-p)/p = .75/.25 =3