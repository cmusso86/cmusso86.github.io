if(!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse)


# Um gerador NPA é um algoritmo que produz como output sequências 
# de números que podem ser usados em substituição de uma sucessão 
# iid de verdadeiros NAs.

## Gerador Congruencial Linear (de Lehmer)
## O R usa o "Mersenne-Twister".

gerador_npa <- function(n = 1000,
                        a = 16807,
                        c = 13, 
                        m = (2^31 -1), 
                        semente = 0,
                        method = "Lehmer"){
  x <- numeric(n)
  x[1] <- semente
  
  if(method == "Lehmer"){
    for(i in 2:n){
      x[i] <- (a*x[i-1] + c) %% m
    }
  }
 
  return(x/m)
} 

gerador_npa(n, a, m, semente, method = "Lehmer")

