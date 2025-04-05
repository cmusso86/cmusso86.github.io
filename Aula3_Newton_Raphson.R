if(!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse)


my_newton <- function(x0,
                      g, 
                      h,
                      tol = 1e-3, 
                      max.iter = 50,
                      x = x0, 
                      iter = 0,
                      made.changes = T){
  
  g <- function(x)log(x)/(1+x)
  h <- function(x){
    ((x+1)*(1+1/x-log(x)))/(3 + 4/x +1 /x^2-2*log(x))
  }
  
  while(made.changes & (iter < max.iter)){
    x.old <- x
    iter <- iter +1
    made.changes <- F
    x.new <- x + h(x)
    relative.change = abs(x.new-x.old)/abs(x.old)
    made.changes <- (relative.change > tol)
    x = x.new}
  return(list(value = x, value = g(x), iter = iter))
}

my_newton(x0 = 3)
