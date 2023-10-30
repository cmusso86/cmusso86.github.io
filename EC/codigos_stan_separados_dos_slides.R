# lm NORMAL
N <- 1000
set.seed(42)
coefs = c(5, .2)
mu = X %*% coefs
sigma = 2

x <- rnorm(n = N)
y <- rnorm(N, mu, sigma)

dados <- data.frame(
  x=x, y=y
)


modlm = lm(y ~ x, data = dados)
summary(modlm)

# Rstan

X <- cbind(Intecept=1, x)

dados_stan = list(
  N = N,
  K = ncol(X),
  y = y,
  X = X
)


# Create the Stan model object using Stan's syntax
stanmodelcode = "
data {                      // Data block
  int<lower = 1 >  N;       // Sample size
  int<lower = 1> K;         // Dimension of model matrix
  matrix[N, K] X;           // Model Matrix
  vector[N] y;              // Target variable
}

parameters {                // Parameters block
  vector[K] beta;           // Coefficient vector
  real<lower = 0> sigma;    // Error scale, lower bound at 0
}

model {                     // Model block
  vector[N] mu;
  mu = X * beta;            // Creation of linear predictor
  
  // priors
  beta  ~ normal(0, 10);  
  sigma ~ cauchy(0, 5);
  
  // likelihood
  y ~ normal(mu, sigma);
}
"


### Run the model and examine results

fit_Rstan = stan(
  model_code = stanmodelcode,
  data   = dados_stan
)

# summary
print(
  fit_Rstan,
  pars   = c('beta', 'sigma'),
  digits = 3,
  prob   = c(.025, .5, .975)
)


# com RSTANARM

beta_prior <- rstanarm::normal(0,10)
sigma_prior <- rstanarm::cauchy(0,5)

fit_rstanarm<-rstanarm::stan_glm(y~x, data=dados, 
                         prior=beta_prior,
                         prior_aux = sigma_prior,
                         seed=42)

fit_rstanarm$coefficients


# com tidymodels

library(tidymodels)

bayes_mod <-   
  linear_reg() %>% 
  set_engine("stan", 
             prior_intercept = beta_prior, 
             prior = beta_prior,
             prior_aux = sigma_prior) 

bayes_fit <- 
  bayes_mod %>% 
  fit(y ~ x, data = dados)

print(bayes_fit, digits = 5)












#############
### Stan ----
pacman::p_load(tidyverse, rstan, shinystan, bayesplot, coda, rstantools)
n <- 15
theta.real <- c(2, 7, 8)  # beta_0, beta_1, sigma
x.obs <- runif(n, 10, 20)
y.obs <- rnorm(n, theta.real[1] + theta.real[2] * x.obs, 
               sd=theta.real[3])


code.stan <- " 
data {
  int<lower=0> N;
  vector[N] x;
  vector[N] y;
}
parameters {
  real beta0;
  real beta1;
  real sigma;
}
model {
  y ~ normal(beta0 + x * beta1, sigma);
}
"
code.stan2 <-"
data {                      // Data block
  int<lower=1> N;           // Sample size
  int<lower=1> K;           // Dimension of model matrix
  matrix[N, K] X;           // Model Matrix
  vector[N] y;              // Target variable
}

transformed data {          // Transformed data block.
} 

parameters {                // Parameters block
  vector[K] beta;           // Coefficient vector
  real<lower=0> sigma;      // Error scale
}

transformed parameters {    // Transformed parameters block.
} 


model {                     // Model block
  vector[N] mu;
  mu = X * beta;            // Creation of linear predictor
  
  // priors
  beta ~ normal(0, 10);
  sigma ~ cauchy(0, 5);     
  
  // likelihood
  y ~ normal(mu, sigma);
}

generated quantities {      // Generated quantities block. 
}
"
data.stan <- list(
  x = x.obs,
  N = length(x.obs),
  y = y.obs
)

data.srtanarm <- data.frame(
  x = x.obs,
  y = y.obs
)

mod_fit2 <- stan(model_code=code.stan2, 
                data=data.stan, cores=4)

print(mod_fit, pars=c('beta', 'sigma'), 
      probs = c(.05,.95), digits=3)

params <- rstan::extract(mod_fit)

mean(params$beta0)
mean(params$beta1)
mean(params$sigma)

#launch_shinystan(mod_fit)



fit1 <- rstanarm::stan_glm(x~y,data=data.srtanarm , cores = 4)
summary(fit1, probs=c(.025, .975), digits=3)
fit1$coefficients

amostras.post <- extract(mod_fit)