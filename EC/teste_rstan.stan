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