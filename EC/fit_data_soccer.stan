data {
  int<lower=1> N; // Number of observations
  vector[N] shotsNum;
}

parameters {
  real theta;
}

model {
  // Prior model
  theta ~ beta(10, 10);
  // Observational model
 shotsNum ~ bernoulli_logit(theta);
}

