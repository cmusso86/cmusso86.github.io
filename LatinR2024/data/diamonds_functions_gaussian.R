
### Cumulative probabilities ----

cumprobs <- function(y_obs, y_fit, mse_fit) {
  
  # p-values
  p <- pnorm(
    y_obs,
    mean = y_fit,
    sd = sqrt(mse_fit)
  )
  
  return(pmin(pmax(p, 1e-16), 1 - 1e-16))
  
}



### Recalibration ----

recal_exact <- function (
  h_fit_val,
  h_fit_test,
  y_fit_test,
  mse_val,
  p_values,
  n_acc,
  epsilon = 0
  
) {
  
  if(!is.matrix(h_fit_val)) {
    
    h_fit_val <- matrix(h_fit_val, ncol = 1)
    
  }
  
  if(!is.matrix(h_fit_test)) {
    
    h_fit_test <- matrix(h_fit_test, ncol = 1)
    
  }
  
  n <- length(y_fit_test)
  
  y_hat <- numeric()
  y_samples <- matrix(nrow = n, ncol = n_acc)
  
  knn <- nn2(
    data = h_fit_val,
    query = h_fit_test,
    k = n_acc,
    eps = epsilon
  )
  
  
  for (i in 1:n) {
    
    knn$nn.dists[i,] <- epk_kernel(knn$nn.dists[i,])
    
    knn_idx <- knn$nn.idx[i,]
    
    y_samples[i,] <- qnorm(
      p_values[knn_idx],
      mean = y_fit_test[i],
      sd = sqrt(mse_val)
    )
    
    y_hat[i] <- weighted.mean(
      x = y_samples[i,],
      w = knn$nn.dists[i,],
      na.rm = T
    )
    
  }
  
  return(
    list(
      y_hat = y_hat,
      y_samples = y_samples,
      y_kernel = knn$nn.dists,
      y_nn = knn$nn.idx
    )
  )
  
}



### Mean standard error ----

mse <- function (y_obs, y_fit) mean((y_obs - y_fit)^2)



### Mean absolute value ----

mav <- function (x) mean(abs(x))



### Confidence intervals ----

ci <- function (
  y_obs_test,
  y_fit_test,
  y_kernel = NULL,
  mse_fit = NULL,
  sig_lvl = .05
  
) {
  
  alpha <- sig_lvl / 2
  
  n <- length(y_obs_test)
  
  
  if (is.null(y_kernel)) {
    
    # Samples without kernel
    ci <- map(
      1:n,
      ~qnorm(
        c(alpha, 1 - alpha),
        mean = y_fit_test[.],
        sd = sqrt(mse_fit)
      )
    ) %>% 
      unlist() %>%
      matrix(ncol = 2, byrow = T)
    
  } else {
    
    # Samples with kernel
    ci <- map(
      1:n,
      ~wtd.quantile(
        x = y_fit_test[.,],
        weight = y_kernel[.,],
        q = c(alpha, 1 - alpha),
        na.rm = T
      )
    ) %>% 
      unlist() %>%
      matrix(ncol = 2, byrow = T)
    
  }
  
  ci[, 1] <- ifelse(ci[, 1] < 0, 0, ci[, 1])
  ci[, 2] <- ifelse(ci[, 2] < 0, 0, ci[, 2])
  
  captured <- ifelse(y_obs_test > ci[, 1] & y_obs_test < ci[, 2], 1, 0)
  coverage <- mean(captured)
  
  return(
    list(
      lower = ci[, 1],
      upper = ci[, 2],
      coverage = coverage
    )
  )
  
}



### Epanechnikov kernel ----

epk_kernel <- function (x) {.75 * (1 - (x / max(x))^2)}



### Kernel constraint ----

zero_constraint <- function(w) return(w * k_cast(0, k_floatx()))


