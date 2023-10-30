# exemplo professor









# exemplo soccer
shots = c('goal','goal','goal','miss','miss',
          'goal','goal','miss','goal','goal')


shots = c('goal','goal','goal','miss','miss',
          'goal','goal','miss','goal','goal')


shotsNum = as.numeric(shots == 'goal')
N = length(shots)              
imput <- list(
  shotsNum, N
)

# sample size
nGoal = sum(shots == 'goal')           # number of shots made
nMiss = sum(shots == 'miss') 
theta = seq(from = 1 / (N + 1),
            to = N / (N + 1),
            length = 10)



theta


fit <- stan(file='fit_data_soccer.stan', 
            data=imput)


library(rstan)
rstan_options(auto_write = TRUE)            # Cache compiled Stan programs
options(mc.cores = parallel::detectCores()) # Parallelize chains
input_data <- read_rdump("intro.data.R")
input_data 


fit <- stan(file='intro.stan', data=input_data, seed=4938483)
print(fit)

params <- extract(fit)

c_dark <- c("#8F2727")
c_dark_highlight <- c("#7C0000")

hist(params$theta, breaks=seq(-4, 4, 0.25),
     col=c_dark, border=c_dark_highlight, main="",
     xlim=c(-4, 4), xlab="theta", yaxt='n', ylab="")

util <- new.env()
source('stan_utility.R', local=util)
ls(util)
c_light <- c("#DCBCBC")
c_light_highlight <- c("#C79999")
c_mid <- c("#B97C7C")
c_mid_highlight <- c("#A25050")
c_dark <- c("#8F2727")
c_dark_highlight <- c("#7C0000")

c_light_trans <- c("#DCBCBC80")
c_light_highlight_trans <- c("#C7999980")
c_mid_trans <- c("#B97C7C80")
c_mid_highlight_trans <- c("#A2505080")
c_dark_trans <- c("#8F272780")
c_dark_highlight_trans <- c("#7C000080")

c_green_trans <- c("#00FF0080")

writeLines(readLines("normal1.stan"))

N <- 3
input_data <- list("N" = N)


fit <- stan(file='normal1.stan', data=input_data, seed=4938483)
launch_shinystan(fit)

input_data1 <- read_rdump("simulation.data.R")
input_data2 <- read_rdump("simulation2.data.R")

fit <- stan(file='fit_data.stan', data=input_data1, seed=4938483)

util$check_all_diagnostics(fit)
params = rstan::extract(fit)

plot(params$mu, log(params$sigma),
     col=c_dark_trans, pch=16, cex=0.8,
     xlab="mu", ylab="log(sigma)")
shiny <- launch_shinystan(fit)
launch_shinystan(shiny)

# y <- array(extract(simu)$y[1,])
# 
# stan_rdump(c("N", "y"), file="simulationUM.data.R")


## com rstanarm

mu_prior <- rstanarm::normal(location = 0, scale = 1)
sigma_prior <- rstanarm::normal(location = 0, scale = 1)

fit1 <- stan_glm(switch ~ dist100, data = wells,
                 family = binomial(link = "logit"),
                 prior = t_prior, prior_intercept = t_prior,
                 cores = 2, seed = 12345)


schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

schools_dat
