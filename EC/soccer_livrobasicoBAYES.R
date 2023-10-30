shots = c('goal','goal','goal','miss','miss',
          'goal','goal','miss','goal','goal')

# convert to numeric, arbitrarily picking goal=1, miss=0
shotsNum = as.numeric(shots == 'goal')
N = length(shots)                      # sample size
nGoal = sum(shots == 'goal')           # number of shots made
nMiss = sum(shots == 'miss')           # number of those miss


x1 = rbinom(1000, size = 10, p = .5)
x2 = rbinom(1000, size = 10, p = .85)

mean(x1); hist(x1)


theta = seq(from = 1 / (N + 1),
            to = N / (N + 1),
            length = 10)

theta


pTheta = dunif(theta)

pData = sum(pDataGivenTheta * pTheta)
