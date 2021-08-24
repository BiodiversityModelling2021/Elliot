#=====================================================================================#
# R script for Andrew's exercice during the Biodiversity Modelling 2021 Summer School #
#=====================================================================================#

dataset <- readr::read_csv2("https://raw.githubusercontent.com/BiodiversityModelling2021/Elliot/main/sides.csv")
sides <- dataset$sides_hit


# Exploration
hist(sides, freq = F)
curve(dbinom(x, size = 4, p = 0.5), n = 5, type = "l", xlim = c(0, 4), add = T)


# Likelihood estimation

## Estimation of p
j <- 1
LL <- numeric(20)
for (i in seq(0.01, 1, 0.05)) {
  LL[j] <- sum(dbinom(x = sides, size = 4, i, log = TRUE))
  j <- j + 1
}
P_estimated <- seq(0.01, 1, 0.05)[which.max(LL)]

## Plotting likelihood profile
plot(x = seq(0.01, 1, 0.05), y = LL)
abline(v = P_estimated, col = "red")

## Sampling from candidate distribution
test1 <- rbinom(20, size = 4, p = P_estimated)
hist(test1, freq = F)


# Simulated annealing

## Initialization of likelihood, candidate and temperature functions
h <- function(obs, pars) { sum(dbinom(x = obs, size = 4, pars, log = TRUE)) }
c_x <- function(pars_lo, pars_hi) { runif(1, pars_lo, pars_hi) }
T_fn <- function(T0, alpha, step) { T0 * exp(alpha * step) }

## Initialization of parameters
res <- matrix(nrow = nsteps, ncol = 3)
T0 <- 10
alpha <- -0.001
nsteps <- 10000
pars0 <- 0.01
pars_lo <- 0
pars_hi <- 1

## Main loop
for (step in 1:nsteps) {
    pars1 <- pars0
    pars1 <- c_x(pars_lo, pars_hi)

    h1 <- h(sides, pars1)
    h0 <- h(sides, pars0)
    diff <- h1 - h0

    if (diff > 0) {
      pars0 <- pars1
    } else {
      p <- exp(diff / T_fn(T0, alpha, step))
      if (runif(1) < p) {
        pars0 <- pars1
      }
    }

  res[step,] <- c(step, pars0, h(sides, pars0))
}

## Exploration of results
plot(c(1:nsteps), res[,3], type = "l", xlab = "Time step", ylab = "h(x)", cex = 2, log = "x")

hist(sides, freq = F)
add_model <- function(step) {
    P_estimated = res[step, 2]
    curve(dbinom(x, size = 4, p = P_estimated), n = 5, type = "l", xlim = c(0, 4), add = T)
}
add_model(10)
add_model(100)
add_model(1000)
add_model(10000)

## Sampling from candidate distribution
test2 <- rbinom(20, size = 4, p = res[10000,2])
hist(test2, freq = F)
