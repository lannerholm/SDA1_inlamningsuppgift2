library(remotes)
#install_github("StatisticsSU/sda1paket")
library(sda1)
head(electricitycost)

df = electricitycost
extrapeople = df$people - 1

### 1.1 
lambda <- mean(extrapeople)
xvalues <- 0:5
pois_probs <- dpois(x = xvalues, lambda)

### 1.2
names(pois_probs) <- xvalues
observed <- tally(extrapeople, format = "proportion")
par(mfrow = c(1, 2))
barplot(observed, xlab  = "x", ylab = "P(X=x)", main = "Observerade data")
barplot(pois_probs, col = "orange", xlab  = "x", ylab = "P(X=x)", main = "Poisson med lambda = 1.65")
par(mfrow = c(1, 1))

### 1.3 Pois(x > 4. lambda = 1.65) = 1 - Pois(x <= 4, lambda = 1.65)
prob <-1 - ppois(4, lambda = lambda)
prob
### 1.4
observed_variance <- var(extrapeople)
observed_expected_value <- weighted.mean(extrapeople)
observed_variance
observed_expected_value
# Variansen för den observerade datan (1.42) är mindre än variansen för Poisson-distributionen (1.65).
# Frågan är om den är för låg för att poisson ska vara en bra modell. 
# Jag gör stickprovsvarianser från poissonfördelningen och  den observerade variansen.
nsim <- 10000
n <- length(extrapeople)
simulated_variance <- rep(0, nsim)
count <- 0
for (i in 1:nsim) {
  count <- count + 1
  x <- rpois(n = n, lambda = lambda)
  simulated_variance[count] <- var(x)
}
hist(simulated_variance, 30, freq = FALSE)
x_grid <- seq(min(simulated_variance), max(simulated_variance), length = 40)
x_grid
standard_deviation <- sd(simulated_variance)
normal_curve <- dnorm(x_grid, mean = mean(simulated_variance), sd = sd(simulated_variance))
lines(x_grid, normal_curve, col = 2, lwd = 2)
# I diagrammet kan vi avläsa att variansen hos observerade datan (1.42) datan skulle vara mycket ovanlig om den observerade datan cerkligen följde en poisson distribution. Kanske är det därför olämpligt att använda poisson-fördelning för att modellera people. 