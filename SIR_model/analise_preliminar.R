Infected <- c(1, 1,	1, 2,	2,	2,	2,	4,	4,	13,	13,	20,	25,	31,	38,	52,	151,	151,	162,	200,	321,	372)
Day <- 1:(length(Infected))
N <- 207660929 # population of mainland china

old <- par(mfrow = c(1, 2))
plot(Day, Infected, type ="b")
plot(Day, Infected, log = "y")
abline(lm(log10(Infected) ~ Day))
title("Confirmed Cases 2019-nCoV Brazil", outer = TRUE, line = -2)

SIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta/N * I * S
    dI <- beta/N * I * S - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}

library(deSolve)
init <- c(S = N-Infected[1], I = Infected[1], R = 0)
RSS <- function(parameters) {
  names(parameters) <- c("beta", "gamma")
  out <- ode(y = init, times = Day, func = SIR, parms = parameters)
  fit <- out[ , 3]
  sum((Infected - fit)^2)
}

Opt <- optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1)) # optimize with some sensible conditions
Opt$message
## [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"

Opt_par <- setNames(Opt$par, c("beta", "gamma"))
Opt_par
##      beta     gamma 
## 0.6746089 0.3253912

t <- 1:70 # time in days
fit <- data.frame(ode(y = init, times = t, func = SIR, parms = Opt_par))
col <- 1:3 # colour

matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col)
matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col, log = "y")
## Warning in xy.coords(x, y, xlabel, ylabel, log = log): 1 y value <= 0
## omitted from logarithmic plot

points(Day, Infected)
legend("bottomright", c("Susceptibles", "Infecteds", "Recovereds"), lty = 1, lwd = 2, col = col, inset = 0.05)
title("SIR model 2019-nCoV Brazil", outer = TRUE, line = -2)

library(tidyr)
fitlong <- gather(data = fit, key = "subjects", value = "number", -time)

fitlong$subjects <- factor(fitlong$subjects, labels = c("Infectados", "Recuperados", "Sucetíveis"))


library(ggplot2)

cbPalette <- c("#de201a", "#29bf12", "#ffbf00")
p <- ggplot(data = fitlong,
            mapping = aes(x = time, y = number, colour = subjects)) +
  geom_line(size = 1) + 
  geom_vline(xintercept = 22, linetype="dotted", colour = "#D3D3D3", size = 1) +
  labs(x = "Tempo (dias)", y = "Número de indivíduos", colour = "Indivíduos") +
  theme_dark() + theme(legend.position = "top") +
  scale_colour_manual(values = cbPalette) +
  ggtitle("Modelo SIR COVID-19 Brasil (a reta pontilhada representa onde estamos agora)")
print(p)
