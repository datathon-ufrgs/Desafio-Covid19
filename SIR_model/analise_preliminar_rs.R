# ------------------------------------
# Carrega pacotes do R

library(dplyr)
library(lubridate)
library(deSolve)
library(scales)
library(ggplot2)
library(tidyr)
library(ggthemes)
library(stringr)
library(httr)
library(jsonlite)

# ------------------------------------
# Funções 

SIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta/N * I * S
    dI <- beta/N * I * S - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}

RSS <- function(parameters) {
  names(parameters) <- c("beta", "gamma")
  out <- ode(y = init, times = Day, func = SIR, parms = parameters)
  fit <- out[ , 3]
  sum((Infected - fit)^2)
}

# ------------------------------------
# Carrega dados 

N <- 11329605 # RS

api_link <- 'https://api.coronaanalytic.com/history/brazil'
call_content <- rawToChar(GET('https://api.coronaanalytic.com/history/brazil/')[["content"]])

brazil_historico <- fromJSON(call_content) %>% 
  unnest(history) %>% 
  separate(name, into = c("uf_name", "uf"), sep = " \\(") %>% 
  mutate(
    date = as.Date(date, format = "%d/%m/%Y"),
    uf = str_remove_all(uf, '\\)')
  )

rs_historico <- brazil_historico %>% 
  filter(uf == "RS" & cases != 0)

Infected <- rs_historico$cases

Day <- 1:(length(Infected))

df <- data.frame(Day, Infected)
df$dm <- seq(ymd("2020-03-10"), ymd("2020-03-10") + (length(Day) - 1), by = "days")

# ------------------------------------
# Análise preliminar

p <- ggplot(data = df, mapping = aes(x = dm, y = Infected)) +
  geom_point() + geom_line() +
  labs(x = "Tempo (dias)", y = "Número de casos confirmados") + theme_bw()
p

p + scale_y_log10() + geom_smooth(method = "lm", se = F)

# ------------------------------------
# Modelo SIR: estimativa das taxas

init <- c(S = N - Infected[1], I = Infected[1], R = 0)

Opt <- optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1)) # optimize with some sensible conditions
Opt$message

Opt_par <- setNames(Opt$par, c("beta", "gamma"))
Opt_par

t <- 1:100 # time in days
fit <- data.frame(ode(y = init, times = t, func = SIR, parms = Opt_par))
fit$dia <- seq(ymd("2020-03-10"), ymd("2020-03-10") + (length(t) - 1), by = "days")

# ------------------------------------
# Modelo SIR: quantidades de interesse

#  basic reproduction number 
# (R0 is the average number of people infected from one other person)
R0 <- setNames(Opt_par["beta"] / Opt_par["gamma"], "R0")
R0

# herd immune threshold (HIT)
pc <- 1 - 1/R0
pc

# Pico do surto
fit[fit$I == max(fit$I), "I", drop = FALSE]

# Casos severos
max_infected <- max(fit$I)
max_infected / 5

# Casos com necessidade de cuidados intensivos
max_infected * 0.06

# Óbitos supondo uma taxa de fatalidade de 0.7%
# https://www.newscientist.com/article/mg24532733-700-why-is-it-so-hard-to-calculate-how-many-people-will-die-from-covid-19/
max_infected * 0.007


# ------------------------------------
# Modelo SIR: gráficos

fitlong <- gather(data = fit, key = "subjects", value = "number", -c(time, dia))
fitlong$subjects <- factor(fitlong$subjects,
                           labels = c("Infectados", "Recuperados", "Suscetíveis"))

cbPalette <- c("#de201a", "#29bf12", "#ffbf00")

p <- ggplot(data = fitlong,
            mapping = aes(x = dia, y = number, colour = subjects)) +
  geom_line(size = 1) + 
  scale_y_continuous(labels = comma) +
  geom_vline(xintercept = ymd("2020-03-19"), linetype = "dotted", colour = "black", size = 1) +
  labs(x = "Tempo (dias)", y = "Número de indivíduos", colour = "Indivíduos") +
  theme_bw() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_colour_manual(values = cbPalette) +
  ggtitle("Modelo SIR COVID-19 RS (a reta pontilhada representa onde estamos agora)")

p + annotate("segment",
             x = fit$dia[which.max(fit$I)] + 10,
             xend = fit$dia[which.max(fit$I)],
             y = max(fit$I) + 10000000,
             yend = max(fit$I) + 1000000,
             colour = "lightsalmon",
             size = 1, arrow = arrow()) +
  annotate("text",
           x = fit$dia[which.max(fit$I)] + 18,
           y = max(fit$I) + 10000000,
           parse = TRUE, size = 4,
           label = as.character(max(fit$I)))

p2 <- p + scale_y_log10() +
  geom_point(data = df, mapping = aes(x = dm, y = Infected, colour = NULL))

p2

# theme_wsj

# p <- ggplot(data = fitlong,
#             mapping = aes(x = time, y = number, colour = subjects)) +
#   geom_line() + 
#   scale_y_continuous(labels = comma) +
#   geom_vline(xintercept = 22, linetype = "dotted") +
#   labs(x = "Tempo (dias)", y = "Número de indivíduos", colour = "Indivíduos") +
#   theme_wsj() +
#   scale_colour_manual(values = cbPalette) +
#   ggtitle("Modelo SIR COVID-19 Brasil (a reta pontilhada representa onde estamos agora)")
# print(p)
# 
