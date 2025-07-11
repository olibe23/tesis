
mod_robusto_1 <- lmer(
  cupos_h_log ~ nbi + alineamiento + elecciones + protestas +
    (1 | prov) + (1 + elecciones | gobierno),
  data = base_mod
)

mod_robusto_2 <- lmer(
  cupos_h_log ~ nbi + alineamiento + elecciones * log_protestas +
    (1 | prov) + (1 | yr),
  data = base_mod_1822
)

library(brms)

# Modelo bayesiano con efectos cruzados y pendiente aleatoria
mod_bayes <- brm(
  formula = cupos_h_log ~ nbi + alineamiento + elecciones + protestas +
    (1 | prov) + (1 + elecciones | gobierno),
  data = base_mod,
  family = gaussian(),  # podés cambiar a lognormal si lo ves más apropiado
  prior = c(
    prior(normal(0, 5), class = "b"),
    prior(normal(0, 5), class = "Intercept"),
    prior(exponential(1), class = "sd")  # para los efectos aleatorios
  ),
  chains = 4, cores = 4, iter = 4000, warmup = 1000,
  control = list(adapt_delta = 0.95),
  seed = 1234
)

summary(mod_bayes)
plot(mod_bayes)
conditional_effects(mod_bayes, effects = "elecciones:protestas")
