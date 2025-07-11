itpc <- read_excel("Recursos Tributarios de Origen Provincial (TOP).xlsx")

itpc <- itpc %>%
  mutate(prov = as.character(provincia))

base_mod <- base_mod %>%
  left_join(itpc %>% select(prov, yr, itpc), by = c("prov", "yr"))

base_mod_1822 <- base_mod_1822 %>%
  left_join(itpc %>% select(prov, yr, itpc), by = c("prov", "yr"))


#Modelo básico jerárquico sin protestas (base completa)
##Controla por NBI, alineamiento, elecciones. Incluye efectos aleatorios por provincia y año.
mod_base <- lmer(
  cupos_h_log ~ nbi + alineamiento + elecciones + itpc + 
    (1 | prov) + (1 | yr),
  data = base_mod
)

#Modelo con protestas (2018–2022)
##Incorpora log(protestas) como predictor adicional.
mod_protestas <- lmer(
  cupos_h_log ~ nbi + alineamiento + elecciones + log_protestas + itpc + 
    (1 | prov) + (1 | yr),
  data = base_mod_1822
)

#Interacción Elecciones × Protestas (2018–2022)
#Evalúa si el efecto de las elecciones cambia según el nivel de protesta.

mod_inter_ep <- lmer(
  cupos_h_log ~ nbi + alineamiento + elecciones * log_protestas + itpc + 
    (1 | prov) + (1 | yr),
  data = base_mod_1822
)

#Modelo con interacción Elecciones × Gobierno (base completa)
##Analiza si el efecto electoral varía según el gobierno en funciones.

mod_inter_eg <- lmer(
  cupos_h_log ~ nbi + alineamiento + elecciones * gobierno + itpc + 
    (1 | prov),
  data = base_mod
)


#Modelo con efectos aleatorios por gobierno (base completa)
##Captura variación entre gobiernos.
mod_gob_random <- lmer(
  cupos_h_log ~ nbi + alineamiento + elecciones + itpc + 
    (1 | prov) + (1 | gobierno),
  data = base_mod
)


#Modelo jerárquico robusto final (pendiente aleatoria)
##Incluye pendiente aleatoria de elecciones por gobierno.
mod_final <- lmer(
  cupos_h_log ~ nbi + alineamiento + elecciones + log_protestas + itpc + 
    (1 | prov) + (1 + elecciones | gobierno),
  data = base_mod_1822
)


#Modelo sobrerrepresentacion: trato a senadores como variable nivel 2
mod_sobrerrep <- lmer(
  cupos_h_log ~ nbi + alineamiento + elecciones + senadores + itpc + 
    (1 | prov) + (1 | yr),
  data = base_mod
)

mod_sobrerrep1822 <- lmer(
  cupos_h_log ~ nbi + alineamiento + elecciones + senadores + log_protestas + itpc + 
    (1 | prov) + (1 | yr),
  data = base_mod_1822
)

#Modelo bayesiano equivalente 
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

model_list <- list(
  mod_base,
  mod_protestas,
  mod_inter_ep,
  mod_inter_eg,
  mod_gob_random,
  mod_final, 
  mod_sobrerrep,
  mod_sobrerrep1822
)


stargazer(
  model_list,          
  type = "html",      
  out = "modelos_jerarquicos.html",
  title = "Modelos jerárquicos de regresión multivariada",
  column.labels = c(
    "Modelo 1: Base (10–23)",
    "Modelo 2: + Protestas (18–22)",
    "Modelo 3: E × Protestas",
    "Modelo 4: E × Gobierno",
    "Modelo 5: Gob. Aleatorio",
    "Modelo 6: Pend. E × Gob",
    "Modelo 7: + Senadores (10–23)",
    "Modelo 8: + Senadores + Protestas (18–22)"
  ),
  dep.var.caption = "Variable dependiente: log(Cupos por mil habitantes)",
  dep.var.labels.include = FALSE,
  align = TRUE,
  no.space = TRUE,
  digits = 3,
  header = FALSE
)

labels <- c(
  "nbi" = "Necesidades Básicas Insatisfechas (NBI)",
  "alineamiento" = "Alineamiento político",
  "elecciones" = "Año electoral",
  "log_protestas" = "Protestas (log)",
  "elecciones:log_protestas" = "Elecciones × Protestas",
  "gobiernoCFK2" = "Gobierno CFK (2011–2015)",
  "gobiernoMacri" = "Gobierno Macri (2015–2019)",
  "gobiernoAlberto" = "Gobierno Fernández (2019–2023)",
  "elecciones:gobiernoCFK2" = "Elecciones × CFK",
  "elecciones:gobiernoMacri" = "Elecciones × Macri",
  "elecciones:gobiernoAlberto" = "Elecciones × Fernández",
  "senadores" = "Sobrerrepresentación senatorial"
)

tablamodelos <- modelsummary(
  model_list,  # tu lista de modelos
  coef_rename = labels,
  output = "gt",
  title = "Modelos jerárquicos comparativos",
  stars = TRUE,
  gof_map = c("nobs", "logLik", "AIC", "BIC", "ICC", "RMSE"),
  notes = list(
    "Errores estándar entre paréntesis.",
    "Todos los modelos controlan por NBI y alineamiento político.",
    "Las protestas están transformadas logarítmicamente.",
    "Modelos (2), (3), (6) y (8) corresponden al período 2018–2022."
  ),
  escape = FALSE
) |>
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = px(14),
    data_row.padding = px(5)
  )

gtsave(tablamodelos, "modelos_jerarquicos.png")   # o .pdf

model_list3 <- list(
  "Jerárquico base" = mod_base,
  "Con protestas" = mod_protestas,
  "Interacción E×P" = mod_inter_ep,
  "Interacción E×Gob" = mod_inter_eg,
  "Random slope por Gob" = mod_gob_random,
  "Bayesiano" = mod_bayes
)

modelsummary(model_list, output = "modelos_comparados.html")

