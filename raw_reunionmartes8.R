# CON ESTO TRABAJAMOS EN LA REU DEL MARTES 08
#=====================================
# Script completo - Modelos Multinivel
# =====================================

#cargo librerias
library(lme4)
library(dplyr)
library(readxl)
library(modelsummary)

#Cargo base
base <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Facu/Tesis/base_final.xlsx")

# limpieza (acá puse 0 en las variables NA de protestas, entiendo que esto no está bien)
base <- base %>%
  mutate(
    provincia = as.factor(provincia),
    año = as.numeric(año),
    log_cupos = log(cupos + 1),
    protestas = coalesce(protestas_acled, protestas_pemps),
    #protestas = ifelse(is.na(protestas), 0, protestas)
  ) %>%
  select(-protestas_acled, -protestas_pemps, -senadores)

# ====================================
# MODELOS COMPLETOS (todo el período)
# ====================================

# Modelo 1: completo con protestas y elecciones
modelo_log1 <- lmer(
  log_cupos ~ pred_nbi_mice + alineamiento + elecciones + diputados + protestas +
    poblacion_proyectada + (1 | provincia) + (1 | año),
  data = base
)

# Modelo 2: sin elecciones
modelo_log2 <- lmer(
  log_cupos ~ pred_nbi_mice + alineamiento + diputados + protestas +
    poblacion_proyectada + (1 | provincia) + (1 | año),
  data = base
)

# Modelo 3: sin protestas
modelo_log3 <- lmer(
  log_cupos ~ pred_nbi_mice + alineamiento + diputados + elecciones +
    poblacion_proyectada + (1 | provincia) + (1 | año),
  data = base
)

# Modelo 4: sin elecciones ni protestas
modelo_log4 <- lmer(
  log_cupos ~ pred_nbi_mice + alineamiento + diputados + poblacion_proyectada +
    (1 | provincia) + (1 | año),
  data = base
)

# ===============================
# MODELOS PARCIALES (2018–2022)
# ===============================

base_1822 <- base %>% filter(año >= 2018 & año <= 2022)

# Modelo 5: completo con protestas (2018–2022)
modelo_log5 <- lmer(
  log_cupos ~ pred_nbi_mice + alineamiento + elecciones + diputados + protestas +
    poblacion_proyectada + (1 | provincia) + (1 | año),
  data = base_1822
)

# Modelo 6: sin protestas (2018–2022)
modelo_log6 <- lmer(
  log_cupos ~ pred_nbi_mice + alineamiento + elecciones + diputados + poblacion_proyectada +
    (1 | provincia) + (1 | año),
  data = base_1822
)

# ======================
# TABLA COMPARATIVA
# ======================

modelsummary(
  list(
    "M1: Completo" = modelo_log1,
    "M2: Sin elecciones" = modelo_log2,
    "M3: Sin protestas" = modelo_log3,
    "M4: Sin elecciones ni protestas" = modelo_log4,
    "M5: 2018–2022 con protestas" = modelo_log5,
    "M6: 2018–2022 sin protestas" = modelo_log6
  ),
  statistic = "conf.int",
  stars = TRUE,
  output = "modelos_multinivel_completos.html"
)
