# ===============================
# ANÁLISIS MULTINIVEL - LOG CUPOS
# ===============================

# 📦 Cargar librerías necesarias
library(lme4)         # Modelos multinivel
library(dplyr)        # Manipulación de datos
library(readxl)       # Leer archivos Excel
library(writexl)      # Escribir archivos Excel
library(ggplot2)      # Visualización
library(modelsummary) # Tablas de resultados elegantes
library(stargazer)    # Reporte clásico de modelos

# 📂 Cargar base de datos
base <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Facu/Tesis/base_final.xlsx")

# 🧹 Limpieza y preparación
base <- base %>%
  mutate(
    protestas = coalesce(protestas_acled, protestas_pemps),  # Consolidar fuente de protestas
    protestas = ifelse(is.na(protestas), 0, protestas),       # Imputar ceros donde falta
    provincia = as.factor(provincia),
    año = as.numeric(año),
    log_cupos = log(cupos + 1)  # Log-transformación (con +1 para evitar log(0))
  ) %>%
  select(-protestas_acled, -protestas_pemps)  # Eliminar columnas originales

# ===============================
# MODELOS MULTINIVEL (LOG CUPOS)
# ===============================

# 🎯 Modelo completo
modelo_log <- lmer(
  log_cupos ~ pred_nbi_mice + alineamiento + elecciones + diputados + senadores + protestas +
    (1 | provincia) + (1 | año),
  data = base
)

# ❌ Sin senadores
modelo_log2 <- lmer(
  log_cupos ~ pred_nbi_mice + alineamiento + elecciones + diputados + protestas +
    (1 | provincia) + (1 | año),
  data = base
)

# ❌ Sin elecciones ni senadores
modelo_log3 <- lmer(
  log_cupos ~ pred_nbi_mice + alineamiento + diputados + protestas +
    (1 | provincia) + (1 | año),
  data = base
)

# ❌ Sin protestas
modelo_log4 <- lmer(
  log_cupos ~ pred_nbi_mice + alineamiento + elecciones + diputados + senadores +
    (1 | provincia) + (1 | año),
  data = base
)

# ❌ Sin protestas ni senadores
modelo_log5 <- lmer(
  log_cupos ~ pred_nbi_mice + alineamiento + elecciones + diputados +
    (1 | provincia) + (1 | año),
  data = base
)

# 📆 Modelo restringido al período 2018–2022
base_reciente <- base %>% filter(año >= 2018 & año <= 2022)

modelo_reciente <- lmer(
  log_cupos ~ pred_nbi_mice + alineamiento + elecciones + diputados + protestas +
    (1 | provincia) + (1 | año),
  data = base_reciente
)

# ===============================
# VISUALIZACIÓN Y EXPORTACIÓN
# ===============================

# 👁️ Ver resumen del modelo principal
summary(modelo_log)

# 💾 Exportar resultado del modelo principal
modelsummary(
  list("Modelo Log-Cupos" = modelo_log),
  statistic = "conf.int",
  stars = TRUE,
  output = "modelo_logcupos_resultados.html"
)

# 📊 Comparación entre modelos
modelsummary(
  list(
    "Multinivel (log cupos) - completo" = modelo_log,
    "Multinivel (log cupos) - sin senadores" = modelo_log2,
    "Multinivel (log cupos) - sin elecciones ni senadores" = modelo_log3,
    "Multinivel (log cupos) - sin protesta" = modelo_log4,
    "Multinivel (log cupos) - sin protesta ni senadores" = modelo_log5,
    "Multinivel (log cupos) - 2018–2022" = modelo_reciente
  ),
  statistic = "conf.int",
  stars = TRUE,
  output = "comparacion_modelos_Multinivel.html"
)
