# ===============================
# MODELOS POISSON - CUPOS
# ===============================

# 📦 Instalar (si no están) y cargar librerías necesarias
# install.packages(c("fixest", "readxl", "dplyr", "stargazer"))
library(fixest)        # Modelos Poisson con efectos fijos
library(readxl)        # Leer archivos Excel
library(dplyr)         # Manipulación de datos
library(stargazer)     # Tablas estilo clásico
library(modelsummary)  # Comparación elegante de modelos

# 📂 Cargar base de datos
base <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Facu/Tesis/base_final.xlsx")

# 🧹 Limpieza y preparación
base <- base %>%
  mutate(
    provincia = as.factor(provincia),
    año = as.numeric(año),
    protestas = coalesce(protestas_acled, protestas_pemps)  # Unificación de fuentes
  ) %>%
  select(-protestas_acled, -protestas_pemps)

# ===============================
# ESPECIFICACIONES POISSON
# ===============================

# 📊 Modelo A: Efectos fijos bidireccionales (provincia + año)
modelo_A <- fepois(
  cupos ~ pred_nbi_mice + alineamiento + diputados + protestas + poblacion_proyectada | provincia + año,
  data = base
)

# 📊 Modelo B: Efectos fijos solo por año
modelo_B <- fepois(
  cupos ~ pred_nbi_mice + alineamiento + elecciones + diputados + senadores + protestas + poblacion_proyectada | año,
  data = base
)

# 📊 Modelo C: Efectos fijos solo por provincia
modelo_C <- fepois(
  cupos ~ pred_nbi_mice + alineamiento + elecciones + diputados + senadores + protestas + poblacion_proyectada | provincia,
  data = base
)

# ===============================
# EXPORTAR RESULTADOS
# ===============================

# Comparación de modelos Poisson
model_list <- list(
  "FE Provincia + Año" = modelo_A,
  "FE Año" = modelo_B,
  "FE Provincia" = modelo_C
)

modelsummary(
  model_list,
  output = "modelos_poisson_comparados.html"
)

