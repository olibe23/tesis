# ===============================
# MODELOS POISSON - CUPOS
# ===============================

# install.packages(c("fixest", "readxl", "dplyr", "stargazer"))
library(fixest)        # Modelos Poisson con efectos fijos
library(readxl)        # Leer archivos Excel
library(dplyr)         # Manipulación de datos
library(stargazer)     # Tablas estilo clásico
library(modelsummary)  # Comparación elegante de modelos

# base
base <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Facu/Tesis/base_final.xlsx")

# limpieza
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

# Modelo: Efectos fijos bidireccionales (provincia + año)
modelo <- fepois(
  cupos ~ pred_nbi_mice + alineamiento + diputados + senadores + elecciones + protestas + poblacion_proyectada | provincia + año,
  data = base
)

#Este modelo me elimina elecciones y senadores por colinearidad.

# Modelo A: Efectos fijos bidireccionales (provincia + año)
modelo_A <- fepois(
  cupos ~ pred_nbi_mice + alineamiento + diputados + protestas + poblacion_proyectada | provincia + año,
  data = base
)

#Modelo B: Efectos fijos solo por año
modelo_B <- fepois(
  cupos ~ pred_nbi_mice + alineamiento + elecciones + diputados + senadores + protestas + poblacion_proyectada | año,
  data = base
)

# Modelo C: Efectos fijos solo por provincia
modelo_C <- fepois(
  cupos ~ pred_nbi_mice + alineamiento + elecciones + diputados + senadores + protestas + poblacion_proyectada | provincia,
  data = base
)

# ===============================
#  RESULTADOS
# ===============================

# Comparación de modelos
model_list <- list(
  "Completo | FE Provincia + Año" = modelo,
  "Parcial | FE Provincia + Año" = modelo_A,
  "FE Año" = modelo_B,
  "FE Provincia" = modelo_C
)

modelsummary(
  model_list,
  output = "modelos_poisson_comparados.html"
)

