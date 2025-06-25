# esto me lo sugirió Chatgpt
#Cargar paquetes necesarios
library(readxl)
library(dplyr)
library(stargazer)

# Cargar la base
base <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Facu/Tesis/base_final.xlsx")

# Preparación
base <- base %>%
  mutate(
    provincia = as.factor(provincia),
    año = as.factor(año),  # para usar como dummies en glm
    protestas = coalesce(protestas_acled, protestas_pemps)
  ) %>%
  select(-protestas_acled, -protestas_pemps)

# Modelo Poisson con offset log(poblacion proyectada)
modelo_offset <- glm(
  cupos ~ pred_nbi_mice + alineamiento + elecciones + diputados + senadores + protestas +
    provincia + año,
  data = base,
  family = poisson(link = "log"),
  offset = log(poblacion_proyectada)
)

# Ver resultados
summary(modelo_offset)

# Exportar tabla elegante
stargazer(modelo_offset, type = "html", out = "modelo_poisson_offset.html")
