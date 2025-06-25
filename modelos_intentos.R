library(stringr)
library(tidyr)
library(writexl)
library(readxl)
library(tibble)
library(lubridate)
library(plm)
library(dplyr)
library(readr)
library(ggplot2)
library(lmtest)
library(stargazer) 
library(modelsummary)


base <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Facu/Tesis/base_final.xlsx")


base <- base %>%
  mutate(provincia = as.factor(provincia),
         año = as.numeric(año)) 

base <- base %>%
  mutate(across(starts_with("protestas"), ~ na_if(., 0)))

base <- base %>%
  mutate(protestas = coalesce(protestas_acled, protestas_pemps))
base<- base %>% select(-protestas_acled, -protestas_pemps)

#Modelo de efectos fijos bidireccional (provincia y año)
modelo_cupos_fijo <- plm(
  cupos ~ pred_nbi_suave + alineamiento + elecciones + diputados + senadores + protestas,
  data = base,
  index = c("provincia", "año"),
  model = "within",
  effect = "twoways"
)

summary(modelo_cupos_fijo)

modelo_indiv <- plm(
  cupos ~ pred_nbi_suave + alineamiento + elecciones + diputados + senadores + protestas,
  data = base,
  index = c("provincia", "año"),
  model = "within",
  effect = "individual"
)
summary(modelo_indiv)

modelo_time_fe <- plm(
  cupos ~ senadores + pred_nbi_suave + alineamiento + elecciones + diputados + protestas,
  data = base,
  index = c("provincia", "año"),
  model = "within",
  effect = "time"
)
