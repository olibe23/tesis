library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)


HFPT<- read_excel("HFPT.xlsx")
ATEH<- read_excel("ATEH.xlsx")

ateh_solapado <- ATEH %>%
  filter(anio %in% c(2016, 2017)) %>%
  arrange(provincia, anio)

hfpt_solapado <- HFPT %>%
  filter(anio %in% c(2016, 2017)) %>%
  select(provincia, anio, at = AT, eh = EH) %>%
  arrange(provincia, anio)

# 2. Unir ambas bases por provincia y año
comparacion <- ateh_solapado %>%
  full_join(hfpt_solapado, by = c("provincia", "anio"), suffix = c("_ateh", "_hfpt"))

# 3. Crear columnas de diferencia
comparacion <- comparacion %>%
  mutate(
    diff_at = titulares_at - at,
    diff_eh = titulares_eh - eh
  )

comparacion_diferencias <- comparacion %>%
  filter(diff_at != 0 | diff_eh != 0)
