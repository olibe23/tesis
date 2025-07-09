library(readr)
library(lubridate)
library(dplyr)
library(tidyr)

datosAT <- read_csv("/Users/oliviaberisso/Downloads/datosabiertosAT.csv")

titulares_anuales <- datosAT %>%
  mutate(periodo = as.Date(periodo),
         anio = as.numeric(format(periodo, "%Y"))) %>%
  filter(!is.na(anio), anio < 2018) %>%  # filtra NAs y descarta 2018
  group_by(provincia, anio) %>%
  summarise(titulares_totales = sum(titulares, na.rm = TRUE)) %>%
  arrange(provincia, anio)

datosEH <- read_csv("/Users/oliviaberisso/Downloads/datosabiertosEH.csv")
titulares_anualesEH <- datosEH %>%
  mutate(periodo = as.Date(periodo),
         anio = as.numeric(format(periodo, "%Y"))) %>%
  filter(!is.na(anio), anio < 2018) %>%  # filtra NAs y descarta 2018
  group_by(provincia, anio) %>%
  summarise(titulares_totales = sum(titulares, na.rm = TRUE)) %>%
  arrange(provincia, anio)


titulares_anuales <- titulares_anuales %>% rename(titulares_at = titulares_totales)
titulares_anualesEH <- titulares_anualesEH %>% rename(titulares_eh = titulares_totales)
titulares_merged <- full_join(titulares_anuales, titulares_anualesEH, by = c("anio", "provincia"))

anios <- 2009:2017
provincias <- unique(titulares_merged$provincia)
base_completa <- expand_grid(
  provincia = provincias,
  anio = anios
)

titulares_completo <- base_completa %>%
  left_join(titulares_merged, by = c("provincia", "anio"))

titulares_completo <- titulares_completo %>%
  mutate(across(
    c(titulares_at, titulares_eh),
    ~replace_na(.x, 0)
  ))
