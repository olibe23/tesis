library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)


HFPT<- read_excel("HFPT.xlsx")
ATEH<- read_excel("ATEH.xlsx")

ATEH <- ATEH %>% rename (AT = titulares_at,
                         EH = titulares_eh)

HFPT <- HFPT %>%
  filter(!(anio %in% c(2016, 2017)))

HFPT <- HFPT %>% select (-AT, -EH)

cupos <- full_join(ATEH, HFPT, by = c("provincia", "anio"))

cupos <- cupos %>%
  arrange(provincia, anio)

cupos <- cupos %>%
  filter(provincia != "total_general")

cupos$total <- rowSums(cupos[, c("AT", "EH", "HF", "SSC", "PT")], na.rm = TRUE)
cupos <- cupos %>% filter(anio > 2009)

anios_faltantes <- setdiff(2010:2017, cupos %>% filter(provincia == "santa_cruz") %>% pull(anio))
santa_cruz_completar <- tibble(
  provincia = "santa_cruz",
  anio = anios_faltantes,
  AT = 0,
  EH = 0,
  HF = 0,
  SSC = 0,
  PT = 0
) %>%
  mutate(total = 0)
cupos <- bind_rows(cupos, santa_cruz_completar) %>%
  arrange(provincia, anio)

anios_faltantes <- setdiff(2010:2017, cupos %>% filter(provincia == "chubut") %>% pull(anio))
chubut_completar <- tibble(
  provincia = "chubut",
  anio = anios_faltantes,
  AT = 0,
  EH = 0,
  HF = 0,
  SSC = 0,
  PT = 0
) %>%
  mutate(total = 0)
cupos <- bind_rows(cupos, chubut_completar) %>%
  arrange(provincia, anio)

anios_faltantes <- setdiff(2010:2017, cupos %>% filter(provincia == "la_pampa") %>% pull(anio))
la_pampa_completar <- tibble(
  provincia = "la_pampa",
  anio = anios_faltantes,
  AT = 0,
  EH = 0,
  HF = 0,
  SSC = 0,
  PT = 0
) %>%
  mutate(total = 0)
cupos <- bind_rows(cupos, la_pampa_completar) %>%
  arrange(provincia, anio)

write_xlsx(cupos, "cupos.xlsx")

