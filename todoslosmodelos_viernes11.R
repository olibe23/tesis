library(lme4)
library(dplyr)
library(readxl)
library(modelsummary)
library(tinytable)
library(stargazer)
library(car)
library(interactions)


setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Facu/Tesis/R JUNIO")
base <- read_excel("base_final.xlsx")
poblacion <- read_excel("poblacion.xlsx")

base <- full_join(base, poblacion, by = c("provincia", "año"))

base <- base %>% rename (yr = año,
                prov = provincia,
                nbi = pred_nbi_mice,
                pob = "Población provincial (proyecciones INDEC)"
                )
#Reescalo, logeo, unifico protestas
#primera base
base_mod <- base %>%
  mutate(
    prov = as.factor(prov),
    yr = as.numeric(yr),
    protestas = coalesce(protestas_acled, protestas_pemps),
    cupos_h = cupos / pob * 1000,
  ) %>% 
  mutate( cupos_h_log = log(cupos_h)) %>% 
  mutate(cupos_h_log = ifelse( is.infinite(cupos_h_log), NA, cupos_h_log ) ) %>%
  select(-protestas_acled, -protestas_pemps )

#segunda base
base_mod_1822 <- base_mod %>%
  filter (yr %in% 2018:2022)

base_mod_1822 <- base_mod_1822 %>%
  mutate(log_protestas = log(protestas + 1))
#===========
#   MODELOS
#===========
#Jerárquicos

#Sin protestas (base completa), sin diputados ni senadores por FE
mod_jer_1 <- lmer (
  cupos_h_log ~ nbi + alineamiento + elecciones +
    (1 | prov),
  data = base_mod
)

#Con protestas (base recortada), sin diputados ni senadores por FE
mod_jer_2 <- lmer (
  cupos_h_log ~ nbi + alineamiento + elecciones + protestas + 
    (1 | prov),
  data = base_mod_1822
)

#Con protestas (base recortada), sin diputados ni senadores por FE, con interacción por protestas en año electoral
mod_jer_3 <- lmer (
  cupos_h_log ~ nbi + alineamiento + elecciones + protestas + elecciones * protestas + 
    (1 | prov),
  data = base_mod_1822
)

#Con protestas (base recortada), sin diputados ni senadores por FE, sin elecciones por FE
mod_jer_4 <- lmer (
  cupos_h_log ~ nbi + alineamiento + protestas + 
    (1 | prov) + (1 | yr),
  data = base_mod_1822
)

#Sin protestas (base completa), sin elecciones por FE

mod_jer_5 <- lmer (
  cupos_h_log ~ nbi + alineamiento + diputados +
    (1 | yr),
  data = base_mod
)

#Sin protestas (base completa), sin elecciones por FE ni diputaods ni senaodres por FE
mod_jer_6 <- lmer (
  cupos_h_log ~ nbi + alineamiento +
    (1 | prov) + (1|yr),
  data = base_mod
)

mod_jer_7 <- lmer(
  cupos_h_log ~ nbi + alineamiento + elecciones + log_protestas + elecciones * log_protestas +
    (1 | prov) + (1 | yr),
  data = base_mod_1822
)

model_list1 <- list(
  "Completo - Sin protestas | Con FE prov (sin dip ni sen)" = mod_jer_1,
  "1822 - Con protestas | Con FE prov (sin dip ni sen) " = mod_jer_2,
  "1822 - Con protestas | Con FE prov (sin dip ni sen) interacción E*P" = mod_jer_3, 
  "1822 - Con protestas | Con FE prov y yr (sin dip ni sen ni elec" = mod_jer_4,
  "Completo - Sin protestas | Con FE yr (sin elec)" = mod_jer_5,
  "Completo - Sin protestas | Con FE yr y prov (sin elec, ni dip)" = mod_jer_6,
  "1822 - Con protestas | Con FE prov y yr (sin dip ni sen=" = mod_jer_7 )

stargazer(
  model_list1,
  type = "html",
  out = "modelosjerarquicos.html",
  title = "Modelos comparativos de regresión jerarquica",
  column.labels = names(model_list),
  dep.var.caption = "Variable dependiente: log(cupos por mil habitantes)",
  align = TRUE,
  no.space = TRUE,
  digits = 3)

#========================================

#Lineales
mod_1 <- lm (cupos_h_log ~ nbi + alineamiento + elecciones + diputados,
             data = base_mod)

mod_2 <- lm (cupos_h_log ~ nbi + alineamiento + elecciones + diputados + protestas,
             data = base_mod_1822)

mod_3 <- lm (cupos_h_log ~ nbi + alineamiento + elecciones + diputados + protestas + elecciones * protestas,
             data = base_mod_1822)


mod_4 <- lm (cupos_h_log ~ nbi + alineamiento + elecciones + diputados + log_protestas,
            data = base_mod_1822)

mod_5 <- lm (cupos_h_log ~ nbi + alineamiento + elecciones + diputados + elecciones*diputados,
             data = base_mod)

mod_6 <- lm (cupos_h_log ~ nbi + alineamiento + elecciones + diputados + elecciones*alineamiento,
             data = base_mod)

mod_7 <- lm (cupos_h_log ~ nbi + alineamiento + elecciones + log_protestas + diputados + elecciones*log_protestas,
             data = base_mod_1822)


stargazer (mod_1, mod_2, mod_3, mod_4, mod_5, mod_6,
           type = "text")

model_list2 <- list(
  "Completo" = mod_1,
  "1822" = mod_2,
  "1822 - interacción E*P" = mod_3, 
  "1822 - protestas log" = mod_4,
  "Completo - interaccion E*D" = mod_5,
  "Completo - interaccion E*A" = mod_6,
  "1822 - interaccion E*P" = mod_7)

stargazer(
  model_list2,
  type = "html",
  out = "modeloslineales.html",
  title = "Modelos comparativos de regresión lineal",
  column.labels = names(model_list),
  dep.var.caption = "Variable dependiente: log(cupos por mil habitantes)",
  align = TRUE,
  no.space = TRUE,
  digits = 3)



base_mod <- base_mod %>%
  mutate(gobierno = case_when(
    yr %in% 2010:2011 ~ "CFK1",
    yr %in% 2012:2015 ~ "CFK2",
    yr %in% 2016:2019 ~ "Macri",
    yr %in% 2020:2023 ~ "Alberto"
  ))

base_mod_1822 <- base_mod_1822 %>%
  mutate(gobierno = case_when(
    yr %in% 2010:2011 ~ "CFK1",
    yr %in% 2012:2015 ~ "CFK2",
    yr %in% 2016:2019 ~ "Macri",
    yr %in% 2020:2023 ~ "Alberto"
  ))

# Convertir en factor con orden cronológico explícito
base_mod_1822$gobierno <- factor(base_mod_1822$gobierno, levels = c("CFK1", "CFK2", "Macri", "Alberto"))
base_mod_1822$gobierno <- factor(base_mod_1822$gobierno, levels = c("CFK1", "CFK2", "Macri", "Alberto"))

mod_gob <- lmer(
  cupos_h_log ~ nbi + alineamiento + elecciones + gobierno + 
    (1 | prov),
  data = base_mod
)

mod_gob2 <- lmer(
  cupos_h_log ~ nbi + alineamiento + elecciones * gobierno + 
    (1 | prov),
  data = base_mod
)

mod_gob3 <- lmer(
  cupos_h_log ~ nbi + alineamiento + elecciones + 
    (1 | prov) + (1 | gobierno),
  data = base_mod
)

mod_gob4 <- lmer(
  cupos_h_log ~ nbi + alineamiento + elecciones + yr +
    (1 | prov),
  data = base_mod
)

stargazer(
  mod_gob, mod_gob2, mod_gob3, mod_gob4,
  type = "html",
  title = "Modelos jerárquicos con efectos por gobierno",
  column.labels = c("Modelo sin interacción", "Modelo con interacción E×Gobierno", "Efecto aleatorio por gobierno", "Control por año (yr)"),
  dep.var.caption = "Variable dependiente: log(cupos por mil habitantes)",
  align = TRUE,
  no.space = TRUE,
  digits = 3,
  out = "modelos_gobierno.html"
)


mod_gob_1822_1 <- lmer(
  cupos_h_log ~ nbi + alineamiento + elecciones + gobierno + protestas +
    (1 | prov),
  data = base_mod_1822
)

mod_gob_1822_2 <- lmer(
  cupos_h_log ~ nbi + alineamiento + elecciones * gobierno + protestas + 
    (1 | prov),
  data = base_mod_1822
)

mod_gob_1822_3 <- lmer(
  cupos_h_log ~ nbi + alineamiento + elecciones + protestas + 
    (1 | prov) + (1 | gobierno),
  data = base_mod_1822
)

mod_gob_1822_4 <- lmer(
  cupos_h_log ~ nbi + alineamiento + elecciones + yr + protestas + 
    (1 | prov),
  data = base_mod_1822
)

stargazer(
  mod_gob_1822_1, mod_gob_1822_2, mod_gob_1822_3, mod_gob_1822_4,
  type = "html",
  title = "Modelos jerárquicos con efectos por gobierno (18-22)",
  column.labels = c("Modelo sin interacción", "Modelo con interacción E×Gobierno", "Efecto aleatorio por gobierno", "Control por año (yr)"),
  dep.var.caption = "Variable dependiente: log(cupos por mil habitantes)",
  align = TRUE,
  no.space = TRUE,
  digits = 3,
  out = "modelos_gobierno1822.html"
)
