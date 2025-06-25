# Instalar y cargar librerías necesarias
install.packages(c("mice", "lme4", "dplyr", "readxl", "writexl", "ggplot2"))
library(mice)
library(lme4)
library(dplyr)
library(readxl)
library(writexl)
library(ggplot2)
# Cargar base
control <- read_excel("/Users/oliviaberisso/Library/Mobile Documents/com~apple~CloudDocs/Facu/Tesis/Controles/Finales/control.xlsx") %>%
  filter(ano < 2023) %>%
  mutate(provincia = as.factor(provincia),
         provincia_num = as.integer(provincia),
         ano = as.integer(ano))

# Declarar NBI como faltante excepto en años censales
control$nbi[!(control$ano %in% c(2010, 2022))] <- NA

# Configurar imputación
ini <- mice(control, maxit = 0)
meth <- ini$method
pred <- ini$predictorMatrix

# Definir método para NBI
meth["nbi"] <- "2l.lmer"
pred["nbi", ] <- 0
pred["nbi", c("pbg", "desempleo", "ano")] <- 1
pred["nbi", "provincia_num"] <- -2  # ahora sí existe

# Ejecutar imputación
imp <- mice(control, method = meth, predictorMatrix = pred, m = 5, seed = 123)

# Promediar imputaciones
completed_data <- complete(imp, "long") %>%
  group_by(.id, provincia, ano) %>%
  summarise(pred_nbi = mean(nbi, na.rm = TRUE), .groups = 'drop') %>%
  arrange(provincia, ano)

# Visualizar
ggplot(completed_data, aes(x = ano, y = pred_nbi, color = provincia)) +
  geom_line(size = 1) +
  labs(title = "NBI imputado con MICE (2l.lmer)",
       x = "Año",
       y = "NBI imputado") +
  theme_minimal()

# Exportar
write_xlsx(completed_data, "serie_nbi_imputada_mice.xlsx")
