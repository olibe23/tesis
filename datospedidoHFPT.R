library(readxl)
library(tidyr)
library(dplyr)

datospedido<- read_excel("pedido.xlsx", sheet = "Destinatarios y Montos")

programas <- as.character(unlist(datospedido[1, -1]))  # sin la columna provincia
anios <- names(datospedido)[-1]                        # nombres tipo "2016...2"
anios <- gsub("\\.\\.\\..*", "", anios)  

nuevos_nombres <- paste0(anios, "_", programas)
names(datospedido) <- c("provincia", nuevos_nombres)
datospedido <- datospedido[-1, ]

datospedido2 <- datospedido %>%
  pivot_longer(
    cols = -provincia,
    names_to = c("anio", "programa"),
    names_sep = "_",
    values_to = "titulares"
  ) %>%
  mutate(
    anio = as.integer(anio),
    titulares = as.numeric(titulares),
    provincia = toupper(trimws(provincia))
  )

datospedido2 <- datospedido2 %>%
  filter(!is.na(provincia), !is.na(anio), !is.na(programa)) %>%
  group_by(provincia, anio, programa) %>%
  summarise(titulares = sum(titulares, na.rm = TRUE), .groups = "drop")

datosHFPT <- datospedido2 %>%
  pivot_wider(
    names_from = programa,
    values_from = titulares,
    values_fill = 0
  ) %>%
  arrange(provincia, anio)

limpiar_provincias <- function(x) {
  x %>%
    tolower() %>%
    trimws() %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%  # quita tildes
    gsub("[^a-z_ ]", "", .) %>%   
    gsub(" ", "_", .)
}

datosHFPT <- datosHFPT %>%
  mutate(provincia = limpiar_provincias(provincia))

write_xlsx(datosHFPT, "HFPT.xlsx")

