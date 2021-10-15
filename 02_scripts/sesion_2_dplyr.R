## Introducción al uso del tidyverse p/ análisis de texto pt.2
## Fecha elaboración: 14 de octubre de 2021
## Fecha de actualización: 14 de octubre de 2021



# 0. Bibliotecas ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pacman::p_load(tidyverse,
               gutenbergr,
               readxl)

# 1. Datos ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

propuesta <- read_excel("01_datos/tablas/baseDatosCandidatos.xls")

glimpse(propuesta)

summary(propuesta)

# 2. Análisis -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



## 2.1 Filtrar texto -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## ¿Cuántos y cuáles candidatos mencionan alguna propuesta referente a la comunidad trans?

trans <- propuesta %>%  # Ctrl + Shift  + M
  filter(str_detect(PROPUESTA_GENERO, "\\b(t|T)rans\\b|\\bTRANS\\b")) %>% #Expresión regular
  select(PARTIDO_COALICION, ENTIDAD, PROPUESTA_GENERO)

trans2 <- propuesta %>%  # Ctrl + Shift  + M
  filter(str_detect(PROPUESTA_GENERO, "\\b(t|T)rans\\b|\\bTRANS\\b")) %>% #Expresión regular
  select(PARTIDO_COALICION, ENTIDAD, 
         starts_with("PROPUESTA"))

trans2 <- propuesta %>%  # Ctrl + Shift  + M
  filter(str_detect(PROPUESTA_GENERO, "\\b(t|T)rans\\b|\\bTRANS\\b")) %>% #Expresión regular
  select(PARTIDO_COALICION, ENTIDAD, 
         contains("PROPUESTA"))

# Eutanasia

# ¿Qué candidates mencionario "eutanasia" en sus propuestas?

eutanasia <- propuesta %>% 
  select(PARTIDO_COALICION, ENTIDAD, ESCOLARIDAD,
         contains("PROPUESTA")) %>% 
  mutate(propuestas = str_c(PROPUESTA_1, PROPUESTA_2, PROPUESTA_GENERO, sep = " ")) %>%  # 3 variables de texto se conviertan en 1
  filter(str_detect(propuestas, "\\b(e|E)utanasia\\b|\\bEUTANASIA\\b")) %>% 
  select(-propuestas)

eutanasia_minus <- propuesta %>% 
  select(PARTIDO_COALICION, ENTIDAD, ESCOLARIDAD,
         contains("PROPUESTA")) %>% 
  mutate(propuestas = str_c(PROPUESTA_1, PROPUESTA_2, PROPUESTA_GENERO, sep = " ")) %>% 
  mutate(propuestas = str_to_lower(propuestas)) %>% 
  filter(str_detect(propuestas, "eutanasia")) %>% 
  select(-propuestas)

## Group_by + summarise

plat_pp <- propuesta %>% 
  mutate(propuestas = str_c(PROPUESTA_1, PROPUESTA_2, PROPUESTA_GENERO, sep = " ")) %>%
  group_by(PARTIDO_COALICION) %>% 
  summarise(plataforma = paste0(propuestas, collapse = " ")) %>% 
  arrange(PARTIDO_COALICION)










