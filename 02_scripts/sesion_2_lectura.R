### Leer archivos no estructurados
### Fecha de elaboración: 14 de octubre de 2021
### Fecha de actualización: 14 de octubre de 2021


# Bibliotecas -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pacman::p_load(
  tidyverse,
  readtext, # Lee textos en pdf o texto plano
  gutenbergr, # Acceso a la biblitoeca Gutenberg
  epubr # Leer epubs
)


# Datos -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# PDF

paramo <- readtext("01_datos/literatura/pedro_páramo.pdf") %>% 
  as_tibble()

class(paramo)

salvajes <- readtext("01_datos/literatura/detectives.pdf") %>% 
  as_tibble()

# Funciona con ms word!


# EPUB

fecal <- epub("01_datos/politica/Decisiones difíciles.epub") %>% 
  unnest(data)

#fecal$data
#fecal$text !!!

ver <- fecal %>% 
  select(section, text)


# PDF - Lectrua masiva

txts <- readtext("01_datos/literatura/*.pdf") # * traéme todo en pdf

class(txts)


