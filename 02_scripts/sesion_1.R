## Titulo: Introducción a la sintaxis del tidyverse
## Fecha de creación: 2021-10-12
## Fecha de última actualización: 2021-10-12


# 0. Bibliotecas ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#install.packages("tidyverse")
library(tidyverse)


# 1. Datos -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

flores <- iris 

### Crear nuestra propia tabla

nombres <-  c("José", "Pablo", "María")

edades <- c(24, 35, 46) #conjunto

aprobado <- c(TRUE, FALSE, TRUE)

txt <- c("El", "niño", "tiene", "frío")
txt2 <- c("En", "invierno", "hace", "fío")

tabla <- tibble(nombres, edades, aprobado)

# 2. Análisis --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## 5 funciones de dplyr
# 1. filtrar - filter 
# 2. seleccionar - select
# 3. mutar - mutate
# 4. ordenar - arrange
# 5. group_by()  + summarise()



agua_jamaica <- verbo2(verbo1(añadir(hervir(agua), infusión = "jamaica")))

agua_jamaica <- agua %>% 
  hervir() %>% 
  añadir(infusión = "jamaica") %>% 
  enfriar() %>% 
  servir()







## 2.1 Filtrar ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# ¿Qué flores tienen másde 1.5 mm de largo?

grandes_petalos <- flores %>% 
  filter(Petal.Width > 1.5) %>%  # Tiene que ser verdadero
  filter(Sepal.Width > 2.5)

pasaron <- tabla %>% 
  filter(aprobado == TRUE)  #==

no_pasaron <- tabla %>% 
  filter(!aprobado == TRUE)  #! negación

no_pasaron <- tabla %>% 
  filter(aprobado != TRUE)  #! negación



virginicas <- flores %>%
  filter(Species == "virginica")

osas_color <- flores %>% 
  filter(Species %in% c("setosa", "versicolor")) #  contiene

osas_color_regex <- flores %>% 
  filter(str_detect(Species, "osa|color")) # str_

jose_m <- tabla %>% 
  filter(str_detect(nombres, "(J|j)os(e|é)|(M|m)ar(i|í)a"))
