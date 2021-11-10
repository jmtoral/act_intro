# Título: Análisis de texto con matrices (quanteda)
# Fecha de elaboración : 29 de octubre de 2021
# Fecha de actualización : 29 de octubre de 2021


# 0. Bibliotecas -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#install.packages("quanteda")
#install.packages("quanteda.textstats")

library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(readxl)
library(igraph) #análisis de redes
library(ggraph) #grafo
library(janitor)


# 1. Datos ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

prop <- read_excel("01_datos/tablas/baseDatosCandidatos.xls")


# 2. Limpieza -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

prop_limpia <- prop %>% 
  janitor::clean_names() %>% 
  filter(tipo_candidato == "PROPIETARIO") %>% 
  mutate(prop_colapsada = str_c(propuesta_1,
                                propuesta_2,
                                propuesta_genero, sep = " ")) %>% 
  select(partido_coalicion, prop_colapsada) %>% 
  mutate(prop_colapsada = str_remove_all(prop_colapsada, "[[:punct:]]")) %>% #PERL REGEX
  distinct(partido_coalicion, prop_colapsada) %>% 
  filter(!is.na(prop_colapsada)) %>% 
  group_by(partido_coalicion) %>% 
  summarise(text = paste0(prop_colapsada, collapse = " ")) %>% 
  ungroup()


# 3. Construcción de un corpus --------------------------------------------------------------------------------------------------------------------------------------------------------------------

corpus_prop <- corpus(prop_limpia, text_field = "text")
docnames(corpus_prop) <- prop_limpia$partido_coalicion

tokens_prop <- tokens(corpus_prop, remove_punct = T, remove_numbers = T) 


# 4. Matriz DFM (Document - Feature Matrix) -------------------------------------------------------------------------------------------------------------------------------------------------------

dfmat <- tokens_prop %>% 
  tokens_remove(stopwords("es")) %>% # Similar a filter
 # tokens_wordstem(language = "es")  %>% #algoritmo de Porter
  dfm() 

topfeatures(dfmat)

lexdiv <- textstat_lexdiv(dfmat)


# 5. Similaridad de textos ------------------------------------------------------------------------------------------------------------------------------------------------------------------------

tstat <- textstat_simil(dfmat, method = "cosine", margin = "documents") %>% 
  as_tibble()

tstat.j <- textstat_simil(dfmat, method = "jaccard") %>% # La más frecuente
  as_tibble()

tstat.r <- textstat_simil(dfmat, method = "correlation") %>% 
  as_tibble()

tstat.sm <- textstat_simil(dfmat, method = "simple matching") %>% 
  as_tibble()


tstat <- textstat_simil(dfmat, dfmat[,c("seguridad", "salud", "social")],  #matriz[documento, término]
                        method = "cosine", 
                        margin = "features") %>% 
  as_tibble()



# 6. Distancia ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

tstat_dist <- as.dist(textstat_dist(dfmat))

cluster <- hclust(tstat_dist)

plot(cluster)
