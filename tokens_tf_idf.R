### Análisis de tokens
### Fecha de elaboración: 19 de octubre de 2021
### Fecha de actualización: 19 de octubre de 2021


# 0. Bibliotecas -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pacman::p_load(tidyverse,
               tidytext, # El paquete para anlizar textos 
               tm,
               janitor, # Útil para la limpeiza de bases de datos.
               readtext,
               epubr, wordcloud) 

library(wordcloud)

# 1. Datos ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

detectives <- readtext("01_datos/literatura/detectives.pdf") %>% 
  as_tibble()

cronopios <- readtext("01_datos/literatura/cronopios_famas.pdf") %>% 
  as_tibble()

# 2. Análisis -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# ¿Cuál es la palabra más frecuente en Detectives Salvajes?
# Ulises

# El gato está triste.

palabras_detectives <- detectives %>% 
  unnest_tokens(palabras, # Destino o variable nueva
                text) %>% # Origen. Variable del texto original
  count(palabras, sort = TRUE) %>% 
  filter(!palabras %in% tm::stopwords("es")) %>% 
  filter(!palabras %in% c("si", "ojalá", "aquél", "dice", "dijo", "dije",
                          "vez", "después", "entonces", "luego", "sólo"))

# cronopio

palabras_cronopios <- cronopios %>% 
  unnest_tokens(palabras, text) %>% 
  count(palabras, sort = TRUE) %>% 
  filter(!palabras %in% tm::stopwords("es")) %>% 
  filter(!palabras %in% c("si", "ojalá", "aquél", "dice", "dijo", "dije",
                          "vez", "después", "entonces", "luego", "sólo"))

#### Visualizar nube

## Gráficas no tidy

png("nube.png", width = 1000, height = 1000)

wordcloud(words = palabras_detectives$palabras,
          freq = palabras_detectives$n,
          max.words = 200,
          color = brewer.pal(8, "Spectral"),
          rot.per=0.35,
          random.order=FALSE)

dev.off()

### Gráfica de barras
## ggplot2

ggplot(
  data = palabras_detectives %>% # 1 Fuente
    top_n(10),
  aes(x= n, # 2. Mapping (estética)
      y= reorder(palabras, n)) #variable
) + #El signo de más une los comandos gráficos
  geom_col() +# 3. Geometría
  theme_minimal() +
  labs(title = "Las 10 palabras más frecuentes en Los Detectives Salvajes",
       subtitle = "Ordenadas por frecuencia",
       x = "Frecuencia",
       y = "Palabra", 
       caption = "Se retiraron las palabras vacías.")



## Bigramas

bigramas_detectives <- detectives %>% 
  unnest_tokens(bigrama, 
                text, 
                token = "ngrams", n = 2) %>% 
  separate(bigrama, c("palabra1", "palabra2"), sep = " ") %>% 
  filter(!palabra1 %in% tm::stopwords("es")) %>% 
  filter(!palabra2 %in% tm::stopwords("es")) %>% 
  count(palabra1, palabra2, sort = T) 

## Trigramas
trigramas_detectives <- detectives %>% 
  unnest_tokens(bigrama, 
                text, 
                token = "ngrams", n = 3) %>% 
  separate(bigrama, c("palabra1", "palabra2", "palabra3"), 
           sep = " ") %>% 
  filter(!palabra1 %in% tm::stopwords("es")) %>% 
  filter(!palabra2 %in% tm::stopwords("es")) %>% 
  filter(!palabra3 %in% tm::stopwords("es")) %>% 
  count(palabra1, palabra2, palabra3, sort = T) 


## Tf - IDF


parte1 <- palabras_detectives %>% 
  mutate(libro = "Los detectives salvajes")

parte2 <- palabras_cronopios %>% 
  mutate(libro = "Cronopios y Famas")

corpus <- parte1 %>% 
  bind_rows(parte2)

corpus_tf_idf <- corpus %>% 
  bind_tf_idf(palabras, libro, n) %>% 
  arrange(-tf_idf)#El orden siempre debe ser el mismo

top_10 <- corpus_tf_idf %>% 
  group_by(libro) %>% 
  top_n(10)


top_10 %>% 
  ggplot(
    aes(x = tf_idf, 
        y = reorder(palabras, tf_idf),
        fill= libro)
  ) +
  geom_col() +
  facet_wrap(~libro, scales = "free") +
  guides(fill = "none")



# Birgrmas Cronopios y Famas
bigramas_cronopios <- cronopios %>% 
  unnest_tokens(bigrama, 
                text, 
                token = "ngrams", n = 2) %>% 
  separate(bigrama, c("palabra1", "palabra2"), sep = " ") %>% 
  filter(!palabra1 %in% tm::stopwords("es")) %>% 
  filter(!palabra2 %in% tm::stopwords("es")) %>% 
  count(palabra1, palabra2, sort = T) 



## Tf - IDF Bigramas




parte1 <- bigramas_detectives %>% 
  mutate(libro = "Detectives Salvajes") %>% 
  unite("bigrama", palabra1, palabra2, sep = " ")

parte2 <-  bigramas_cronopios %>% 
  mutate(libro = "Cronopios y famas") %>% 
  unite("bigrama", palabra1, palabra2, sep = " ")


corpus <- parte1 %>% 
  bind_rows(parte2)



corpus_tf_idf <- corpus %>% 
  bind_tf_idf(bigrama, libro, n) %>% 
  arrange(-tf_idf)


top_10 <- corpus_tf_idf %>% 
  group_by(libro) %>% 
  top_n(10)


top_10 %>% 
  ggplot(
    aes(x = tf_idf, 
        y = reorder(bigrama, tf_idf),
        fill= libro)
  ) +
  geom_col() +
  facet_wrap(~libro, scales = "free") +
  guides(fill = "none")
