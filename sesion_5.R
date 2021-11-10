# Título: Raspado (Scraping) y visualización
# Fecha de elaboración : 2 de noviuembre de 2021
# Fecha de actualización : 2 de noviembre de 2021


# 0. Bibliotecas ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(rvest) # Scraping (harvest)
library(igraph) # Análisis de redes
library(ggraph) # Grafos
library(tidytext)


# 1. Datos ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


# 1.1 Scrapeando una sóla página ------------------------------------------------------------------------------------------------------------------------------------------------------------------

url <- "https://lopezobrador.org.mx/2021/10/28/version-estenografica-de-la-conferencia-de-prensa-matutina-del-presidente-andres-manuel-lopez-obrador-633/"

txt_mananera <- url %>% 
  read_html() %>% # Leer la página web
  html_nodes(".p1") %>% 
  html_text() %>% 
  as_tibble()

## 1.2 Screpeo en bucle --------------------------------------

### A. Vamos conseguir las direcciones de todos los artículos -------------------

url <- "https://lopezobrador.org.mx/page/1/"

links <- url %>% 
  read_html() %>% 
  html_nodes(".entry-post .entry-title a") %>% 
  html_attr("href") %>% 
  as_tibble() %>% 
  filter(str_detect(value, "estenografica"))

# Bucle (loop)

textos <- lapply(links$value, function(i){
  read_html(i) %>% 
    html_nodes(".p1") %>% 
    html_text() %>% 
    as_tibble() %>% 
    mutate(fuente = i)
})


#titulos<- lapply(links$value, function(i){
 # read_html(i) %>% 
  #  html_nodes(".entry-title") %>% 
   # html_text() %>% 
    #as_tibble() %>% 
    #mutate(fuente = i)
#})

textos <- bind_rows(textos)

#titulos <- bind_rows(titulos) %>% 
 # rename(titulo = value)

#mananeras <- textos %>% 
 # left_join(titulos)



### B. Vamos conseguir las direcciones de varias páginas -------------------

url_loop <- "https://lopezobrador.org.mx/page/" 

links_paginas <- paste0(url_loop, 1:10, "/")

links_est <- lapply(links_paginas, function(x){
  x %>% 
    read_html() %>% 
    html_nodes(".entry-post .entry-title a") %>% 
    html_attr("href") %>% 
    as_tibble() %>% 
    filter(str_detect(value, "estenografica"))
}) %>% 
  bind_rows()


textos <- lapply(links_est$value, function(i){
  read_html(i) %>% 
    html_nodes("p") %>% 
    html_text() %>% 
    as_tibble() %>% 
    mutate(fuente = i)
})


textos.df <- textos %>% bind_rows()



# 2. Análisis de redes -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


bigramas_cuenta <- textos.df %>% 
  unnest_tokens(bigramas, value, token = "ngrams", n=2) %>% 
  filter(!str_detect(bigramas, "presidente|andrés|manuel|lópez|obrador|señor|ver|si")) %>% 
  filter(!str_detect(bigramas, "[0-9]")) %>% 
  separate(bigramas, c("palabra1", "palabra2"), sep = " ") %>% 
  filter(!palabra1 %in% tm::stopwords("es")) %>% 
  filter(!palabra2 %in% tm::stopwords("es"))  %>% 
  filter(!is.na(palabra1)) %>% 
  count(palabra1, palabra2, sort =T)

bigrama_red <- bigramas_cuenta %>% 
  filter(n > 20) %>% 
  graph_from_data_frame()


set.seed(123)

ggraph(bigrama_red, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), 
                 vjust = 1, hjust = 1) +
  theme_void()


### Cosméticos

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))


ggraph(bigrama_red, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n),
                 show.legend = F,
                 arrow = a,
                 end_cap = circle(.07, "inches")) +
  geom_node_point(color = "lightblue", size=3) +
  geom_node_text(aes(label = name), 
                 vjust = 1, hjust = 1) +
  theme_void()




