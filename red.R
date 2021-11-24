library(tidyverse)
library(readtext)
library(tidytext)
library(igraph)
library(ggraph)

lc <- readtext("C:/Users/LENOVO/Downloads/La democracia no se construyó en un día.pdf") %>% 
  as_tibble()

bigramas_cuenta <- lc %>% 
  unnest_tokens(bigramas, text, token = "ngrams", n=2) %>% 
  filter(!str_detect(bigramas, "[0-9]")) %>% 
  filter(!str_detect(bigramas, "tal efecto|bitstream|repositorio|vianello|html|http|\\bsi\\b|\\bvez\\b|cit\\b|lorenzo córdova|ernesto núñez")) %>% 
  separate(bigramas, c("palabra1", "palabra2"), sep = " ") %>% 
  filter(!palabra1 %in% tm::stopwords("es")) %>% 
  filter(!palabra2 %in% tm::stopwords("es"))  %>% 
  filter(!is.na(palabra1)) %>% 
  count(palabra1, palabra2, sort =T)

bigrama_red <- bigramas_cuenta %>% 
  filter(n > 15) %>% 
  graph_from_data_frame()


a <- grid::arrow(type = "closed", length = unit(.15, "inches"))


ggraph(bigrama_red, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n),
                 show.legend = F,
                 arrow = a,
                 end_cap = circle(.07, "inches")) +
  geom_node_point(color = "lightblue", size=3) +
  geom_node_text(aes(label = name), 
                 vjust = 1, hjust = 1) +
  theme_void() +
  labs(title = "Grafo de los bigramas más frecuentes en 'La democracia no se construyó en un día (2021)'",
       subtitle = "de Lorenzo Córdova y Ernesto Núñez",
       caption = "@jmtoralc | Bigramas con al menos 20 apariciones.")

ggsave("cordova.jpg", width = 12, height = 12)
 