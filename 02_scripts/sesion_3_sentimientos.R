### Análisis de sentimientos y repaso de tf-idf
### Fecha de elaboración: 26 de octubre de 2021
### Fecha de actualización: 26 de octubre de 2021


# Bibliotecas -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pacman::p_load(tidyverse,
               tidytext,
               tm, 
               syuzhet, #análisis de sentimiento
               rtweet,#Recolectar tweets
               esquisse) 


# 1. Datos ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

tmls <- get_timelines(c("lopezobrador_", "RicardoAnayaC"), 
                      n = 3200,
                      include_rts = FALSE)

tmls %>% 
  count(screen_name)


# 2. Análisis de frecuencias y de TFIDFs -------------------------------------------------------------------------------------------------------------------------------------------------------------

vacias <- c(tm::stopwords("es"), "http", "t.co", "ojalá",
            "aquél", "si", "https", "t")

palabras_frecuentes <- tmls %>% 
  unnest_tokens(palabras, text) %>% 
  select(palabras, screen_name) %>% 
  count(palabras, screen_name, sort=T) %>% 
  filter(!palabras %in% vacias)

top_tuits <- palabras_frecuentes %>% 
  group_by(screen_name) %>% 
  top_n(20) %>% 
  ungroup %>% 
  arrange(screen_name, -n)


## 2.1 TF_IDF-------------------------------------------------------------

tfidfs <- palabras_frecuentes %>% 
  bind_tf_idf(palabras, screen_name, n) %>% 
  arrange(-tf_idf) # para que el comando de top_n funcione correctamente


top_tf_idf <- tfidfs %>% 
  group_by(screen_name) %>% 
  top_n(20) %>% 
  ungroup %>% 
  arrange(screen_name, -tf_idf)
  

esquisser()

ggplot(top_tf_idf) +
  aes(x = reorder(palabras, tf_idf), 
      fill = screen_name, 
      y = tf_idf) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  coord_flip() +
  theme_minimal() +
  facet_wrap(vars(screen_name), scales = "free")



# 3. Análisis de sentimientos ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

 # lexicon en inglés
get_sentiments("bing")

get_sentiments("nrc")

get_sentiments("afinn") %>% 
  arrange(value)

get_sentiments("loughran")

# lexicon en español

get_nrc_sentiment("La niña está triste porque perdió su juguete", 
                  language = "spanish")


library(lubridate)

tmls_t <- tmls %>% 
  select(screen_name, created_at, text) %>% 
  mutate(dia = floor_date(created_at, unit = "day")) %>% 
  group_by(dia, screen_name) %>% 
  summarise(text = paste0(text, collapse = " ")) %>% 
  ungroup()

sentimientos_esp <- get_sentiment_dictionary('nrc', language = "spanish") %>% 
  select(word, sentiment)


sentimientos_tmls <- tmls_t %>% 
  unnest_tokens(word, text) %>% 
  left_join(sentimientos_esp)

## Sentimientos negativos y positivos

posneg <- sentimientos_tmls %>%
  filter(sentiment %in% c("positive", "negative")) %>% 
  mutate(valor = recode(sentiment,  "positive" = 1, 
                        "negative" = -1)) %>% 
  group_by(dia, screen_name) %>% 
  summarise(sent_total = sum(valor))

esquisser()

ggplot(posneg) +
  aes(x = dia, y = sent_total, colour = screen_name) +
  geom_line(size = 0.5) +
  scale_color_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(screen_name), ncol = 1) +
  geom_smooth(color = "black")
