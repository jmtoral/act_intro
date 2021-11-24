## Titulo: Vectorización de textos (GloVe)
## Fecha de creación: 2021-11-17
## Fecha de última actualización: 2021-11-17


# 0. Bibliotecas ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


library(tidyverse)
library(gutenbergr)
library(tidytext)
library(text2vec) # Biblioteca para la implementación de GloVe


# 1. Datos ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

william <- gutenberg_download(c(53207))

william2 <- gutenberg_download(c(56454)) 
Encoding(william2$text) <- "latin1" # cambio del encoding

ws <- william %>% 
  bind_rows(william2) #uno dos objetos de manera verticas

rm(william, william2) # borro objetos que estorban


# 2. Tokenizar ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ws_tokens <- william %>% 
  mutate(text = str_replace_all(text, "\\.--", " ")) %>% 
  mutate(text = str_replace_all(text, "[0-9]", " ")) %>%
  mutate(text = str_replace_all(text, "_|á", " ")) %>%
  unnest_tokens(palabra, text) %>% 
  filter(!palabra %in% c(tm::stopwords("es"), "dónde", "cómo")) %>% 
  filter(nchar(palabra) > 3)


# 3. IT (iToken) ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

lista_ws <- list(ws_tokens$palabra)

it <- itoken(lista_ws, progressbar = T)


# 4. Vocabulario ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Bigramas
#vocab <- create_vocabulary(it, ngram = c(1,2)) %>% 
 # prune_vocabulary(term_count_min = 5)

vocab <- create_vocabulary(it) %>% 
  prune_vocabulary(term_count_min = 5)

# 5. Vectorizar -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

vector <- vocab_vectorizer(vocab)


# 6. Matriz co-ocurrencia -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(123)

tcm_ws <- create_tcm(it, vector, skip_grams_window = 5)

tcm_ws[1]


# 7. GloVe ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

glove <- GlobalVectors$new(rank = 50, x_max = 10)

ws_main <- glove$fit_transform(tcm_ws, n_iter = 300, convergence_tol = 0.001)

ws_context <- glove$components

ws_vector <- ws_main + t(ws_context)


# 8. Operaciones con vectores ---------------------------------------------------------------------------------------------------------------------------------------------------------------------


# Amor

amor <- ws_vector["amor", , drop = FALSE]

cos_amor <- sim2(x = ws_vector,
                 y = amor, 
                 method = "cosine",
                 norm = "l2")

cos_amor[,1] %>% 
  sort(decreasing = T) %>% 
  head(30)# arrange = Rbase
  


# Romeo

romeo <- ws_vector["romeo", , drop = FALSE]

cos_romeo <- sim2(x = ws_vector,
                 y = romeo, 
                 method = "cosine",
                 norm = "l2")

cos_romeo[,1] %>% 
  sort(decreasing = T) %>% 
  head(30)# arrange = Rbase


# Romeo + Amor

romeo_amor <- ws_vector["romeo", , drop = FALSE] +
  ws_vector["amor", , drop = FALSE]

cos_romeo_amor <- sim2(x = ws_vector,
                  y = romeo_amor, 
                  method = "cosine",
                  norm = "l2")

cos_romeo_amor[,1] %>% 
  sort(decreasing = T) %>% 
  head(30)# arrange = Rbase


# Romeo - Amor

romeo_sin_amor <- ws_vector["romeo", , drop = FALSE] -
  ws_vector["amor", , drop = FALSE]

cos_romeo_sin_amor <- sim2(x = ws_vector,
                       y = romeo_sin_amor, 
                       method = "cosine",
                       norm = "l2")

cos_romeo_sin_amor[,1] %>% 
  sort(decreasing = T) %>% 
  head(30)# arrange = Rbase


# Rey vs Reina

reina = ws_vector["muerte", , drop = FALSE] 

cos_reina <- sim2(x = ws_vector,
                           y = reina, 
                           method = "cosine",
                           norm = "l2")

cos_reina[,1] %>% 
  sort(decreasing = T) %>% 
  head(30)# arrange = Rbase



# 9. Visualización ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

cos_reina[,1] %>% 
  sort(decreasing = T) %>% 
  head(30) %>% as.data.frame()
