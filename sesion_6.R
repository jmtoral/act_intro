# Título: LDA (Distribución Latente de Dirichlet) y Seeded LDA
# Fecha de elaboración : 9 de noviembre de 2021
# Fecha de actualización : 9 de noviembre de 2021


# 0. Bibliotecas ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(quanteda)
library(quanteda.textmodels)
library(seededlda)
library(topicmodels)
library(SnowballC)



# 1. Datos -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

getStemLanguages()

noticias <- read_excel("01_datos/train.xlsx") %>% 
  filter(Category == "Fake") %>% 
  select(Headline, Text) %>% 
  mutate(Text = str_replace_all(Text, "\\*NUMBER\\*", "")) %>% 
  mutate(Text = str_replace_all(Text, "\\.|\\-", "")) %>% 
  mutate(Text = str_squish(Text)) 
  
corpus_falsas <- corpus(noticias, text_field = "Text")

docnames(corpus_falsas) <- noticias$Headline




# 2. Preprocesamiento -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

token_falsas <- tokens(corpus_falsas,
                       remove_punct = T,
                       remove_numbers = T,
                       remove_symbols = T) %>% 
  tokens_tolower() %>% 
  tokens_remove(pattern = c(stopwords("es"), "si","van","ser", 
                            "tras", "solo", "cada", "y"))


# 3. Matriz Documento Término  --------------------------------------------------------------------------------------------------------------------------------------------------------------------

# SnowballC

dfm_falsas <- dfm(token_falsas) %>% 
  #dfm_wordstem(language = "spanish") %>% # Stematización
  dfm_trim(min_termfreq = 0.9, termfreq_type = "quantile",
           max_docfreq = 0.3, docfreq_type = "prop")


# 4. Modelos LDA ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(123)

model1 <- textmodel_lda(dfm_falsas, k = 5)

seededlda::terms(model1, 20)

dfm_falsas$topics <- seededlda::topics(model1)

clasificacion_final <- as_tibble(model1$theta, rownames = "doc_id") 


# 5. Modelo LDA sembrado (seededLDA) --------------------------------------------------------------------------------------------------------------------------------------------------------------

dic <- dictionary(
  list(
    amlo = c("amlo", "lópez", "morena","obrador", "transformación", "austeridad"),
    meade = c("meade", "pri", "salinas", "peña"),
    trump = c("trump", "estados", "unidos", "blanca", "washington")
  )
)

model2 <- textmodel_seededlda(x = dfm_falsas,
                              dictionary = dic,
                              residual = T)

seededlda::terms(model2, 20)

clasificacion_m2 <- as_tibble(model2$theta, rownames = "doc_id") 



# 6. topicmodels ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

lda_tp <- LDA(dfm_falsas, k=5) #topicmodels: término

topicos <- tidytext::tidy(lda_tp, matrix = "beta")

rep <- topicos %>% 
  arrange(-beta) %>% 
  group_by(topic) %>% 
  top_n(5)




