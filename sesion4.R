pacman::p_load(tidyverse, quanteda, tm, readxl)
require(quanteda.textstats)

prop <- read_excel("01_datos/tablas/baseDatosCandidatos.xls")

#Limpieza

prop_limpia <- prop %>% 
  filter(TIPO_CANDIDATO == "PROPIETARIO") %>% 
  mutate(propuesta_completa = paste(PROPUESTA_1, PROPUESTA_2, PROPUESTA_GENERO,
                             sep = " ")) %>% 
  select(PARTIDO_COALICION,  propuesta_completa) %>% 
  distinct(PARTIDO_COALICION, propuesta_completa) %>% 
  group_by(PARTIDO_COALICION) %>% 
  summarise(text = paste0(propuesta_completa, collapse = " ")) %>% 
  ungroup 


# Corpus quanteda


corp_plat <- corpus(prop_limpia, text_field = "text")
docnames(corp_plat) <- prop_limpia$PARTIDO_COALICION


toks <- tokens(corp_plat, remove_punct = TRUE)
#kwic(toks, pattern =  "trans*")

dfmat <- dfm(toks) %>% 
  dfm_remove(stopwords("es")) 

dfmat <- corp_plat %>% 
  tokens(remove_punct = TRUE) %>% 
  tokens_wordstem(language = "es") %>% 
  tokens_remove(stopwords("es")) %>% dfm()

topfeatures(dfmat)

tstat_lexdiv <- textstat_lexdiv(dfmat)
tail(tstat_lexdiv, 5)

#require(quanteda.textstats)