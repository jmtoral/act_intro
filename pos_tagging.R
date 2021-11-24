# Título: POS Tagging (Part-of-Speech)

# bibliotecas -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pacman::p_load(udpipe, tidyverse, tidytext)


# modelo UD ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

es <- udpipe_download_model(language = "spanish-ancora") # Descargar
udmodel <- udpipe_load_model(file = es$file_model)

# Encoding

txt <- "El día de hoy fue muy soleado y lo disfrutamos mucho." 

txt <- iconv(txt, from = "Latin1", to = "UTF-8")
Encoding(txt)

tag <- udpipe_annotate(udmodel, x = txt)
tag <- as_tibble(tag)

# Análisis estilométrico

pp <- readtext::readtext("01_datos/literatura/pedro_páramo.pdf") %>% 
  as_tibble() %>% 
  mutate(text = str_remove_all(text, "[«—»]"))

tag_pp <- udpipe_annotate(udmodel, x = pp$text) 

tag_pp <- tag_pp %>% 
  as_tibble()


tag_pp %>% 
  count(upos, sort = T)

tag_pp %>% 
  filter(upos == "NOUN") %>% 
  count(lemma, sort = T)

tag_pp %>% 
  filter(upos == "VERB") %>% 
  count(lemma, sort = T)
