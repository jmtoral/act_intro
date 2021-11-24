# Logistic Regression (OLS)
# 18 de noviembre de 2021


# Bibliotecas -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(rsample)
library(glmnet)
library(gutenbergr)
library(yardstick)


# Datos -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

libros <- c(36, 1342)

textos <- gutenberg_download(libros, meta_fields = "title") %>% 
  mutate(document = row_number())


tokens_libros <- textos %>% 
  unnest_tokens(palabras, text) %>% 
  group_by(palabras) %>% 
  filter(n() > 10) %>% 
  ungroup


# Muestra de entrenamiento y de prueba ------------------------------------------------------------------------------------------------------------------------------------------------------------

particion <- textos %>% 
  select(document) %>% 
  initial_split() # 75% de los enunciados son de entremiento y 15% son de prueba

entrenamiento <- training(particion)
prueba <- testing(particion)


# Matriz de entrenamiento ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Sparse matrix

matriz_train <- tokens_libros %>% 
  count(document, palabras) %>% 
  filter(!palabras %in% tm::stopwords("en")) %>% 
  inner_join(entrenamiento) %>% 
  cast_sparse(document, palabras, n)



# Tratamiento preregression -----------------------------------------------------------------------------------------------------------------------------------------------------------------------

palabras_obs <- as.integer(rownames(matriz_train))

libros_completo <- data_frame(document = palabras_obs) %>% 
  left_join(textos %>% 
              select(document, title))


# Estimar el modelo -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

set.seed(123)

is_jane <- libros_completo$title == "Pride and Prejudice"

modelo1 <- cv.glmnet(matriz_train, #X... o palabras 
                     is_jane, # Y... predictor
                     family = "binomial", 
                     keep = T) 

plot(modelo1)
plot(modelo1$glmnet.fit)

coefs <- modelo1$glmnet.fit %>% 
  tidy() %>% 
  filter(lambda == modelo1$lambda.1se)


# Gráfica No. 1 -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

top10 <- coefs %>% 
  group_by(estimate > 0) %>% 
  top_n(10, abs(estimate)) %>% 
  ungroup %>% 
  arrange(-estimate)

ggplot(
  data = top10,
  mapping= aes(x = reorder(term, estimate), 
               y = estimate, fill = estimate > 0)
) +
  geom_col(show.legend = F) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Coeficientes que cambian la probabilidad de clasificación",
       subtitle = "La palabra marciano importa mucho en la obra de Wells",
       x = "")


# Probabilidades por enunciado --------------------------------------------------------------------------------------------------------------------------------------------------------------------

intercepto <- coefs %>% 
  filter(term == "(Intercept)") %>% 
  pull(estimate)



clasificaciones <- tokens_libros %>% 
  inner_join(prueba) %>% 
  inner_join(coefs, by =c("palabras" = "term")) %>% 
  group_by(document) %>% 
  summarise(score = sum(estimate)) %>% 
  mutate(probabilidad = plogis(intercepto + score))


# ROC ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library(yardstick)

clases <- clasificaciones %>% 
  left_join(textos %>% 
              select(title, document), by = "document") %>% 
  mutate(title = as.factor(title))

clases %>% 
  roc_curve(title, probabilidad) %>% 
  ggplot(aes( x = 1-specificity,
              y = sensitivity)) +
  geom_line(size = 1.5, color = "navyblue") +
  geom_abline(lty = 2) +
  theme_minimal() +
  labs(title = "Curva ROC para la clasificación de textos utilizando una regresión regularizada",
       subtitle = "Predicción de si el texto fue escrito por Jane Austen o por HG Wells.")

clases %>% 
  roc_auc(title, probabilidad)


# Matriz de confusión -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------


#mutate(variable = case_when(Verdadero ~ "1",
#                            Falso ~ "0"  ))

matriz_confusion <- clases %>% 
  mutate(
    prediccion = case_when(
      probabilidad > 0.5 ~ "Pride and Prejudice",
      probabilidad <= 0.5 ~ "The War of the Worlds"
    )
  ) %>% 
  mutate(prediccion = as.factor(prediccion)) %>% 
  conf_mat(title, prediccion)


# Errores de clasificación ------------------------------------------------------------------------------------------------------------------------------------------------------------------------

errores <- clases %>% 
  filter(probabilidad > 0.8) %>% 
  filter(title == "The War of the Worlds") %>% 
  arrange(-probabilidad) %>% 
  inner_join(textos %>% 
               select(document, text)) %>% 
  select(probabilidad, text)


errores2 <- clases %>% 
  filter(probabilidad < 0.3) %>% 
  filter(title == "Pride and Prejudice") %>% 
  arrange(-probabilidad) %>% 
  inner_join(textos %>% 
               select(document, text)) %>% 
  select(probabilidad, text)


















