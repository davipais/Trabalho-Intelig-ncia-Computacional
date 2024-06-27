library(readxl)
Base4 <- read_excel("C:/Users/flada/Downloads/Base4.xlsx", 
                      +     col_types = c("text", "text", "text", 
                                          +         "numeric", "numeric", "numeric", 
                                          +         "numeric", "numeric", "numeric", 
                                          +         "numeric", "numeric", "numeric", 
                                          +         "numeric", "numeric"))

library(dplyr)
#Coluna indicando 1 se há "!" e 0 se não
Base4 <- Base4 %>%
  mutate(exclamacao = ifelse(grepl("!", texto_unido), 1, 0))

#Coluna indicando 1 se há "não" e 0 se não
Base4 <- Base4 %>%
  mutate(negação = ifelse(grepl("não", texto_unido), 1, 0))


conta_palavras <- function(texto) {
  strsplit(texto, "\\s+")[[1]] %>% length()
}

# Adicionar uma nova coluna com o logaritmo do total de palavras
Base4 <- Base4 %>%
  mutate(total_palavras = sapply(texto_unido, conta_palavras),
         log_total_palavras = log(total_palavras))

#Pegando só se houver alguma emoção no texto

Base5= Base4[(Base4$soma>0),c(3:13,15,16,18)]
Base5 = as.data.frame(lapply(Base5, unlist))


# Proporção de dados para a base de treino
prop_treino <- 0.8

# Definir a semente para reprodutibilidade
set.seed(123)

# Criar um índice aleatório para dividir o data frame
indices <- sample(1:nrow(Base5), size = prop_treino * nrow(Base5))
Base5$log_total_palavras= as.numeric(Base5$log_total_palavras)
# Criar a base de treino e teste

Base5_treino <- Base5[indices, ]
Base5_teste <- Base5[-indices, ]

# Carregar a biblioteca necessária
library(nnet)
base_sem_resposta= Base5_treino[,-1]

# Ajustar o modelo de regressão logística multinomial
modelo <- multinom(sentFator ~ raiva+anticipação+desgosto+
                     medo+alegria+tristeza+surpresa+
                     confiança+negativo+positivo+exclamacao+
                     negação+log_total_palavras, data = Base5_treino)
# Resumo do modelo ajustado
summary(modelo)



common_columns
# Carregar as bibliotecas necessárias

library(caret)

# Suponha que 'Base5_treino' e 'Base5_teste' sejam os data frames de treino e teste
# E 'sentFator' seja a variável dependente

# Assegure que 'sentFator' esteja presente em ambos os data frames
# E que esteja corretamente definido como um fator
Base5_treino$sentFator <- as.factor(Base5_treino$sentFator)
Base5_teste$sentFator <- as.factor(Base5_teste$sentFator)

# Remover linhas com NAs (opção simples)
Base5_treino <- na.omit(Base5_treino)
Base5_teste <- na.omit(Base5_teste)


# Ajustar o modelo novamente e fazer previsões
previsoes <- predict(modelo, newdata= Base5_teste)


# Continuar com a avaliação do modelo
matriz_confusao <- table(Predicted = previsoes, Actual = Base5_teste$sentFator)
print(matriz_confusao)

acuracia <- sum(diag(matriz_confusao)) / sum(matriz_confusao)
print(paste("Acurácia:", round(acuracia, 4)))

resultado <- confusionMatrix(data = as.factor(previsoes), reference = as.factor(Base5_teste$sentFator))
print(resultado)
