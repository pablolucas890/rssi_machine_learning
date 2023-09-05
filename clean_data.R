library(dplyr)
library(readr)
library(stats)
library(class)
library(MLmetrics)

#--------------------------------------------------------------
#Passo 1 -  LIMPEZA DE DADOS
#--------------------------------------------------------------

# Pegando apenas os dados com baixa taxa de amostragem
data <- read_csv("dados.csv")
data <- data %>% filter(amostragem == 0.050)

# Remoção de outliers
outliers <- boxplot(data$rssi, plot = FALSE)$out
data_filtered <- data %>%
  filter(!rssi %in% outliers)

#--------------------------------------------------------------
#Passo 2 -  ANALISE DE DADOS
#--------------------------------------------------------------

# Quantidade de itens em cada nivel
qtd_nivel_0 <- sum(data$nivel_invasao == 0)
qtd_nivel_1 <- sum(data$nivel_invasao == 1)

# Dividindo a base
data_nivel_1 <- data %>%
  filter(nivel_invasao == 1)

data_nivel_0 <- data %>%
  filter(nivel_invasao == 0)

# Função para contar as sequências repetidas
count_repeated_sequences <- function(coluna, repeticao) {
  contador_total <- 0
  contador_sequencia <- 1
  
  for (i in 2:length(coluna)) {
    if (coluna[i] == coluna[i - 1]) {
      contador_sequencia <- contador_sequencia + 1
      if (contador_sequencia == repeticao) {
        contador_total <- contador_total + 1
      }
    } else {
      contador_sequencia <- 1
    }
  }
  
  return(contador_total)
}

repeticoes <- c(2, 3, 4, 5)
porcentagem_nivel_1 <- c()
porcentagem_nivel_0 <- c()

# Calculando a diferença de repetição entre os niveis de invasão
for (repeticao in repeticoes) {
  
  # Contar sequências repetidas
  contador_total_nivel_1 <- count_repeated_sequences(data_nivel_1$rssi, repeticao)
  contador_total_nivel_0 <- count_repeated_sequences(data_nivel_0$rssi, repeticao)
  
  # Calcula porcentagem e relação
  porcentagem_1 <- (contador_total_nivel_1 / qtd_nivel_1) * 100
  porcentagem_0 <- (contador_total_nivel_0 / qtd_nivel_0) * 100
  total <- porcentagem_1 + porcentagem_0
  relacao_1 = (porcentagem_1 * 100) / total
  relacao_0 = (porcentagem_0 * 100) / total
  porcentagem_nivel_1[repeticao - 1] <- relacao_1
  porcentagem_nivel_0[repeticao - 1] <- relacao_0
}

#--------------------------------------------------------------
#Passo 3 -  CRIAÇÃO DE ALGORRÍTMO TEMPORAL
#--------------------------------------------------------------

melhor_repetição <- which.max(porcentagem_nivel_0) + 1
cols <- c("last_rssi_1", "last_rssi_2", "last_rssi_3", "last_rssi_4", "last_rssi_5", "last_rssi_6",
          "last_rssi_7","last_rssi_8","last_rssi_9","last_rssi_10")

# Captação dos ultimos valores de RSSI para verificação de invasões
data_nivel_1 <- data_nivel_1 %>%
  mutate(last_rssi_1 = lag(rssi, n = 1),
         last_rssi_2 = lag(rssi, n = 2),
         last_rssi_3 = lag(rssi, n = 3),
         last_rssi_4 = lag(rssi, n = 4),
         last_rssi_5 = lag(rssi, n = 5),
         last_rssi_6 = lag(rssi, n = 6),
         last_rssi_7 = lag(rssi, n = 7),
         last_rssi_8 = lag(rssi, n = 8),
         last_rssi_9 = lag(rssi, n = 9),
         last_rssi_10 = lag(rssi, n = 10))
data_nivel_1 <- na.omit(data_nivel_1)
data_nivel_1$resultado <- apply(data_nivel_1[cols], 1, function(row) {
  if (sum(duplicated(row)) >= melhor_repetição) {
    return(1)
  } else {
    return(0)
  }
})
data_nivel_0 <- data_nivel_0 %>%
  mutate(last_rssi_1 = lag(rssi, n = 1),
         last_rssi_2 = lag(rssi, n = 2),
         last_rssi_3 = lag(rssi, n = 3),
         last_rssi_4 = lag(rssi, n = 4),
         last_rssi_5 = lag(rssi, n = 5),
         last_rssi_6 = lag(rssi, n = 6),
         last_rssi_7 = lag(rssi, n = 7),
         last_rssi_8 = lag(rssi, n = 8),
         last_rssi_9 = lag(rssi, n = 9),
         last_rssi_10 = lag(rssi, n = 10))
data_nivel_0 <- na.omit(data_nivel_0)
data_nivel_0$resultado <- apply(data_nivel_0[cols], 1, function(row) {
  if (sum(duplicated(row)) >= melhor_repetição) {
    return(1)
  } else {
    return(0)
  }
})

# Adicionar as linhas de data_nivel_1 ao final de data_nivel_0
data <- rbind(data_nivel_1, data_nivel_0)

# Criar o gráfico usando ggplot2
nivel_invasao_labels <- c("Sem Invasão", "Invasão")
resultado_labels <- c("Invasão", "Sem Invasão")

# Calcular porcentagens ajustadas
percentage_data <- data %>%
  group_by(nivel_invasao, resultado) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)
temp <- percentage_data[1, "count"]
percentage_data[1, "count"] <- percentage_data[2, "count"]
percentage_data[2, "count"] <- temp

#--------------------------------------------------------------
#Passo 4 -  EXPLORAÇÃO DE DADOS PARA ML
#--------------------------------------------------------------
# Organizando colunas
data <- data %>%
  select(tempo, amostragem, distancia, rssi, last_rssi_1, last_rssi_2, last_rssi_3,
         last_rssi_4, last_rssi_5, last_rssi_6, last_rssi_7, last_rssi_8, last_rssi_9,
         last_rssi_10, resultado, nivel_invasao)

# Normalização
data$distancia <- (data$distancia - min(data$distancia)) / (max(data$distancia) - min(data$distancia))
data$rssi <- (data$rssi - min(data$rssi)) / (max(data$rssi) - min(data$rssi))
last_rssi_columns <- paste0("last_rssi_", 1:10)
data <- data %>%
  mutate(across(all_of(last_rssi_columns), ~ (. - min(.)) / (max(.) - min(.))))

# Eliminação de atributos não necessários
data$amostragem <- NULL
data$tempo <- NULL

# Definir as proporções de teste e treinamento
prop_test <- 0.2
prop_train <- 0.8

# Calcular o tamanho das amostras
size_test <- round(nrow(data) * prop_test)
size_train <- nrow(data) - size_test

# Criar amostras aleatórias para teste e treinamento
indices <- sample(seq_len(nrow(data)), size = size_train + size_test)
indices_train <- indices[1:size_train]
indices_test <- indices[(size_train + 1):(size_train + size_test)]

# Dividir os dados
test <- data[indices_test, ]
train <- data[indices_train, ]

######################################### KNN ##############################################
melhor_k <- 1
melhor_acuracia <- 0
melhor_matriz <- NULL
k_knn <- 1


train_sem_resposta <- train[, -which(names(train) == "nivel_invasao")]
test_sem_resposta <- test[, -which(names(test) == "nivel_invasao")]

#Laço de repetição da variação do K no Knn, K varia entre numeros impares
for(j in 1:10){
  #Indução do Modelo
  model <- class::knn(train = train_sem_resposta, test = test_sem_resposta, cl = train$nivel_invasao, k = k_knn)
  predsVal <- as.numeric(as.character(model))
  
  #Criação da Matriz de Confusão
  cm1 <- MLmetrics::ConfusionMatrix(y_pred = predsVal, y_true = test$nivel_invasao)
  matriz = as.data.frame.matrix(cm1)
  
  #Cálculo da acurácia
  somatorio_diagonal_principal = sum(diag(cm1))
  somatorio_matriz = sum(cm1)
  acuracia_knn = somatorio_diagonal_principal/somatorio_matriz
  if(acuracia_knn > melhor_acuracia){
    melhor_acuracia = acuracia_knn
    melhor_matriz <- matriz
    melhor_k <- k_knn
  }
  k_knn  = k_knn + 2
}

print("Melhor matriz de confusão KNN:")
print(melhor_matriz)
print("Melhor acurácia KNN:")
print(melhor_acuracia)
print("Melhor K KNN:")
print(melhor_k)

write.csv(train, file = "train.csv", row.names = FALSE)
write.csv(train_sem_resposta, file = "train_sem_resposta.csv", row.names = FALSE)

#######################################################################################
