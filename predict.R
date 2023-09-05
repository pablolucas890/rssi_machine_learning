library(readr)
library(dplyr)

novo_dado <- data.frame(
  distancia = 0,
  rssi = 0.607843137254902,
  last_rssi_1 = 0.725490196078431,
  last_rssi_2 = 0.647058823529412,
  last_rssi_3 = 0.705882352941177,
  last_rssi_4 = 0.705882352941177,
  last_rssi_5 = 0.686274509803922,
  last_rssi_6 = 0.745098039215686,
  last_rssi_7 = 0.764705882352941,
  last_rssi_8 = 0.745098039215686,
  last_rssi_9 = 0.92156862745098,
  last_rssi_10 = 0.882352941176471,
  resultado = 0
)


train_sem_resposta <- read_csv("train_sem_resposta.csv")
train <- read_csv("train.csv")

prever_novo <- function(novo_dado) {
  # Fazer previsões com base no modelo K-NN
  previsoes <- class::knn(train = train_sem_resposta, test = novo_dado, cl = train$nivel_invasao, k = 3)
  return(previsoes)
}

previsoes = prever_novo(novo_dado)
print(previsoes)