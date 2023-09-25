Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(readr)
library(dplyr)

melhor_repeticao <- 4
melhor_k <- 3
min_rssi <- -81
max_rssi <- -30

train_model <- read_csv("train_model.csv")
result <- read_csv("result.csv")$x

data <- read_csv("real_time.csv")
cols <- c("last_rssi_1", "last_rssi_2", "last_rssi_3", "last_rssi_4", "last_rssi_5", "last_rssi_6",
          "last_rssi_7","last_rssi_8","last_rssi_9","last_rssi_10")

data <- data %>%
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

data <- na.omit(data)

data$resultado <- apply(data[cols], 1, function(row) {
  if (sum(duplicated(row)) >= melhor_repeticao) {
    return(1)
  } else {
    return(0)
  }
})

data <- data %>%
  select(rssi, last_rssi_1, last_rssi_2, last_rssi_3, last_rssi_4, last_rssi_5,
         last_rssi_6, last_rssi_7, last_rssi_8, last_rssi_9, last_rssi_10, resultado)

data$rssi <- (data$rssi - min_rssi) / (max_rssi - min_rssi)
last_rssi_columns <- paste0("last_rssi_", 1:10)
data <- data %>%   mutate(across(all_of(last_rssi_columns), ~ (. - (min_rssi)) / ((max_rssi) - (min_rssi))))

prever_novo <- function(novo_dado) {
  previsoes <- class::knn(train = train_model, test = novo_dado, cl = result, k = melhor_k)
  return(previsoes)
}

previsoes = prever_novo(data[1,])

print("Valor Previsto:")
print(previsoes)