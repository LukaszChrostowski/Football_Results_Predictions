library(tidyverse)
library(caret)
load("output_data/processed_data.Rdata")
colnames(proccessed_data)[35] <- "Liczba sezonów Gość"

numOfLastPlayedGames <- 1 # change this to change the number of last games taken into consideration
dW <- dummyVars(formula = " ~ .", data = proccessed_data %>% select(Wynik))
dW <- data.frame(predict(dW, proccessed_data %>% select(Wynik)))
dW <- cbind(dW, dW)
dW <- dW[, -c(4, 6)]
colnames(dW) <- c("Porażka Gospodarz", "Remis Gospodarz", "Porażka Gość", "Remis Gość")
proccessed_data <- cbind(proccessed_data, dW)

proccessed_data$Data <- proccessed_data$Data %>% as.Date()
proccessed_data <- proccessed_data[order(proccessed_data$Data), ]

proccessed_data_averages <- data.frame(matrix(NA, ncol = ncol(proccessed_data) + 2, nrow = nrow(proccessed_data)))
colnames(proccessed_data_averages) <- c(colnames(proccessed_data), "Gole stracone Gość", "Gole stracone Gospodarz")
rownames(proccessed_data_averages) <- rownames(proccessed_data)
proccessed_data_averages <- as_tibble(proccessed_data_averages)
numStatNames <- colnames(proccessed_data_averages)[grepl(pattern = "Gość", x = colnames(proccessed_data_averages))][-c(1, 16)]
numStatNames <- str_replace(numStatNames, pattern = " Gość", replacement = "")
colRest <- colnames(proccessed_data)
colRest <- colRest[!(sapply(numStatNames, function (x) grepl(pattern = x, colRest)) %>% rowSums %>% as.logical)]

for (k in 1:nrow(proccessed_data_averages)) {
  team1 <- proccessed_data[k, "Gość"] %>% unlist
  team2 <- proccessed_data[k, "Gospodarz"] %>% unlist
  time <- proccessed_data[k, "Data"] %>% unlist
  proccessed_data_averages[k, colRest] <- proccessed_data[k, colRest]
  
  temp <- subset(proccessed_data, proccessed_data$Gość == team1 | proccessed_data$Gospodarz == team1)
  temp <- subset(temp, temp$Data < time)
  temp <- data.table::last(temp, n = numOfLastPlayedGames)
  auxMatrix <- data.frame(matrix(0, nrow = min(nrow(temp), numOfLastPlayedGames), ncol = length(numStatNames)))
  if (nrow(auxMatrix) > 0) {
    for (m in 1:nrow(auxMatrix)) {
      if (temp[m, "Gość"] == team1) {
        auxMatrix[m, ] <- temp[m, ] %>% dplyr::select(c(paste(numStatNames[-length(numStatNames)], "Gość"), "Gole Gospodarz"))
      } else {
        auxMatrix[m, ] <- temp[m, ] %>% dplyr::select(c(paste(numStatNames[-length(numStatNames)], "Gospodarz"), "Gole Gość"))
      }
    }
    proccessed_data_averages[k, paste(numStatNames, "Gość")] <- matrix(auxMatrix %>% colMeans(), ncol = length(numStatNames))
  }
  
  temp <- subset(proccessed_data, proccessed_data$Gość == team2 | proccessed_data$Gospodarz == team2)
  temp <- subset(temp, temp$Data < time)
  temp <- data.table::last(temp, n = numOfLastPlayedGames)
  auxMatrix <- data.frame(matrix(0, nrow = min(nrow(temp), numOfLastPlayedGames), ncol = length(numStatNames)))
  if (nrow(auxMatrix) > 0) {
    for (m in 1:nrow(auxMatrix)) {
      if (temp[m, "Gość"] == team2) {
        auxMatrix[m, ] <- temp[m, ] %>% dplyr::select(c(paste(numStatNames[-length(numStatNames)], "Gość"), "Gole Gospodarz"))
      } else {
        auxMatrix[m, ] <- temp[m, ] %>% dplyr::select(c(paste(numStatNames[-length(numStatNames)], "Gospodarz"), "Gole Gość"))
      }
    }
    proccessed_data_averages[k, paste(numStatNames, "Gospodarz")] <- matrix(auxMatrix %>% colMeans(), ncol = length(numStatNames))
  }
}

proccessed_data_averages[,c("Porażka Gospodarz", "Remis Gospodarz", "Porażka Gość", "Remis Gość")] <- proccessed_data_averages[,c("Porażka Gospodarz", "Remis Gospodarz", "Porażka Gość", "Remis Gość")] * numOfLastPlayedGames
proccessed_data_averages[, c("Porażka Gospodarz", "Porażka Gość", "Remis Gospodarz", "Remis Gość")] <- round(proccessed_data_averages[, c("Porażka Gospodarz", "Porażka Gość", "Remis Gospodarz", "Remis Gość")])
proccessed_data_averages$`Porażka Gospodarz` <- proccessed_data_averages$`Porażka Gospodarz` %>% as.factor()
proccessed_data_averages$`Remis Gospodarz` <- proccessed_data_averages$`Remis Gospodarz` %>% as.factor()
proccessed_data_averages$`Porażka Gość` <- proccessed_data_averages$`Porażka Gość` %>% as.factor()
proccessed_data_averages$`Remis Gość` <- proccessed_data_averages$`Remis Gość` %>% as.factor()

proccessed_data_averages_1 <- proccessed_data_averages
save(proccessed_data_averages_1, file = "output_data/processed_data_averages_1.Rdata")
