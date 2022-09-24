library(tidyverse)
load("output/processed_data.Rdata")

proccessed_data$Data <- proccessed_data$Data %>% as.Date()
proccessed_data <- proccessed_data[order(proccessed_data$Data), ]

proccessed_data_averages <- data.frame(matrix(ncol = ncol(proccessed_data), nrow = nrow(proccessed_data)))
colnames(proccessed_data_averages) <- colnames(proccessed_data)
rownames(proccessed_data_averages) <- rownames(proccessed_data)
proccessed_data_averages <- as_tibble(proccessed_data_averages)
colAway <- colnames(proccessed_data)[grepl(pattern = "Gość", x = colnames(proccessed_data))][-1]
colHome <- colnames(proccessed_data)[grepl(pattern = "Gospodarz", x = colnames(proccessed_data))][-1]
colRest <- colnames(proccessed_data)
colRest <- colRest[!(colRest %in% colAway)]
colRest <- colRest[!(colRest %in% colHome)]

for (k in 1:nrow(proccessed_data_averages)) {
  team1 <- proccessed_data[k, "Gość"] %>% unlist
  team2 <- proccessed_data[k, "Gospodarz"] %>% unlist
  time <- proccessed_data[k, "Data"] %>% unlist
  proccessed_data_averages[k, colRest] <- proccessed_data[k, colRest]
  
  temp <- subset(proccessed_data, proccessed_data$Gość == team1 | proccessed_data$Gospodarz == team1)
  temp <- subset(temp, temp$Data < time, select = colAway)
  proccessed_data_averages[k, colAway] <- colMeans(last(temp, n = 5), na.rm = TRUE) %>% as_tibble %>% t
  
  temp <- subset(proccessed_data, proccessed_data$Gość == team2 | proccessed_data$Gospodarz == team2)
  temp <- subset(temp, temp$Data < time, select = colHome)
  proccessed_data_averages[k, colHome] <- colMeans(last(temp, n = 5), na.rm = TRUE) %>% as_tibble %>% t
}

save(proccessed_data_averages, file = "output/processed_data_averages.Rdata")
