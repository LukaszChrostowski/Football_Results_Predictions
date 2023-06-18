library(logitFD)
library(fda)
library(fda.usc)
library(tidyverse)
library(FDboost)

load("C:/Users/Kertoo/Desktop/Football_Results_Predictions/output/processed_data.Rdata")

invalid_cols <- sapply(proccessed_data, FUN = function(x) {sum(is.na(x))}) / NROW(proccessed_data) > .1
invalid_cols <- invalid_cols[invalid_cols] %>% names

proccessed_data <- proccessed_data %>% 
  subset(Sezon != "2012/13") %>% 
  select(!(all_of(invalid_cols)))

y <- proccessed_data$Wynik == "H"

df <- data.frame(
  y,
  "home" = proccessed_data$Gospodarz,
  "away" = proccessed_data$Gość
)

x <- data.frame(
  "1" = numeric(), 
  "2" = numeric(), 
  "3" = numeric(), 
  "4" = numeric(), 
  "5" = numeric()
)

for (k in 1:nrow(proccessed_data)) {
  team1 <- proccessed_data[k, "Gospodarz"] %>% unlist
  time <- proccessed_data[k, "Data"] %>% unlist
  
  temp <- subset(proccessed_data, proccessed_data$Gość == team1)
  temp <- subset(temp, temp$Data < time)
  temp <- data.table::last(temp, n = 5)
  
  x[k, ] <- data.table::last(c(0, 0, 0, 0, 0, temp$`Posiadanie piłki Gospodarz`), 5)
}

basis1 <- create.bspline.basis(rangeval=range(1:5))
basis2 <- create.bspline.basis(rangeval=range(1:5))

modelH <- fregre.glm(
  y ~ home + away + x,
  data = list("df" = df[-(1:72), ], "x" = structure(list(
    data = x[-(1:72), ],
    argvals = 1:5,
    rangeval = c(1, 5)
  ), class = "fdata")),
  family = binomial(),
  basis.x = list("x" = basis1),
  basis.b = list("x" = basis2)
)

y <- proccessed_data$Wynik == "A"

df <- data.frame(
  y,
  "home" = proccessed_data$Gospodarz,
  "away" = proccessed_data$Gość
)

x <- data.frame(
  "1" = numeric(), 
  "2" = numeric(), 
  "3" = numeric(), 
  "4" = numeric(), 
  "5" = numeric()
)

for (k in 1:nrow(proccessed_data)) {
  team1 <- proccessed_data[k, "Gość"] %>% unlist
  time <- proccessed_data[k, "Data"] %>% unlist
  
  temp <- subset(proccessed_data, proccessed_data$Gość == team1)
  temp <- subset(temp, temp$Data < time)
  temp <- data.table::last(temp, n = 5)
  
  x[k, ] <- data.table::last(c(0, 0, 0, 0, 0, temp$`Posiadanie piłki Gość`), 5)
}

modelA <- fregre.glm(
  y ~ home + away + x,
  data = list("df" = df[-(1:72), ], "x" = structure(list(
    data = x[-(1:72), ],
    argvals = 1:5,
    rangeval = c(1, 5)
  ), class = "fdata")),
  family = binomial(),
  basis.x = list("x" = basis1),
  basis.b = list("x" = basis2)
)
