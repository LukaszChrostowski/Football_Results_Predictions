library(logitFD)
library(fda)
library(fda.usc)
library(tidyverse)
library(FDboost)

load("C:/Users/Kertoo/Desktop/Football_Results_Predictions/output/processed_data.Rdata")

invalid_cols <- sapply(proccessed_data, FUN = function(x) {sum(is.na(x))}) / NROW(proccessed_data) > .1
invalid_cols <- invalid_cols[invalid_cols] %>% names

proccessed_data <- proccessed_data %>% 
  select(!(all_of(invalid_cols)))

y <- proccessed_data$Wynik == "H"

cls_xpl <- colnames(proccessed_data)[-c(1:6, 28:30)]

df <- data.frame(
  y,
  "home" = proccessed_data$Gospodarz,
  "away" = proccessed_data$Gość
)

XX <- list()

for (k in cls_xpl) {
  XX[[k]] <- tibble(
    "1" = numeric(), 
    "2" = numeric(), 
    "3" = numeric(), 
    "4" = numeric(), 
    "5" = numeric()
  )
}

cond_season <- proccessed_data[, "Sezon"] != "2012/13" & proccessed_data[, "Sezon"] != "2022/23"

for (k in 1:nrow(proccessed_data)) {
  team1 <- proccessed_data[k, "Gospodarz"] %>% unlist
  time <- proccessed_data[k, "Data"] %>% unlist
  
  temp <- subset(proccessed_data, proccessed_data$Gość == team1)
  temp <- subset(temp, temp$Data < time)
  temp <- data.table::last(temp, n = 5)
  
  for (t in cls_xpl) {
    XX[[t]][k, ] <- data.table::last(c(0, 0, 0, 0, 0, temp[, t] %>% unlist), 5) %>% as.list()
  }
}

basis1 <- create.bspline.basis(rangeval=range(1:5))
basis2 <- create.bspline.basis(rangeval=range(1:5))

ldata <- append(sapply(XX, FUN = function(x) {
  structure(list(
    data = x[cond_season, ],
    argvals = 1:5,
    rangeval = c(1, 5)
  ), class = "fdata")
}), list(df[cond_season, ]))

ldata_test <- append(sapply(XX, FUN = function(x) {
  structure(list(
    data = x[proccessed_data[, "Sezon"] == "2022/23", ],
    argvals = 1:5,
    rangeval = c(1, 5)
  ), class = "fdata")
}), list(df[proccessed_data[, "Sezon"] == "2022/23", ]))

names(ldata)[length(ldata)] <- "df"

basis1 <- lapply(1:length(XX), FUN = function(x) create.bspline.basis(rangeval=range(1:5)))
names(basis1) <- names(XX)

basis2 <- lapply(1:length(XX), FUN = function(x) create.bspline.basis(rangeval=range(1:5)))
names(basis2) <- names(XX)

names(ldata)[1:(length(ldata)-1)] <- names(basis2) <- names(basis1) <- paste0("x", 1:(length(ldata)-1))

names(ldata_test)[1:(length(ldata)-1)] <- paste0("x", 1:(length(ldata)-1))
names(ldata_test)[length(ldata)] <- "df"

## replcaing na's

for (t in 1:(length(ldata)-1)) {
  ind <- which(is.na(ldata[[t]]$data), arr.ind=TRUE)
  mm <- rowMeans(ldata[[t]]$data,  na.rm = TRUE)[ind[, 1]]
  if (length(mm)) {
    for (k in 1:length(mm)) {
      ldata[[t]]$data[ind[k, 1], ind[k, 2]] <- mm[k]
    }
  }
}

modelH <- fregre.glm(
  formula = as.formula(paste("y~", paste(names(ldata)[-length(ldata)], collapse = "+"))),
  data = ldata,
  family = binomial(),
  basis.x = basis1,
  basis.b = basis2
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
