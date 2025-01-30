library(caret)
library(neuralnet)
library(tidyverse)
load("output_data/processed_data_averages.Rdata")

df <- proccessed_data_averages
AAA <- apply(df[, sapply(df, is.numeric)], MARGIN = 2, FUN = function(x) {sum(is.na(x))}) / nrow(df)
AAA <- AAA[AAA < .1] %>% names
df <- df %>% mutate_if(is.numeric, ~replace_na(., mean(., na.rm = TRUE)))
df$Wynik <- ordered(df$Wynik)
# only numeric
formula <- Wynik ~ `Gole stracone Gospodarz` + `Gole stracone Gość` + `Posiadanie piłki Gospodarz` + `Posiadanie piłki Gospodarz` + `Sytuacje bramkowe Gospodarz` + `Strzały na bramkę Gospodarz` + `Strzały niecelne Gospodarz` + `Rzuty rożne Gospodarz` + `Spalone Gospodarz` + `Interwencje bramkarzy Gospodarz` + `Faule Gospodarz` + `Żółte kartki Gospodarz` + `Czerwone kartki Gospodarz` + `Posiadanie piłki Gość` + `Sytuacje bramkowe Gość` + `Strzały na bramkę Gość` + `Strzały niecelne Gość` + `Rzuty rożne Gość` + `Spalone Gość` + `Interwencje bramkarzy Gość` + `Faule Gość` + `Żółte kartki Gość` + `Czerwone kartki Gość` + `Gole Gospodarz` + `Gole Gość` + Sezon + Gospodarz + Gość
dd1 <- model.frame(formula, df, na.action = na.fail)
dd1$Gospodarz <- factor(dd1$Gospodarz)
dd1$Gość <- factor(dd1$Gość)
dd2 <- dd1[dd1$Sezon == "2022/23", ]
dd1 <- dd1[dd1$Sezon != "2022/23", ]
dd2 <- dd2[, !(grepl("Sezon", colnames(dd2)))]
dd1 <- dd1[, !(grepl("Sezon", colnames(dd1)))]
dd2 <- dd2[, !(grepl("Wynik", colnames(dd2)))]
dd1 <- dd1[, !(grepl("Wynik", colnames(dd1)))]

dd1y <- ((df %>% subset(!(df$Sezon %in% c("2022/23"))) %>% dplyr::select("Wynik"))$Wynik %>% as.numeric()) - 1
dd2y <- ((df %>% subset(df$Sezon == "2022/23") %>% dplyr::select("Wynik"))$Wynik %>% as.numeric()) - 1

dd <- rbind(dd1, dd2)
dd <- dd[, sapply(as.data.frame(dd), is.numeric)]

pr <- prcomp(dd)

(pr$sdev %>% cumsum()) / sum(pr$sdev)

barplot(pr$sdev, col = "navy", names = colnames(pr$x))

# choosing variables so that at least 97.5% of std.dev is explained

varNum <- which(((pr$sdev %>% cumsum()) / sum(pr$sdev)) > .975)[1]

dd <- pr$x[, 1:varNum]

dd <- cbind(dd, model.frame( ~ Gospodarz + Gość + LowerLeague_Away + LowerLeague_Home - 1, df))

dd2 <- dd[-(1:(dim(dd1)[1])), ]
dd1 <- dd[1:(dim(dd1)[1]), ]

dd1y <- ifelse(dd1y == 0, "A", ifelse(dd1y == 1, "D", "H"))
dd2y <- ifelse(dd2y == 0, "A", ifelse(dd1y == 1, "D", "H"))
dd1 <- cbind(dd1, "Wynik" = dd1y)
dd2 <- cbind(dd2, "Wynik" = dd2y)
fit <- train(
  Wynik ~ .,
  data = dd1,
  metric = "Accuracy",
  method = "evtree"
)
