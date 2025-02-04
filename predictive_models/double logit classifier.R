library(tidyverse)
load("output_data/processed_data_averages_1.Rdata")
df <- processed_data_averages_1

df$Wynik <- ordered(df$Wynik)
# only numeric
formula <- Wynik ~ possession_diff + shots_diff + attack_ratio +
  forma_diff + defensive_strength_home + defensive_strength_away +
  shooting_accuracy_home + shooting_accuracy_away + shot_efficiency_diff +
  discipline_ratio + LowerLeague_Home + LowerLeague_Away + Sezon

dd1 <- model.frame(formula, df %>% drop_na, na.action = na.fail)
dd2 <- dd1[dd1$Sezon == "2024/25", ]
dd1 <- dd1[dd1$Sezon != "2024/25", ]
dd2 <- dd2[, !(grepl("Wynik", colnames(dd2)))]
dd1 <- dd1[, !(grepl("Wynik", colnames(dd1)))]

dd1y <- ((df %>% subset(!(df$Sezon %in% c("2024/25"))) %>% dplyr::select("Wynik"))$Wynik %>% as.numeric()) - 1
dd2y <- ((df %>% subset(df$Sezon == "2024/25") %>% dplyr::select("Wynik"))$Wynik %>% as.numeric()) - 1

dd <- rbind(dd1, dd2)
dd <- dd[, sapply(as.data.frame(dd), is.numeric)]

pr <- prcomp(dd)

(pr$sdev %>% cumsum()) / sum(pr$sdev)

barplot(pr$sdev, col = "navy", names = colnames(pr$x))

# choosing variables so that at least 46% of std.dev is explained

varNum <- which(((pr$sdev %>% cumsum()) / sum(pr$sdev)) > .4605395)[1]

dd <- pr$x[, 1:varNum]

dd <- cbind(dd, model.frame(Wynik ~ Gospodarz + Gość + LowerLeague_Away + LowerLeague_Home - 1, df))

dd2 <- dd[-(1:(dim(dd1)[1])), ]
dd1 <- dd[1:(dim(dd1)[1]), ]

dd2$Wynik <- ifelse(dd2y == 2, 1, 0)
dd1$Wynik <- ifelse(dd1y == 2, 1, 0)

logitModel1 <- glm(formula = Wynik ~ ., family = binomial(), data = dd1)

dd2$Wynik <- ifelse(dd2y == 1, 1, 0)
dd1$Wynik <- ifelse(dd1y == 1, 1, 0)

logitModel2 <- glm(formula = Wynik ~ ., family = binomial(), data = dd1, control = glm.control(maxit = 1000))

# train error
mean(ifelse(predict(logitModel1) > 0, 2, ifelse(predict(logitModel2) > 0, 1, 0)) == dd1y)

# test error
mean(ifelse(predict(logitModel1, dd2) > 0, 2, ifelse(predict(logitModel2, dd2) > 0, 1, 0)) == dd2y)
