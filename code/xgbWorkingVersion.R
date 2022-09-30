library(xgboost)
library(caret)

# 1 ####
df <- proccessed_data_averages
AAA <- apply(df[, sapply(df, is.numeric)], MARGIN = 2, FUN = function(x) {sum(is.na(x))}) / nrow(df)
AAA <- AAA[AAA < .1] %>% names
df <- df %>% mutate_if(is.numeric, ~replace_na(.,mean(., na.rm = TRUE)))
df$Wynik <- ordered(df$Wynik)

formula <- Wynik ~ `Posiadanie piłki Gospodarz` + `Posiadanie piłki Gospodarz` + `Sytuacje bramkowe Gospodarz` + `Strzały na bramkę Gospodarz` + `Strzały niecelne Gospodarz` + `Rzuty rożne Gospodarz` + `Spalone Gospodarz` + `Interwencje bramkarzy Gospodarz` + `Faule Gospodarz` + `Żółte kartki Gospodarz` + `Czerwone kartki Gospodarz` + `Posiadanie piłki Gość` + `Sytuacje bramkowe Gość` + `Strzały na bramkę Gość` + `Strzały niecelne Gość` + `Rzuty rożne Gość` + `Spalone Gość` + `Interwencje bramkarzy Gość` + `Faule Gość` + `Żółte kartki Gość` + `Czerwone kartki Gość` + `Gole Gospodarz` + `Gole Gość` + Sezon
dd1 <- model.frame(formula, df)
dd1 <- model.matrix(formula, dd1)
dd2 <- dd1[dd1[, "Sezon2022/23"] == 1, ]
dd1 <- dd1[dd1[, "Sezon2022/23"] == 0, ]
dd2 <- dd2[, !(grepl("Sezon", colnames(dd2)))]
dd1 <- dd1[, !(grepl("Sezon", colnames(dd1)))]

dd1y <- ((df %>% subset(!(df$Sezon %in% c("2021/22", "2022/23"))) %>% dplyr::select("Wynik"))$Wynik %>% as.numeric()) - 1
dd1 <- data.matrix(dd1[, -1])

dd2y <- ((df %>% subset(df$Sezon == "2022/23" | df$Sezon == "2021/22") %>% dplyr::select("Wynik"))$Wynik %>% as.numeric()) - 1
dd2 <- data.matrix(dd2[, -1])

dd1 <- xgb.DMatrix(data = dd1, label = dd1y)
dd2 <- xgb.DMatrix(data = dd2, label = dd2y)

watchlist <- list(train = dd1, test = dd2)

CV <- xgb.cv(data = dd1, max.depth = 5, watchlist = watchlist, nrounds = 300, params = list(objective = "multi:softmax", num_class = 3), nfold = 40)

bb <- xgb.train(data = dd1, max.depth = 5, watchlist = watchlist, nrounds = CV$evaluation_log$test_mlogloss_mean %>% which.min(), params = list(objective = "multi:softmax", num_class = 3))

mean(predict(bb, dd1) != dd1y)
mean(predict(bb, dd2) != dd2y)

confusionMatrix(predict(bb, dd1) %>% as.factor(), dd1y %>% as.factor())
confusionMatrix(predict(bb, dd2) %>% as.factor(), dd2y %>% as.factor())

# 2 ####
df <- proccessed_data_averages
df <- df[!(df$Sezon == "2022/23"), ]
AAA <- apply(df[, sapply(df, is.numeric)], MARGIN = 2, FUN = function(x) {sum(is.na(x))}) / nrow(df)
AAA <- AAA[AAA < .1] %>% names
df1 <- df %>% mutate_if(is.numeric, ~replace_na(.,mean(., na.rm = TRUE)))
df1$Wynik <- ordered(df1$Wynik)

df2 <- proccessed_data_averages
df2 <- df2[df2$Sezon == "2022/23", ]
AAA <- apply(df2[, sapply(df2, is.numeric)], MARGIN = 2, FUN = function(x) {sum(is.na(x))}) / nrow(df2)
AAA <- AAA[AAA < .1] %>% names
df2 <- df2 %>% mutate_if(is.numeric, ~replace_na(.,mean(., na.rm = TRUE)))
df2$Wynik <- ordered(df2$Wynik)

formula <- Wynik ~ `Posiadanie piłki Gospodarz` + `Posiadanie piłki Gospodarz` + `Sytuacje bramkowe Gospodarz` + `Strzały na bramkę Gospodarz` + `Strzały niecelne Gospodarz` + `Rzuty rożne Gospodarz` + `Spalone Gospodarz` + `Interwencje bramkarzy Gospodarz` + `Faule Gospodarz` + `Żółte kartki Gospodarz` + `Czerwone kartki Gospodarz` + `Posiadanie piłki Gość` + `Sytuacje bramkowe Gość` + `Strzały na bramkę Gość` + `Strzały niecelne Gość` + `Rzuty rożne Gość` + `Spalone Gość` + `Interwencje bramkarzy Gość` + `Faule Gość` + `Żółte kartki Gość` + `Czerwone kartki Gość` + `Gole Gospodarz` + `Gole Gość`
dd1 <- model.frame(formula, df1)
dd2 <- model.frame(formula, df2)

dd1y <- (dd1$Wynik %>% as.numeric()) - 1
dd1 <- data.matrix(dd1[, -1])

dd2y <- (dd2$Wynik %>% as.numeric()) - 1
dd2 <- data.matrix(dd2[, -1])

dd1 <- xgb.DMatrix(data = dd1, label = dd1y)
dd2 <- xgb.DMatrix(data = dd2, label = dd2y)

watchlist <- list(train = dd1, test = dd2)

modelXGB <- xgb.train(data = dd1, max.depth = 10, watchlist = watchlist, nrounds = 300, params = list(objective = "multi:softmax", num_class = 3))
modelXGB1 <- xgb.train(data = dd1, max.depth = 2, watchlist = watchlist, nrounds = 300, params = list(objective = "multi:softmax", num_class = 3))

CV <- xgb.cv(data = dd1, max.depth = 12, watchlist = watchlist, nrounds = 300, params = list(objective = "multi:softmax", num_class = 3), nfold = 50)


bb <- xgb.train(data = dd1, max.depth = 12, watchlist = watchlist, nrounds = CV$evaluation_log$test_mlogloss_mean %>% which.min(), params = list(objective = "multi:softmax", num_class = 3))

mean(predict(bb, dd1) != dd1y)
mean(predict(bb, dd2) != dd2y)

confusionMatrix(predict(bb, dd1) %>% as.factor(), dd1y %>% as.factor())
confusionMatrix(predict(bb, dd2) %>% as.factor(), dd2y %>% as.factor())

bb1 <- xgb.train(data = dd1, max.depth = 2, watchlist = watchlist, nrounds = modelXGB1$evaluation_log$test_mlogloss %>% which.min(), params = list(objective = "multi:softmax", num_class = 3))

mean(predict(bb1, dd1) != dd1y)
mean(predict(bb1, dd2) != dd2y)

confusionMatrix(predict(bb1, dd1) %>% factor(levels = 0:2), dd1y %>% as.factor())
confusionMatrix(predict(bb1, dd2) %>% factor(levels = 0:2), dd2y %>% as.factor())

# 3 ####
df <- proccessed_data_averages
AAA <- apply(df[, sapply(df, is.numeric)], MARGIN = 2, FUN = function(x) {sum(is.na(x))}) / nrow(df)
AAA <- AAA[AAA < .1] %>% names
df <- df %>% mutate_if(is.numeric, ~replace_na(.,mean(., na.rm = TRUE)))
df$Wynik <- ordered(df$Wynik)

formula <- Wynik ~ `Posiadanie piłki Gospodarz` + `Posiadanie piłki Gospodarz` + `Sytuacje bramkowe Gospodarz` + `Strzały na bramkę Gospodarz` + `Strzały niecelne Gospodarz` + `Rzuty rożne Gospodarz` + `Spalone Gospodarz` + `Interwencje bramkarzy Gospodarz` + `Faule Gospodarz` + `Żółte kartki Gospodarz` + `Czerwone kartki Gospodarz` + `Posiadanie piłki Gość` + `Sytuacje bramkowe Gość` + `Strzały na bramkę Gość` + `Strzały niecelne Gość` + `Rzuty rożne Gość` + `Spalone Gość` + `Interwencje bramkarzy Gość` + `Faule Gość` + `Żółte kartki Gość` + `Czerwone kartki Gość` + `Gole Gospodarz` + `Gole Gość` + Sezon
dd1 <- model.frame(formula, df)
dd1 <- model.matrix(formula, dd1)
dd2 <- dd1[dd1[, "Sezon2022/23"] == 1, ]
dd1 <- dd1[dd1[, "Sezon2022/23"] == 0, ]
dd2 <- dd2[, !(grepl("Sezon", colnames(dd2)))]
dd1 <- dd1[, !(grepl("Sezon", colnames(dd1)))]

dd1y <- ((df %>% subset(!(df$Sezon %in% c("2022/23"))) %>% dplyr::select("Wynik"))$Wynik %>% as.numeric()) - 1
dd1 <- data.matrix(dd1[, -1])

dd2y <- ((df %>% subset(df$Sezon == "2022/23") %>% dplyr::select("Wynik"))$Wynik %>% as.numeric()) - 1
dd2 <- data.matrix(dd2[, -1])

dd <- rbind(dd1, dd2)

pr <- prcomp(dd)

(pr$sdev %>% cumsum()) / sum(pr$sdev)

barplot(pr$sdev, col = "navy", names = colnames(pr$x))

# choosing variables so that at least 95% of std.dev is explained

varNum <- which(((pr$sdev %>% cumsum()) / sum(pr$sdev)) > .95)[1]

dd <- pr$x[, 1:varNum]

dd2 <- dd[-(1:(dim(dd1)[1])), ]
dd1 <- dd[1:(dim(dd1)[1]), ]

# rough models
# LDA
LDA <- lda(x = dd1, grouping = dd1y)

mean(predict(LDA)$class != dd1y)
mean(predict(LDA, dd2)$class != dd2y)

confusionMatrix(predict(LDA)$class %>% as.factor(), dd1y %>% as.factor())
confusionMatrix(predict(LDA, dd2)$class %>% as.factor(), dd2y %>% as.factor())

# QDA
QDA <- qda(x = dd1, grouping = dd1y)

mean(predict(QDA)$class != dd1y)
mean(predict(QDA, dd2)$class != dd2y)

confusionMatrix(predict(QDA)$class %>% as.factor(), dd1y %>% as.factor())
confusionMatrix(predict(QDA, dd2)$class %>% as.factor(), dd2y %>% as.factor())

# xgboost

dd1 <- xgb.DMatrix(data = dd1, label = dd1y)
dd2 <- xgb.DMatrix(data = dd2, label = dd2y)

watchlist <- list(train = dd1, test = dd2)

CV <- xgb.cv(data = dd1, max.depth = 3, watchlist = watchlist, nrounds = 300, params = list(objective = "multi:softmax", eval_metric = c("merror", "auc"), num_class = 3), nfold = 40)

bb <- xgb.train(data = dd1, max.depth = 3, watchlist = watchlist, nrounds = CV$evaluation_log$test_merror_mean %>% which.min(), params = list(objective = "multi:softmax", eval_metric = c("merror", "auc"), num_class = 3))

mean(predict(bb, dd1) != dd1y)
mean(predict(bb, dd2) != dd2y)

confusionMatrix(predict(bb, dd1) %>% as.factor(), dd1y %>% as.factor())
confusionMatrix(predict(bb, dd2) %>% as.factor(), dd2y %>% as.factor())

# with added Home/Away teams

df <- proccessed_data_averages
AAA <- apply(df[, sapply(df, is.numeric)], MARGIN = 2, FUN = function(x) {sum(is.na(x))}) / nrow(df)
AAA <- AAA[AAA < .1] %>% names
df <- df %>% mutate_if(is.numeric, ~replace_na(.,min(., na.rm = TRUE)))
df$Wynik <- ordered(df$Wynik)

formula <- Wynik ~ `Posiadanie piłki Gospodarz` + `Posiadanie piłki Gospodarz` + `Sytuacje bramkowe Gospodarz` + `Strzały na bramkę Gospodarz` + `Strzały niecelne Gospodarz` + `Rzuty rożne Gospodarz` + `Spalone Gospodarz` + `Interwencje bramkarzy Gospodarz` + `Faule Gospodarz` + `Żółte kartki Gospodarz` + `Czerwone kartki Gospodarz` + `Posiadanie piłki Gość` + `Sytuacje bramkowe Gość` + `Strzały na bramkę Gość` + `Strzały niecelne Gość` + `Rzuty rożne Gość` + `Spalone Gość` + `Interwencje bramkarzy Gość` + `Faule Gość` + `Żółte kartki Gość` + `Czerwone kartki Gość` + `Gole Gospodarz` + `Gole Gość` + Sezon
dd1 <- model.frame(formula, df, na.action = na.fail)
dd1 <- model.matrix(formula, dd1)
dd2 <- dd1[dd1[, "Sezon2022/23"] == 1, ]
dd1 <- dd1[dd1[, "Sezon2022/23"] == 0, ]
dd2 <- dd2[, !(grepl("Sezon", colnames(dd2)))]
dd1 <- dd1[, !(grepl("Sezon", colnames(dd1)))]

dd1y <- ((df %>% subset(!(df$Sezon %in% c("2022/23"))) %>% dplyr::select("Wynik"))$Wynik %>% as.numeric()) - 1
dd1 <- data.matrix(dd1[, -1])

dd2y <- ((df %>% subset(df$Sezon == "2022/23") %>% dplyr::select("Wynik"))$Wynik %>% as.numeric()) - 1
dd2 <- data.matrix(dd2[, -1])

dd <- rbind(dd1, dd2)

pr <- prcomp(dd)

(pr$sdev %>% cumsum()) / sum(pr$sdev)

barplot(pr$sdev, col = "navy", names = colnames(pr$x))

# choosing variables so that at least 95% of std.dev is explained

varNum <- which(((pr$sdev %>% cumsum()) / sum(pr$sdev)) > .95)[1]

dd <- pr$x[, 1:varNum]

dd <- cbind(dd, model.matrix(Wynik ~ Gospodarz + Gość + LowerLeague_Away + LowerLeague_Home - 1, model.frame(Wynik ~ Gospodarz + Gość + LowerLeague_Away + LowerLeague_Home - 1, df)))

dd2 <- dd[-(1:(dim(dd1)[1])), ]
dd1 <- dd[1:(dim(dd1)[1]), ]

dd1 <- xgb.DMatrix(data = dd1, label = dd1y)
dd2 <- xgb.DMatrix(data = dd2, label = dd2y)

watchlist <- list(train = dd1, test = dd2)

CV <- xgb.cv(data = dd1, max.depth = 4, watchlist = watchlist, nrounds = 300, params = list(objective = "multi:softmax", eval_metric = c("merror", "auc"), num_class = 3), nfold = 40)
# 3 - 0.5632184 4 - 0.5172414 5 - 0.4942529 (63) 6 - 0.5862069
bb <- xgb.train(data = dd1, max.depth = 5, watchlist = watchlist, nrounds = 63, params = list(objective = "multi:softmax", eval_metric = c("merror", "auc"), num_class = 3))

mean(predict(bb, dd1) != dd1y)
mean(predict(bb, dd2) != dd2y)

confusionMatrix(predict(bb, dd1) %>% as.factor(), dd1y %>% as.factor())
confusionMatrix(predict(bb, dd2) %>% as.factor(), dd2y %>% as.factor())
