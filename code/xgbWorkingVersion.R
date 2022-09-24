library(xgboost)
library(caret)

# 1 ####
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

formula <- Wynik ~ Gospodarz + Gość + `Posiadanie piłki Gospodarz` + `Posiadanie piłki Gospodarz` + `Sytuacje bramkowe Gospodarz` + `Strzały na bramkę Gospodarz` + `Strzały niecelne Gospodarz` + `Rzuty rożne Gospodarz` + `Spalone Gospodarz` + `Interwencje bramkarzy Gospodarz` + `Faule Gospodarz` + `Żółte kartki Gospodarz` + `Czerwone kartki Gospodarz` + `Posiadanie piłki Gość` + `Sytuacje bramkowe Gość` + `Strzały na bramkę Gość` + `Strzały niecelne Gość` + `Rzuty rożne Gość` + `Spalone Gość` + `Interwencje bramkarzy Gość` + `Faule Gość` + `Żółte kartki Gość` + `Czerwone kartki Gość` + `Gole Gospodarz` + `Gole Gość`
dd1 <- model.frame(formula, df1)
dd2 <- model.frame(formula, df2)

dd1y <- (dd1$Wynik %>% as.numeric()) - 1
dd1 <- data.matrix(dd1[, -1])

dd2y <- (dd2$Wynik %>% as.numeric()) - 1
dd2 <- data.matrix(dd2[, -1])

dd1 <- xgb.DMatrix(data = dd1, label = dd1y)
dd2 <- xgb.DMatrix(data = dd2, label = dd2y)

watchlist <- list(train = dd1, test = dd2)

modelXGB <- xgb.train(data = dd1, max.depth = 4, watchlist = watchlist, nrounds = 1000, params = list(objective = "multi:softmax", num_class = 3))
modelXGB1 <- xgb.train(data = dd1, max.depth = 4, watchlist = watchlist, nrounds = 1000, params = list(objective = "multi:softprob", num_class = 3))

CV <- xgb.cv(data = dd1, max.depth = 4, watchlist = watchlist, nrounds = 1000, params = list(objective = "multi:softmax", num_class = 3), nfold = 25)


bb <- xgb.train(data = dd1, max.depth = 4, watchlist = watchlist, nrounds = CV$evaluation_log$test_mlogloss_mean %>% which.min(), params = list(objective = "multi:softmax", num_class = 3))

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
