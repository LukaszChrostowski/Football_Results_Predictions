df <- proccessed_data_averages
AAA <- apply(df[, sapply(df, is.numeric)], MARGIN = 2, FUN = function(x) {sum(is.na(x))}) / nrow(df)
AAA <- AAA[AAA < .1] %>% names
df <- df %>% mutate_if(is.numeric, ~replace_na(.,min(., na.rm = TRUE)))
df$Wynik <- ordered(df$Wynik)

formula <- Wynik ~ `Posiadanie piłki Gospodarz` + `Posiadanie piłki Gospodarz` + `Sytuacje bramkowe Gospodarz` + `Strzały na bramkę Gospodarz` + `Strzały niecelne Gospodarz` + `Rzuty rożne Gospodarz` + `Spalone Gospodarz` + `Interwencje bramkarzy Gospodarz` + `Faule Gospodarz` + `Żółte kartki Gospodarz` + `Czerwone kartki Gospodarz` + `Posiadanie piłki Gość` + `Sytuacje bramkowe Gość` + `Strzały na bramkę Gość` + `Strzały niecelne Gość` + `Rzuty rożne Gość` + `Spalone Gość` + `Interwencje bramkarzy Gość` + `Faule Gość` + `Żółte kartki Gość` + `Czerwone kartki Gość` + `Gole Gospodarz` + `Gole Gość` + Sezon + Gospodarz + Gość
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


train_pool <- catboost.load_pool(dd1, label = dd1y)
test_pool <- catboost.load_pool(dd2, label = dd2y)

catModel <- catboost.train(
  learn_pool = train_pool, 
  test_pool = test_pool, 
  params = list(loss_function = "MultiClass", 
                eval_metric = "Accuracy", 
                iterations = 1000, 
                depth = 4, 
                prediction_type = "Class", 
                classes_count = 3)
)

mean(catboost.predict(catModel, train_pool, prediction_type = "Class") != dd1y)
mean(catboost.predict(catModel, test_pool, prediction_type = "Class") != dd2y)

confusionMatrix(catboost.predict(catModel, train_pool, prediction_type = "Class") %>% factor, dd1y %>% factor)
confusionMatrix(catboost.predict(catModel, test_pool, prediction_type = "Class") %>% factor, dd2y %>% factor)
