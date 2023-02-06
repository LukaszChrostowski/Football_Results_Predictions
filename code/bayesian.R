library(rstanarm)
library(tidyverse)
df <- proccessed_data_averages_7

condition <- sapply(df, FUN = function (x) sum(is.na(x))) / NROW(df) < .1
df <- df %>% select(condition[condition] %>% names)
df_test <- df[df$Sezon == "2022/23", ]
df_train <- df[df$Sezon != "2022/23", ]

# normalize numeric variables
numeric_columns <- colnames(df_test)[sapply(df_test, is.numeric)]
df_test[, numeric_columns] <- sapply(df_test[, numeric_columns], FUN = function(x) {
  if (is.numeric(x)) {
    (x - min(x)) / (max(x) - min(x))
  } else x
})

df_train[, numeric_columns] <- sapply(df_train[, numeric_columns], FUN = function(x) {
  if (is.numeric(x)) {
    (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  } else x
})

df_train1 <- df_train %>% select(!c("Data", "Sezon", "Walkower", "Gospodarz", "Gość"))
df_test1 <- df_test %>% select(!c("Data", "Sezon", "Walkower", "Gospodarz", "Gość"))
df_train1$Wynik <- df_train1$Wynik == "H"
df_test1$Wynik <- df_test1$Wynik == "H"

model <- stan_glm(Wynik ~ ., data = df_train1, family = binomial,
                  iter = 5000, cores = 7, chains = 10)

((predict(model, newdata  = df_test1, type = "response") < .5) != df_test1$Wynik) %>% mean

df_train2 <- df_train %>% select(!c("Data", "Sezon", "Walkower", "Gospodarz", "Gość"))
df_test2 <- df_test %>% select(!c("Data", "Sezon", "Walkower", "Gospodarz", "Gość"))
df_train2$Wynik <- df_train2$Wynik == "A"
df_test2$Wynik <- df_test2$Wynik == "A"

model2 <- stan_glm(Wynik ~ ., data = df_train2, family = binomial,
                   iter = 5000, cores = 7, chains = 10)

((predict(model2, newdata  = df_test2, type = "response") < .5) != df_test2$Wynik) %>% mean

pred <- ifelse(
  predict(model, newdata  = df_test1, type = "response") < .5,
  "H",
  ifelse(
    predict(model2, newdata  = df_test2, type = "response") < .5,
    "A",
    "D"
  )
) %>% as.factor

actual <- df_test$Wynik %>% as.factor

caret::confusionMatrix(reference = actual, data = pred)
