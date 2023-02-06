library(xgboost)
library(caret)
library(tidyverse)
load("C:/Users/Kertoo/Desktop/Football_Results_Predictions/output/processed_data_averages_5.Rdata")

df <- proccessed_data_averages_5
df$`Porażka Gospodarz` <- df$`Porażka Gospodarz` %>% as.numeric()
df$`Porażka Gość` <- df$`Porażka Gość` %>% as.numeric()
df$`Remis Gospodarz` <- df$`Remis Gospodarz` %>% as.numeric()
df$`Remis Gość` <- df$`Remis Gość` %>% as.numeric()
# These few first records don't have much data
invalid_cols <- sapply(df, FUN = function(x) {sum(is.na(x))}) / NROW(df) > .1
invalid_cols <- invalid_cols[invalid_cols] %>% names
df <- df %>% subset(Sezon != "2012/13") %>% select(!(all_of(invalid_cols)))
# Train test split
df_train <- df[df$Sezon != "2022/23", ]
df_test <- df[df$Sezon == "2022/23", ]
# Imputation
df_train <- df_train %>% drop_na

# pooling
df_train_y <- df_train %>% select("Wynik")
df_test_y <- df_test %>% select("Wynik")
df_train <- model.matrix(Wynik ~ . - 1 - Data - Sezon - Walkower, df_train)
df_test <- model.matrix(Wynik ~ . - 1 - Data - Sezon - Walkower, df_test)

train_pool <- xgb.DMatrix(data = df_train, label = df_train_y$Wynik %>% as.numeric() - 1)
test_pool <- xgb.DMatrix(data = df_test, label = df_test_y$Wynik %>% as.numeric() - 1)

watchlist <- list(train = train_pool, test = test_pool)

xgb_model <- xgb.train(
  watchlist = watchlist, 
  nrounds = 50000, 
  data = train_pool,
  params = list(
    objective = "multi:softmax", 
    eval_metric = "merror", 
    num_class = 3,
    eta = .001,
    max_depth = 5,
    lambda = 0,
    subsample  = .5
  )
)

xgb_model$evaluation_log$test_merror %>% min
xgb_model$evaluation_log$test_merror %>% which.min
