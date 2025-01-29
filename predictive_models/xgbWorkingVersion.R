library(xgboost)
library(caret)
library(tidyverse)
load("output_data/processed_data_averages_7.Rdata")

df <- proccessed_data_averages_7
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
N <- NROW(df_train)
df_train <- model.matrix(Wynik ~ . - 1 - Data - Sezon - Walkower, rbind(df_train, df_test))
df_test <- df_train[-(1:N),]
df_train <- df_train[1:N,]

train_pool <- xgb.DMatrix(data = df_train, label = df_train_y$Wynik %>% as.numeric() - 1)
test_pool <- xgb.DMatrix(data = df_test, label = df_test_y$Wynik %>% as.numeric() - 1)

watchlist <- list(train = train_pool, test = test_pool)

xgb_model <- xgb.train(
  watchlist = watchlist, 
  nrounds = 5000, 
  data = train_pool,
  params = list(
    objective = "multi:softprob", 
    eval_metric = "merror", 
    num_class = 3,
    eta = .0005,
    max_depth = 4,
    subsample  = .5,
    lambda = 1,
    alpha = 1
  )
)

xgb_model$evaluation_log$test_merror %>% min
xgb_model$evaluation_log$test_merror %>% which.min

saveRDS(xgb_model, "saved_models/xgb_model.rds")
