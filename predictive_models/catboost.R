library(catboost)
library(caret)
library(tidyverse)

load("output/processed_data_averages_5.Rdata")

# Working version ####
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
df_train <- cbind(
  model.matrix(Wynik ~ . - 1 - Data - Sezon - Gospodarz - Gość - LowerLeague_Away - LowerLeague_Home - Walkower, df_train) %>% as.data.frame,
  Gospodarz = df_train$Gospodarz %>% as.factor,
  Gość = df_train$Gość %>% as.factor,
  LowerLeague_Home = df_train$LowerLeague_Home %>% as.factor,
  LowerLeague_Away = df_train$LowerLeague_Away %>% as.factor
)
df_test <- cbind(
  model.matrix(Wynik ~ . - 1 - Data - Sezon - Gospodarz - Gość - LowerLeague_Away - LowerLeague_Home - Walkower, df_test) %>% as.data.frame,
  Gospodarz = df_test$Gospodarz %>% as.factor(),
  Gość = df_test$Gość %>% as.factor(),
  LowerLeague_Home = df_test$LowerLeague_Home %>% as.factor,
  LowerLeague_Away = df_test$LowerLeague_Away %>% as.factor
)

train_pool <- catboost.load_pool(df_train, 
                                 label = df_train_y$Wynik %>% as.numeric() - 1)
test_pool <- catboost.load_pool(df_test, 
                                label = df_test_y$Wynik %>% as.numeric() - 1)

cat_model <- catboost.train(
  learn_pool = train_pool, 
  test_pool = test_pool, 
  params = list(
    task_type = "GPU",
    loss_function = "MultiClass", 
    eval_metric = "Accuracy", 
    iterations = 2000,
    depth = 3,
    prediction_type = "Class", 
    classes_count = 3,
    leaf_estimation_iterations = 70,
    learning_rate = .01
  )
)

caret::confusionMatrix(
  catboost.predict(cat_model, test_pool, prediction_type = "Class") %>% factor, 
  (df_test_y$Wynik %>% as.numeric() - 1) %>% factor
)

