library(catboost)
library(caret)
library(tidyverse)

# TODO load(data_for_ml) df

# Train test split
df_train <- df[df$Sezon != "2024/25", ]
df_test <- df[df$Sezon == "2024/25", ]
# Imputation
df_train <- df_train %>% drop_na
df_test <- df_test %>% drop_na

# pooling
df_train_y <- df_train %>% select("Wynik")
df_test_y <- df_test %>% select("Wynik")

# formulas to model
# base formula
form_base <- Wynik ~ possession_diff + shots_diff + attack_ratio +
  forma_diff + defensive_strength_home + defensive_strength_away + shot_efficiency_diff +
  discipline_ratio + elo_diff - 1
# advanced formula based on corr matrix
form_adv <- Wynik ~ possession_diff + dominance_index + 
  forma_diff + defensive_strength_home + defensive_strength_away + 
  shooting_accuracy_home + shooting_accuracy_away + 
  discipline_ratio + elo_diff - 1

df_train <- model.matrix(
  form_base,
  df_train
)
df_test <- model.matrix(
  form_base,
  df_test
)
# df_train <- cbind(
#   model.matrix(Wynik ~ . - 1 - Data - Sezon - Gospodarz - Gość - LowerLeague_Away - LowerLeague_Home - Walkower, df_train) %>% as.data.frame,
#   Gospodarz = df_train$Gospodarz %>% as.factor,
#   Gość = df_train$Gość %>% as.factor,
#   LowerLeague_Home = df_train$LowerLeague_Home %>% as.factor,
#   LowerLeague_Away = df_train$LowerLeague_Away %>% as.factor
# )
# df_test <- cbind(
#   model.matrix(Wynik ~ . - 1 - Data - Sezon - Gospodarz - Gość - LowerLeague_Away - LowerLeague_Home - Walkower, df_test) %>% as.data.frame,
#   Gospodarz = df_test$Gospodarz %>% as.factor(),
#   Gość = df_test$Gość %>% as.factor(),
#   LowerLeague_Home = df_test$LowerLeague_Home %>% as.factor,
#   LowerLeague_Away = df_test$LowerLeague_Away %>% as.factor
# )

train_pool <- catboost.load_pool(df_train, 
                                 label = df_train_y$Wynik %>% as.numeric() - 1)
test_pool <- catboost.load_pool(df_test, 
                                label = df_test_y$Wynik %>% as.numeric() - 1)

cat_model <- catboost.train(
  learn_pool = train_pool, 
  test_pool = test_pool, 
  params = list(
    #task_type = "GPU",
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


# bess acc 0.5555555556

caret::confusionMatrix(
  catboost.predict(cat_model, test_pool, prediction_type = "Class") %>% factor, 
  (df_test_y$Wynik %>% as.numeric() - 1) %>% factor
)

