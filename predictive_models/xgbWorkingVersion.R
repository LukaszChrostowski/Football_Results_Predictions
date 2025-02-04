library(xgboost)
library(caret)
library(ggplot2)

# TODO load(data_for_ml) df
evaluate <- FALSE ## Whether grid search of xgboost params should be run

# corr matrix

# Wybierz tylko numeryczne kolumny i usuń zmienne, które nie chcesz analizować
cor_data <- df %>%
  select_if(is.numeric) %>%
  select(-matches("Data|Sezon|Walkower|Spalone"))

# Stwórz macierz korelacji
cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")

# Znajdujemy silne korelacje (np. > 0.7 lub < -0.7)
high_cors <- which(abs(cor_matrix) > 0.1 & cor_matrix < 0.999, arr.ind = TRUE)

# Tworzymy ramkę danych z wynikami
high_cor_pairs <- data.frame(
  var1 = rownames(cor_matrix)[high_cors[,1]],
  var2 = colnames(cor_matrix)[high_cors[,2]],
  correlation = cor_matrix[high_cors]
) %>%
  arrange(desc(abs(correlation)))

# Wyświetlamy wyniki
print(high_cor_pairs)

# Train test split
df_train <- df[df$Sezon != "2024/25", ]
df_test <- df[df$Sezon == "2024/25", ]
# Imputation
df_train <- df_train %>% drop_na
df_test <- df_test %>% drop_na

# pooling
df_train_y <- df_train %>% select("Wynik")
df_test_y <- df_test %>% select("Wynik")
N <- NROW(df_train)

# formulas to model
# base formula
form_base <- Wynik ~ possession_diff + shots_diff + attack_ratio +
  forma_diff + defensive_strength_home + defensive_strength_away + shot_efficiency_diff +
  discipline_ratio + elo_diff + wiek_diff  - 1
# advanced formula based on corr matrix
form_adv <- Wynik ~ possession_diff + dominance_index + 
  forma_diff + defensive_strength_home + defensive_strength_away + 
  shooting_accuracy_home + shooting_accuracy_away + 
  discipline_ratio + elo_diff + market_values_diff + 
  league_level_advantage + match_week - 1
df_train <- model.matrix(
  form_base,
  rbind(df_train, df_test)
)
df_test <- df_train[-(1:N),]
df_train <- df_train[1:N,]

# Correct approach
label_train <- df_train_y$Wynik %>% as.numeric() - 1
classes <- table(label_train)
# Make sure we're properly indexing
pos_weights <- 1 / classes[as.character(label_train)]  # Add as.character()
weights <- pos_weights * length(label_train)

# Verify
print(length(weights))  # Should now be 3400
label_test <- df_test_y$Wynik %>% as.numeric() - 1
# A = 0, D = 1, H = 2

if (evaluate) {
  # Definiowanie siatki parametrów do przeszukania
  param_grid <- expand.grid(
    eta = c(0.01, 0.05, 0.1),
    max_depth = c(3, 4, 5),
    alpha = c(0, 0.5, 1)
  )
  
  # Funkcja do trenowania i oceny modelu z danymi parametrami
  evaluate_params <- function(params, train_data, test_data, train_label, test_label, num_class = 3) {
    train_pool <- xgb.DMatrix(data = train_data, label = train_label)
    test_pool <- xgb.DMatrix(data = test_data, label = test_label)
    
    watchlist <- list(train = train_pool, test = test_pool)
    
    model <- xgb.train(
      watchlist = watchlist,
      nrounds = 5000,
      early_stopping_rounds = 1000,
      data = train_pool,
      params = list(
        objective = "multi:softprob",
        eval_metric = "auc",
        num_class = num_class,
        eta = params$eta,
        max_depth = params$max_depth,
        alpha = params$alpha
      )
    )
    
    return(list(
      best_iteration = xgb_model$evaluation_log$test_merror %>% which.min,
      best_score = xgb_model$evaluation_log$test_merror %>% min
    ))
  }
  
  # Przeszukiwanie siatki
  results <- data.frame()
  
  for(i in 1:nrow(param_grid)) {
    cat("Evaluating parameter set", i, "of", nrow(param_grid), "\n")
    
    eval_result <- evaluate_params(
      params = param_grid[i,],
      train_data = df_train,
      test_data = df_test,
      train_label = label_train,
      test_label = label_test
    )
    
    results <- rbind(results, data.frame(
      param_set = i,
      best_iteration = eval_result$best_iteration,
      best_score = eval_result$best_score,
      param_grid[i,]
    ))
  }
  
  # Znalezienie najlepszych parametrów
  best_params <- results[which.max(results$best_score), ]
  print("Najlepsze parametry:")
  print(best_params)
  # eta = 0.01
  # max_depth = 3
  # alpha = 0
}

train_pool <- xgb.DMatrix(data = df_train,
                          #weights = weights,
                          label = label_train)
test_pool <- xgb.DMatrix(data = df_test, label = label_test)

watchlist <- list(train = train_pool, test = test_pool)
set.seed(1000)
# Trenowanie końcowego modelu z najlepszymi parametrami
xgb_model <- xgb.train(
  watchlist = watchlist,
  nrounds = 5000,
  data = train_pool,
  early_stopping_rounds=1000,
  params = list(
    objective = "multi:softprob",
    eval_metric = "merror",
    num_class = 3,
    eta = 0.001,
    max_depth = 3,
    alpha = 0.1,
    min_child_weight = 5
    # subsample = 0.9
    # colsample_bytree = 0.8
    # lambda = 10
  )
)

xgb_model$evaluation_log$test_merror %>% min
xgb_model$evaluation_log$test_merror %>% which.min
# AUC 
# xgb_model$evaluation_log$test_auc %>% max
# xgb_model$evaluation_log$test_auc %>% which.max

# the best results for form based
# number of last games 3
# params
# num_class = 3,
# eta = 0.005,
# max_depth = 3
# min_child_weight = 5
# alpha = 0.1
# final error: 0.4183007

importance_matrix <- xgb.importance(model = xgb_model)
print(importance_matrix)

# With visualization
xgb.plot.importance(importance_matrix)

# confusion matrix
predictions <- predict(xgb_model, newdata = test_pool)
pred_classes <- max.col(matrix(predictions, ncol = 3, byrow = TRUE)) - 1
confusion_matrix <- confusionMatrix(factor(pred_classes), factor(label_test))
print(confusion_matrix)

saveRDS(xgb_model, "saved_models/xgb_model.rds")
