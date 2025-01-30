library(xgboost)
library(caret)
library(tidyverse)
library(ggplot2)
load("output_data/processed_data_averages_1.Rdata")

evaluate <- FALSE ## Whether grid search of xgboost params should be run
df <- proccessed_data_averages_1
df$`Porażka Gospodarz` <- df$`Porażka Gospodarz` %>% as.numeric()
df$`Porażka Gość` <- df$`Porażka Gość` %>% as.numeric()
df$`Remis Gospodarz` <- df$`Remis Gospodarz` %>% as.numeric()
df$`Remis Gość` <- df$`Remis Gość` %>% as.numeric()
# These few first records don't have much data
invalid_cols <- sapply(df, FUN = function(x) {sum(is.na(x))}) / NROW(df) > .1
invalid_cols <- invalid_cols[invalid_cols] %>% names
# TODO df <- df %>% subset(Sezon != "2012/13") %>% select(!(all_of(invalid_cols)))
df <- df %>% select(!(all_of(invalid_cols)))

df <- df %>%
  mutate(
    possession_diff = `Posiadanie piłki Gospodarz` - `Posiadanie piłki Gość`,
    
    shots_diff = (`Strzały na bramkę Gospodarz` + `Strzały niecelne Gospodarz`) - 
      (`Strzały na bramkę Gość` + `Strzały niecelne Gość`),
    
    attack_ratio = (`Gole Gospodarz` / (`Strzały na bramkę Gospodarz` + `Strzały niecelne Gospodarz`)) - 
      (`Gole Gość` / (`Strzały na bramkę Gość` + `Strzały niecelne Gość`)),
    
    dominance_index = (`Posiadanie piłki Gospodarz` * (`Strzały na bramkę Gospodarz` + `Strzały niecelne Gospodarz`)) / 
      (`Posiadanie piłki Gość` * (`Strzały na bramkę Gość` + `Strzały niecelne Gość`)),
    
    possession_efficiency_home = `Gole Gospodarz` / `Posiadanie piłki Gospodarz`,
    possession_efficiency_away = `Gole Gość` / `Posiadanie piłki Gość`,
    
    forma_diff = ((`Porażka Gość` * 3 + `Remis Gość` * 1) - 
                    (`Porażka Gospodarz` * 3 + `Remis Gospodarz` * 1)),
    
    defensive_strength_home = `Gole stracone Gospodarz` / (`Strzały na bramkę Gość` + `Strzały niecelne Gość`),
    defensive_strength_away = `Gole stracone Gość` / (`Strzały na bramkę Gospodarz` + `Strzały niecelne Gospodarz`),
    
    shooting_accuracy_home = `Strzały na bramkę Gospodarz` / (`Strzały na bramkę Gospodarz` + `Strzały niecelne Gospodarz`),
    shooting_accuracy_away = `Strzały na bramkę Gość` / (`Strzały na bramkę Gość` + `Strzały niecelne Gość`),
    shot_efficiency_diff = shooting_accuracy_home - shooting_accuracy_away,
    
    discipline_ratio = ifelse(
      (`Żółte kartki Gość` + `Czerwone kartki Gość` * 2) == 0,
      NA,  # lub inna wartość zastępcza
      (`Żółte kartki Gospodarz` + `Czerwone kartki Gospodarz` * 2) /
        (`Żółte kartki Gość` + `Czerwone kartki Gość` * 2)
    )
  )

# df check NA 
sapply(df, function(x) sum(is.na(x))/length(x) * 100)

# corr matrix

# Wybierz tylko numeryczne kolumny i usuń zmienne, które nie chcesz analizować
cor_data <- df %>%
  select_if(is.numeric) %>%
  select(-matches("Data|Sezon|Walkower|Spalone"))

# Stwórz macierz korelacji
cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")

# Znajdujemy silne korelacje (np. > 0.7 lub < -0.7)
high_cors <- which(abs(cor_matrix) > 0.7 & cor_matrix < 0.999, arr.ind = TRUE)

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
df_train <- df[df$Sezon != "2023/24", ]
df_test <- df[df$Sezon == "2023/24", ]
# Imputation
df_train <- df_train %>% drop_na
df_test <- df_test %>% drop_na

# pooling
df_train_y <- df_train %>% select("Wynik")
df_test_y <- df_test %>% select("Wynik")
N <- NROW(df_train)
df_train <- model.matrix(
  Wynik ~ possession_diff + shots_diff + attack_ratio +
    forma_diff + defensive_strength_home + defensive_strength_away +
    shooting_accuracy_home + shooting_accuracy_away + shot_efficiency_diff +
    discipline_ratio - 1,
  rbind(df_train, df_test)
)
df_test <- df_train[-(1:N),]
df_train <- df_train[1:N,]

label_train <- df_train_y$Wynik %>% as.numeric() - 1
label_test <- df_test_y$Wynik %>% as.numeric() - 1

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
        eval_metric = "merror",
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
  best_params <- results[which.min(results$best_score), ]
  print("Najlepsze parametry:")
  print(best_params)
  # eta = 0.01
  # max_depth = 3
  # alpha = 0
}

train_pool <- xgb.DMatrix(data = df_train, label = label_train)
test_pool <- xgb.DMatrix(data = df_test, label = label_test)

watchlist <- list(train = train_pool, test = test_pool)

# Trenowanie końcowego modelu z najlepszymi parametrami
xgb_model <- xgb.train(
  watchlist = watchlist,
  nrounds = 5000,
  data = train_pool,
  params = list(
    objective = "multi:softprob",
    eval_metric = "merror",
    num_class = 3,
    eta = 0.01,
    max_depth = 3,
    alpha = 0
  )
)

xgb_model$evaluation_log$test_merror %>% min
xgb_model$evaluation_log$test_merror %>% which.min

# the best results for form based on 1 and 5 previous games

importance_matrix <- xgb.importance(model = xgb_model)
print(importance_matrix)

# With visualization
xgb.plot.importance(importance_matrix)

saveRDS(xgb_model, "saved_models/xgb_model.rds")
