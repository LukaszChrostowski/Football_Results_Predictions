######################################################
######################### STAGE THREE ##################
#####################################################
library(tidyverse)

load("output_data/processed_data_averages_3.Rdata")
load("data/elo_ranking.Rdata")
load("data/ClubsMarketValues.Rdata")

evaluate <- FALSE ## Whether grid search of xgboost params should be run
df <- proccessed_data_averages_3
df$`Porażka Gospodarz` <- df$`Porażka Gospodarz` %>% as.numeric()
df$`Porażka Gość` <- df$`Porażka Gość` %>% as.numeric()
df$`Remis Gospodarz` <- df$`Remis Gospodarz` %>% as.numeric()
df$`Remis Gość` <- df$`Remis Gość` %>% as.numeric()
# These few first records don't have much data
invalid_cols <- sapply(df, FUN = function(x) {sum(is.na(x))}) / NROW(df) > .1
invalid_cols <- invalid_cols[invalid_cols] %>% names
# TODO df <- df %>% subset(Sezon != "2012/13") %>% select(!(all_of(invalid_cols)))
df <- df %>% select(!(all_of(invalid_cols)))

# Teraz dołączamy ratingi do głównej ramki danych
df <- df %>%
  left_join(elo_df, by = c("Gospodarz" = "Player")) %>%
  rename(elo_home = Rating) %>%
  left_join(elo_df, by = c("Gość" = "Player")) %>%
  rename(elo_away = Rating) %>%
  mutate(
    elo_diff = elo_home - elo_away
  )

############## market values added
# Łączenie danych dla gospodarzy
df <- merge(
  df,
  dane_klubow,
  by.x = c("Gospodarz", "Sezon"),
  by.y = c("Klub", "Sezon"),
  suffixes = c("", "_gospodarz"),
  all.x = TRUE
)

# Łączenie danych dla gości
df <- merge(
  df,
  dane_klubow,
  by.x = c("Gość", "Sezon"),
  by.y = c("Klub", "Sezon"),
  suffixes = c("_gospodarz", "_gosc"),
  all.x = TRUE
)

# Feature Engineering
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
    ),
    
    # Sezonowość
    match_week = week(Data),
    is_end_season = match_week > 30,
    
    market_values_diff = Wartosc_calkowita_mln_euro_gospodarz - Wartosc_calkowita_mln_euro_gosc,
    wiek_diff = Sredni_wiek_gospodarz - Sredni_wiek_gosc,
    kadra_diff = Kadra_gospodarz - Kadra_gosc,
    league_level_advantage = as.numeric(LowerLeague_Home) - as.numeric(LowerLeague_Away)
  )

# df check NA 
sapply(df, function(x) sum(is.na(x))/length(x) * 100)
