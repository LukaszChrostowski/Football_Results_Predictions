######################################################
######################### STAGE TWO.FIVE ##################
#####################################################

library(PlayerRatings)
library(dplyr)
######## ELO rating added and H2H stats
load("output_data/processed_data_averages_3.Rdata")
df <- proccessed_data_averages_3

# Obliczanie ELO lub podobnego ratingu

# Przygotowanie danych do ELO
matches_for_elo <- df %>%
  mutate(
    # (1) Kolejność czasowa meczów
    period = row_number(),
    
    # (4) Konwersja wyniku na format numeryczny:
    # 1 = wygrana gospodarza (H)
    # 0.5 = remis (D)
    # 0 = wygrana gościa (A)
    result = case_when(
      Wynik == "H" ~ 1,
      Wynik == "D" ~ 0.5,
      Wynik == "A" ~ 0
    )
  ) %>%
  select(
    period,    # (1) okres/czas
    Gospodarz, # (2) gracz pierwszy
    Gość,      # (3) gracz drugi
    result     # (4) wynik
  )

# Obliczanie ELO
elo_ratings <- elo(matches_for_elo)

elo_df <- elo_ratings$ratings %>%
  select(Player, Rating) # wybieramy tylko nazwę drużyny i rating
save(elo_df, file = "data/elo_ranking.Rdata")
