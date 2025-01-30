library(tidyverse)
load("output_data/processed_data_averages_1.Rdata")

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
    
    discipline_ratio = (`Żółte kartki Gospodarz` + `Czerwone kartki Gospodarz` * 2) /
        (`Żółte kartki Gość` + `Czerwone kartki Gość` * 2)
  )

df$Wynik <- ordered(df$Wynik)
# only numeric
formula <- Wynik ~ possession_diff + shots_diff + attack_ratio +
  forma_diff + defensive_strength_home + defensive_strength_away +
  shooting_accuracy_home + shooting_accuracy_away + shot_efficiency_diff +
  discipline_ratio + LowerLeague_Home + LowerLeague_Away + Sezon

dd1 <- model.frame(formula, df %>% drop_na, na.action = na.fail)
dd2 <- dd1[dd1$Sezon == "2024/25", ]
dd1 <- dd1[dd1$Sezon != "2024/25", ]
dd2 <- dd2[, !(grepl("Wynik", colnames(dd2)))]
dd1 <- dd1[, !(grepl("Wynik", colnames(dd1)))]

dd1y <- ((df %>% subset(!(df$Sezon %in% c("2024/25"))) %>% dplyr::select("Wynik"))$Wynik %>% as.numeric()) - 1
dd2y <- ((df %>% subset(df$Sezon == "2024/25") %>% dplyr::select("Wynik"))$Wynik %>% as.numeric()) - 1

dd <- rbind(dd1, dd2)
dd <- dd[, sapply(as.data.frame(dd), is.numeric)]

pr <- prcomp(dd)

(pr$sdev %>% cumsum()) / sum(pr$sdev)

barplot(pr$sdev, col = "navy", names = colnames(pr$x))

# choosing variables so that at least 46% of std.dev is explained

varNum <- which(((pr$sdev %>% cumsum()) / sum(pr$sdev)) > .4605395)[1]

dd <- pr$x[, 1:varNum]

dd <- cbind(dd, model.frame(Wynik ~ Gospodarz + Gość + LowerLeague_Away + LowerLeague_Home - 1, df))

dd2 <- dd[-(1:(dim(dd1)[1])), ]
dd1 <- dd[1:(dim(dd1)[1]), ]

dd2$Wynik <- ifelse(dd2y == 2, 1, 0)
dd1$Wynik <- ifelse(dd1y == 2, 1, 0)

logitModel1 <- glm(formula = Wynik ~ ., family = binomial(), data = dd1)

dd2$Wynik <- ifelse(dd2y == 1, 1, 0)
dd1$Wynik <- ifelse(dd1y == 1, 1, 0)

logitModel2 <- glm(formula = Wynik ~ ., family = binomial(), data = dd1, control = glm.control(maxit = 1000))

# train error
mean(ifelse(predict(logitModel1) > 0, 2, ifelse(predict(logitModel2) > 0, 1, 0)) == dd1y)

# test error
mean(ifelse(predict(logitModel1, dd2) > 0, 2, ifelse(predict(logitModel2, dd2) > 0, 1, 0)) == dd2y)
