######################################################
######################### STAGE TWO.FIVE ##################
#####################################################

# Funkcja do procesowania danych dla jednego sezonu
process_season_data <- function(sezon, dane_tekst) {
  # Podziel tekst na linie i usuń puste linie
  linie <- strsplit(dane_tekst, "\n")[[1]]
  linie <- linie[linie != ""]
  
  # Usuń nagłówek i wiersz sum
  linie <- linie[!grepl("KlubKadra|wartość rynkowa$|^\\s*$", linie)]
  
  # Funkcja do czyszczenia i konwersji wartości
  clean_value <- function(x) {
    as.numeric(gsub("[^0-9.]", "", x))
  }
  
  # Przygotuj puste wektory na dane
  kluby <- character()
  kadra <- numeric()
  wiek <- numeric()
  wartosc_obc <- numeric()
  wartosc_total <- numeric()
  
  # Przetwórz każdą linię
  for(linia in linie) {
    # Wyciągnij dane z linii
    parts <- strsplit(linia, "€")[[1]][1]  # weź część przed pierwszym €
    parts <- strsplit(parts, " ")[[1]]     # podziel na słowa
    parts <- parts[parts != ""]            # usuń puste elementy
    
    # Zbierz nazwę klubu
    last_num_idx <- max(which(grepl("^[0-9]", parts)))
    klub <- paste(parts[1:(last_num_idx-2)], collapse = " ")
    
    # Zbierz wartości numeryczne
    kadra_val <- clean_value(parts[last_num_idx-1])
    wiek_val <- clean_value(parts[last_num_idx])
    
    # Znajdź wartości pieniężne
    wartosc_obc_match <- regexpr("[0-9.]+(?=\\s*tys\\.\\s*€)", linia, perl=TRUE)
    wartosc_total_match <- regexpr("[0-9.]+(?=\\s*mln\\s*€)", linia, perl=TRUE)
    
    wartosc_obc_val <- clean_value(substr(linia, wartosc_obc_match, 
                                          wartosc_obc_match + attr(wartosc_obc_match, "match.length") - 1))
    wartosc_total_val <- clean_value(substr(linia, wartosc_total_match, 
                                            wartosc_total_match + attr(wartosc_total_match, "match.length") - 1))
    
    # Dodaj do wektorów
    kluby <- c(kluby, klub)
    kadra <- c(kadra, kadra_val)
    wiek <- c(wiek, wiek_val)
    wartosc_obc <- c(wartosc_obc, wartosc_obc_val)
    wartosc_total <- c(wartosc_total, wartosc_total_val)
  }
  
  # Stwórz ramkę danych
  df <- data.frame(
    Sezon = rep(sezon, length(kluby)),
    Klub = trimws(kluby),
    Kadra = kadra,
    Sredni_wiek = wiek,
    # Wartosc_obcokrajowcow_tys_euro = wartosc_obc,
    Wartosc_calkowita_mln_euro = wartosc_total
  )
  
  # Dodaj kolumnę z procentowym udziałem wartości obcokrajowców
  df$Procent_wartosci_obcokrajowcow <- 
    (df$Wartosc_obcokrajowcow_tys_euro / 1000) / df$Wartosc_calkowita_mln_euro * 100
  
  return(df)
}

# Dane dla sezonu 2012/13
dane_2012_13 <- data.frame(
  Sezon = rep("2012/13", 16),
  Klub = c(
    "Legia Warszawa", "Lech Poznań", "Śląsk Wrocław", "Wisła Kraków", 
    "Polonia Warszawa", "Górnik Zabrze", "Zagłębie Lubin", "Jagiellonia Białystok",
    "Lechia Gdańsk", "Ruch Chorzów", "Piast Gliwice", "Widzew Łódź",
    "GKS Bełchatów", "Korona Kielce", "Pogoń Szczecin", "Podbeskidzie BB"
  ),
  Kadra = c(33, 33, 34, 33, 41, 33, 32, 36, 41, 34, 31, 34, 42, 38, 34, 38),
  Sredni_wiek = c(26.3, 25.5, 26.2, 26.3, 25.0, 26.6, 26.2, 24.1, 24.1, 26.4, 26.8, 23.5, 24.9, 25.8, 26.2, 27.3),
  Wartosc_calkowita_mln_euro = c(24.90, 19.50, 15.95, 15.35, 15.03, 13.83, 13.15, 11.30, 10.68, 9.75, 8.85, 8.68, 8.55, 8.45, 8.13, 7.58)
)

# Dane dla sezonu 2013/14
dane_2013_14 <- data.frame(
  Sezon = rep("2013/14", 16),
  Klub = c(
    "Legia Warszawa", "Lech Poznań", "Górnik Zabrze", "Śląsk Wrocław",
    "Zagłębie Lubin", "Wisła Kraków", "Lechia Gdańsk", "Jagiellonia Białystok",
    "Korona Kielce", "Piast Gliwice", "Widzew Łódź", "Pogoń Szczecin",
    "Cracovia", "Zawisza Bydgoszcz", "Ruch Chorzów", "Podbeskidzie BB"
  ),
  Kadra = c(42, 34, 44, 36, 43, 38, 39, 43, 43, 34, 45, 34, 34, 33, 38, 40),
  Sredni_wiek = c(25.0, 25.2, 26.2, 26.2, 24.4, 24.0, 24.2, 23.4, 25.2, 26.0, 23.6, 25.0, 25.2, 26.6, 24.9, 26.7),
  Wartosc_calkowita_mln_euro = c(26.23, 17.40, 13.15, 12.90, 12.58, 12.25, 11.65, 11.63, 10.83, 10.20, 10.13, 9.13, 8.93, 8.13, 7.83, 7.25)
)

# Dane dla sezonu 2014/15
dane_2014_15 <- data.frame(
  Sezon = rep("2014/15", 16),
  Klub = c(
    "Legia Warszawa", "Lech Poznań", "Lechia Gdańsk", "Jagiellonia Białystok",
    "Zawisza Bydgoszcz", "Wisła Kraków", "Śląsk Wrocław", "Pogoń Szczecin",
    "Cracovia", "Piast Gliwice", "Ruch Chorzów", "Górnik Zabrze",
    "Korona Kielce", "GKS Bełchatów", "Górnik Łęczna", "Podbeskidzie BB"
  ),
  Kadra = c(39, 33, 40, 43, 43, 32, 31, 37, 33, 34, 34, 36, 35, 34, 36, 34),
  Sredni_wiek = c(25.5, 24.8, 24.1, 22.9, 26.3, 24.9, 26.3, 24.4, 26.2, 26.6, 26.5, 26.6, 26.1, 26.3, 27.8, 28.0),
  Wartosc_calkowita_mln_euro = c(26.45, 24.30, 13.40, 13.00, 10.80, 9.15, 9.15, 8.25, 7.55, 7.20, 6.95, 6.90, 6.90, 6.50, 5.50, 5.20)
)

# Dane dla sezonu 2015/16
dane_2015_16 <- data.frame(
  Sezon = rep("2015/16", 16),
  Klub = c(
    "Legia Warszawa", "Lech Poznań", "Lechia Gdańsk", "Jagiellonia Białystok",
    "Cracovia", "Wisła Kraków", "Piast Gliwice", "Pogoń Szczecin",
    "Śląsk Wrocław", "Zagłębie Lubin", "Korona Kielce", "Górnik Zabrze",
    "Ruch Chorzów", "Podbeskidzie BB", "BrukBet T.", "Górnik Łęczna"
  ),
  Kadra = c(36, 35, 37, 41, 30, 36, 34, 32, 31, 31, 43, 41, 31, 35, 33, 31),
  Sredni_wiek = c(26.9, 25.7, 25.5, 23.7, 25.4, 25.4, 24.6, 24.2, 26.1, 25.0, 25.1, 27.4, 26.0, 26.1, 28.6, 27.9),
  Wartosc_calkowita_mln_euro = c(31.00, 19.95, 14.53, 13.33, 11.93, 10.53, 9.48, 8.60, 7.93, 7.15, 6.95, 6.73, 6.68, 6.13, 5.98, 5.28)
)

# Dane dla sezonu 2016/17
dane_2016_17 <- data.frame(
  Sezon = rep("2016/17", 16),
  Klub = c(
    "Legia Warszawa", "Lech Poznań", "Lechia Gdańsk", "Cracovia",
    "Jagiellonia Białystok", "Wisła Kraków", "Pogoń Szczecin", "Piast Gliwice",
    "Zagłębie Lubin", "Ruch Chorzów", "Śląsk Wrocław", "Korona Kielce",
    "Wisła Płock", "Arka Gdynia", "BrukBet T.", "Górnik Łęczna"
  ),
  Kadra = c(48, 39, 40, 36, 37, 37, 40, 41, 39, 32, 31, 44, 38, 36, 30, 34),
  Sredni_wiek = c(26.2, 25.1, 26.4, 25.1, 24.8, 26.0, 23.8, 25.0, 25.8, 25.2, 26.9, 25.5, 25.7, 26.9, 27.2, 28.0),
  Wartosc_calkowita_mln_euro = c(50.23, 18.73, 18.25, 16.88, 12.53, 11.70, 10.20, 9.53, 9.40, 9.10, 8.08, 8.05, 6.83, 6.68, 6.45, 5.95)
)

# Dane dla sezonu 2017/18
dane_2017_18 <- data.frame(
  Sezon = rep("2017/18", 16),
  Klub = c(
    "Legia Warszawa", "Lech Poznań", "Jagiellonia Białystok", "Lechia Gdańsk",
    "Wisła Kraków", "Zagłębie Lubin", "Cracovia", "Piast Gliwice",
    "Górnik Zabrze", "Korona Kielce", "BrukBet T.", "Arka Gdynia",
    "Śląsk Wrocław", "Pogoń Szczecin", "Wisła Płock", "Sandecja Nowy Sącz"
  ),
  Kadra = c(48, 34, 37, 40, 40, 36, 45, 34, 35, 37, 34, 36, 35, 41, 37, 33),
  Sredni_wiek = c(26.5, 26.3, 26.1, 26.0, 25.9, 26.4, 24.1, 27.0, 23.8, 25.8, 26.5, 27.8, 26.5, 23.9, 25.7, 27.4),
  Wartosc_calkowita_mln_euro = c(34.20, 19.28, 18.03, 15.68, 12.46, 12.20, 10.38, 9.13, 9.05, 8.88, 8.80, 8.73, 8.55, 8.53, 7.95, 6.68)
)

# Dane dla sezonu 2018/19
dane_2018_19 <- data.frame(
  Sezon = rep("2018/19", 16),
  Klub = c(
    "Legia Warszawa", "Lech Poznań", "Jagiellonia Białystok", "Pogoń Szczecin",
    "Górnik Zabrze", "Lechia Gdańsk", "Cracovia", "Piast Gliwice",
    "Zagłębie Lubin", "Wisła Kraków", "Wisła Płock", "Korona Kielce",
    "Arka Gdynia", "Śląsk Wrocław", "Miedź Legnica", "Zagłębie Sosnowiec"
  ),
  Kadra = c(44, 41, 45, 44, 39, 36, 50, 33, 39, 45, 36, 37, 39, 37, 37, 47),
  Sredni_wiek = c(26.5, 25.0, 25.3, 24.1, 23.5, 24.5, 24.6, 25.7, 25.4, 24.6, 25.4, 24.6, 25.5, 25.9, 27.3, 26.1),
  Wartosc_calkowita_mln_euro = c(43.80, 26.70, 21.98, 18.53, 15.98, 15.93, 14.05, 13.00, 12.75, 11.65, 10.60, 10.43, 10.18, 8.70, 8.70, 7.80)
)

# Dane dla sezonu 2019/20
dane_2019_20 <- data.frame(
  Sezon = rep("2019/20", 16),
  Klub = c(
    "Legia Warszawa", "Lech Poznań", "Jagiellonia Białystok", "Lechia Gdańsk",
    "Pogoń Szczecin", "Piast Gliwice", "Zagłębie Lubin", "Cracovia",
    "Wisła Kraków", "Górnik Zabrze", "Śląsk Wrocław", "Wisła Płock",
    "Arka Gdynia", "Korona Kielce", "Raków Częstochowa", "ŁKS Łódź"
  ),
  Kadra = c(44, 33, 43, 42, 39, 36, 39, 36, 40, 37, 38, 38, 41, 45, 38, 38),
  Sredni_wiek = c(25.6, 24.2, 24.7, 24.5, 24.0, 26.8, 24.6, 25.2, 24.9, 24.5, 25.8, 26.1, 26.0, 24.3, 25.7, 26.1),
  Wartosc_calkowita_mln_euro = c(34.68, 23.85, 18.65, 14.45, 13.10, 11.83, 11.20, 10.78, 10.10, 10.08, 9.15, 9.05, 8.88, 8.00, 7.73, 6.60)
)

# Dane dla sezonu 2020/21
dane_2020_21 <- data.frame(
  Sezon = rep("2020/21", 16),
  Klub = c(
    "Lech Poznań", "Legia Warszawa", "Raków Częstochowa", "Pogoń Szczecin",
    "Jagiellonia Białystok", "Cracovia", "Górnik Zabrze", "Zagłębie Lubin",
    "Wisła Kraków", "Lechia Gdańsk", "Piast Gliwice", "Wisła Płock",
    "Śląsk Wrocław", "Warta Poznań", "Stal Mielec", "Podbeskidzie BB"
  ),
  Kadra = c(41, 41, 39, 36, 44, 49, 30, 36, 38, 32, 31, 41, 38, 33, 37, 39),
  Sredni_wiek = c(24.4, 26.3, 25.5, 24.4, 24.9, 24.6, 24.2, 25.1, 25.7, 24.7, 26.8, 25.3, 25.7, 24.8, 25.8, 26.7),
  Wartosc_calkowita_mln_euro = c(50.40, 36.73, 23.93, 21.20, 20.05, 17.53, 16.25, 15.78, 14.88, 14.50, 13.90, 11.73, 11.45, 9.63, 7.50, 7.05)
)

# Dane dla sezonu 2021/22
dane_2021_22 <- data.frame(
  Sezon = rep("2021/22", 18),
  Klub = c(
    "Lech Poznań", "Legia Warszawa", "Raków Częstochowa", "Pogoń Szczecin",
    "Lechia Gdańsk", "Górnik Zabrze", "Piast Gliwice", "Zagłębie Lubin",
    "Cracovia", "Śląsk Wrocław", "Wisła Kraków", "Jagiellonia Białystok",
    "Radomiak Radom", "Wisła Płock", "Warta Poznań", "Stal Mielec",
    "BrukBet T.", "Górnik Łęczna"
  ),
  Kadra = c(42, 47, 39, 34, 34, 37, 39, 46, 42, 41, 40, 50, 36, 34, 39, 40, 44, 37),
  Sredni_wiek = c(25.0, 24.9, 25.3, 25.5, 25.9, 24.4, 26.5, 24.7, 24.5, 25.2, 25.7, 24.3, 26.4, 25.3, 24.6, 26.0, 26.5, 26.8),
  Wartosc_calkowita_mln_euro = c(46.55, 36.78, 32.75, 28.68, 19.48, 18.05, 16.75, 16.70, 16.03, 13.95, 13.23, 13.10, 12.53, 12.40, 10.33, 10.15, 8.48, 5.03)
)

# Dane dla sezonu 2022/23
dane_2022_23 <- data.frame(
  Sezon = rep("2022/23", 18),
  Klub = c(
    "Raków Częstochowa", "Lech Poznań", "Legia Warszawa", "Pogoń Szczecin",
    "Górnik Zabrze", "Cracovia", "Zagłębie Lubin", "Jagiellonia Białystok",
    "Piast Gliwice", "Warta Poznań", "Radomiak Radom", "Śląsk Wrocław",
    "Wisła Płock", "Widzew Łódź", "Stal Mielec", "Korona Kielce",
    "Miedź Legnica", "Lechia Gdańsk"
  ),
  Kadra = c(35, 39, 34, 37, 43, 44, 40, 40, 33, 38, 38, 39, 41, 29, 32, 41, 36, 33),
  Sredni_wiek = c(25.8, 25.3, 26.0, 25.8, 25.5, 25.2, 25.3, 24.9, 26.8, 24.6, 25.7, 24.4, 25.4, 26.5, 25.9, 26.6, 26.7, 26.6),
  Wartosc_calkowita_mln_euro = c(38.23, 36.03, 34.45, 19.20, 17.45, 17.13, 15.30, 14.88, 12.68, 12.08, 11.43, 10.70, 10.50, 10.48, 9.73, 9.10, 8.08, 7.78)
)

# Dane dla sezonu 2023/24
dane_2023_24 <- data.frame(
  Sezon = rep("2023/24", 18),
  Klub = c(
    "Legia Warszawa", "Raków Częstochowa", "Lech Poznań", "Jagiellonia Białystok",
    "Śląsk Wrocław", "Górnik Zabrze", "Radomiak Radom", "Pogoń Szczecin",
    "Zagłębie Lubin", "Cracovia", "Stal Mielec", "Piast Gliwice",
    "Widzew Łódź", "Korona Kielce", "Puszcza Niepołomice", "Ruch Chorzów",
    "ŁKS Łódź", "Warta Poznań"
  ),
  Kadra = c(43, 42, 35, 34, 41, 36, 37, 33, 38, 42, 28, 32, 34, 36, 39, 41, 43, 37),
  Sredni_wiek = c(25.1, 25.9, 25.7, 25.4, 24.0, 25.2, 26.3, 25.8, 25.3, 25.4, 26.2, 27.0, 26.1, 26.0, 25.6, 27.2, 25.3, 24.9),
  Wartosc_calkowita_mln_euro = c(43.63, 38.63, 32.60, 21.78, 20.20, 18.80, 18.58, 16.95, 15.20, 13.45, 11.95, 11.70, 11.70, 9.45, 8.90, 8.88, 7.85, 7.35)
)

# Dane dla sezonu 2024/25
dane_2024_25 <- data.frame(
  Sezon = rep("2024/25", 18),
  Klub = c(
    "Lech Poznań", "Raków Częstochowa", "Legia Warszawa", "Jagiellonia Białystok",
    "Cracovia", "Zagłębie Lubin", "Śląsk Wrocław", "Piast Gliwice",
    "Pogoń Szczecin", "Lechia Gdańsk", "Motor Lublin", "Górnik Zabrze",
    "Widzew Łódź", "Korona Kielce", "Stal Mielec", "Radomiak Radom",
    "GKS Katowice", "Puszcza Niepołomice"
  ),
  Kadra = c(28, 31, 30, 28, 27, 32, 35, 29, 26, 27, 32, 26, 30, 32, 28, 30, 29, 29),
  Sredni_wiek = c(24.8, 25.7, 25.0, 25.1, 25.4, 24.9, 24.2, 26.9, 25.4, 22.4, 25.0, 24.7, 24.9, 24.9, 26.0, 25.9, 27.5, 25.9),
  Wartosc_calkowita_mln_euro = c(32.85, 30.03, 29.75, 22.90, 14.70, 14.58, 13.13, 12.03, 10.90, 10.33, 10.03, 9.95, 9.50, 8.95, 8.90, 8.90, 8.83, 8.15)
)

# Połączenie danych z wszystkich sezonów
dane_klubow <- rbind(dane_2012_13, dane_2013_14, dane_2014_15, dane_2015_16, dane_2016_17, dane_2017_18, dane_2018_19, dane_2019_20, dane_2020_21, dane_2021_22, dane_2022_23, dane_2023_24, dane_2024_25)

# Sortowanie po sezonie i wartości całkowitej (malejąco)
dane_klubow <- dane_klubow[order(dane_klubow$Sezon, -dane_klubow$Wartosc_calkowita_mln_euro), ]

save(dane_klubow, file = "data/ClubsMarketValues.Rdata")
