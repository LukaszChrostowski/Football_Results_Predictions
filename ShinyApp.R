library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library(lubridate)
library(lattice)
library(xgboost)
library(bslib)

# Data loading
load("output_data/processed_data.Rdata")
load("data/contingency_table_update.Rdata")
load("data/elo_ranking.Rdata")
df <- proccessed_data

# some helpers
ConvFun <- function(data) {
  data[data == "H"] <- 1 
  data[data == "D"] <- 2
  data[data == "A"] <- 3
  
  for (col in colnames(data)) {
    data[, col] <- as.numeric(data[, col])
  }
  data <- as.matrix(data)
  data
}

levelplotFun <- function(data){
  plt <- levelplot(t(data[c(nrow(data):1), ]), 
                   scales=list(x=list(rot=45)),
                   col.regions = colorRampPalette(c("green", "yellow", "red"))(100),
                   colorkey=FALSE,
                   xlab = "Gość", 
                   ylab = "Gospodarz")
  plt
}

# UI
ui <- fluidPage(
  theme = bs_theme(
    version = 4,
    bg = "#101010",
    fg = "#FDF7F7",
    primary = "#ED79F9",
    "navbar-bg" = "#3ADAC6",
    base_font = font_google("Prompt"),
    heading_font = font_google("Sen"),
    code_font = font_google("JetBrains Mono")
  ),
  
  titlePanel("Analytics Report - Polish Ekstraklasa"),
  
  sidebarLayout(
      sidebarPanel(
        
        selectInput("club",
                    label = "Club",
                    choices = unique(df$Gospodarz),
                    selected = "Lech Poznań"),
        
        selectInput("season",
                    label = "Season",
                    choices = as.vector(unique(df$Sezon)),
                    selected = "2023/24"),
        tags$div(
          tags$p(style = "font-size: 14px;", "Select a club to see a detailed analysis of the team statistics in the Club Analysis section. Statistics are for all available seasons."),
          tags$p(style = "font-size: 14px;", "Select a Season to view general statistics from that season in the Season Analysis section."),
          tags$p(style = "font-size: 14px;", "The Predictive Analysis section is used to predict matches based on performance from the last match.")
        ),
        
        tags$div(
          tags$hr(),  # Dodanie poziomej linii dla oddzielenia
          tags$p(style = "font-size: 12px;", "Source: ", tags$a(href = "https://github.com/LukaszChrostowski/Football_Results_Predictions", 
                                         target = "_blank", "GitHub"))
        )
    ),
    
    mainPanel(
      tabsetPanel(
        # Zakładka Analiza Klubu
        tabPanel("Club Analysis",
                 
                 # selectInput("club", 
                 #             label = "Klub",
                 #             choices = unique(df$Gospodarz), 
                 #             selected = "Lech Poznań"),
                 
                 h3("Who has the club faced most often?"),
                 plotlyOutput("najczestsze_mecze"),
                 
                 h3("Who has the club won against most often?"),
                 plotlyOutput("najczestsze_wygrane"),
                 
                 h3("Who has the club drawn with most often?"),
                 plotlyOutput("najczestsze_remisy"),
                 
                 h3("Who has the club lost to most often?"),
                 plotlyOutput("najczestsze_porazki")
        ),
        
        # Zakładka Analiza Sezonu
        tabPanel("Season Analysis",
                 
                 # selectInput("season", 
                 #             label = "Sezon",
                 #             choices = as.vector(unique(df$Sezon)), 
                 #             selected = "2023/24"),
                 
                 # h3("Tabela końcowa"),
                 # plotOutput("table"),
                 
                 h3("Results Heat Map"),
                 plotOutput("heatmap_wynikow", height = "650px"),
                 
                 # h3("Konkurencyjność Sezonu"),
                 # plotOutput("competetiveness"),
                 
                 h3("Distribution of Results"),
                 plotlyOutput("rozklad_wynikow"),
                 
                 h3("Distribution of Goals"),
                 plotlyOutput("rozklad_goli"),
                 
                 h3("Number of Goals in the Season"),
                 plotlyOutput("gole_sezon"),
                 
                 h3("Most Effective Teams"),
                 plotlyOutput("najskuteczniejsze_zespoly"),
                 
                 h3("Teams Ranking by Red Cards"),
                 plotlyOutput("czerwone_kartki"),
                 
                 h3("Teams Ranking by Yellow Cards"),
                 plotlyOutput("zolte_kartki"),
                 
                 h3("Number of Matches per Month"),
                 plotlyOutput("mecze_miesiac"),
                 
                 h3("Number of Matches at a Specific Time"),
                 plotlyOutput("mecze_godzina")
        ),
        
        # Zakładka Analiza Predykcyjna
        tabPanel("Predictive Analysis",
                 div(
                   style = "max-width: 1200px; margin: 0 auto;",
                   
                   # Panel statystyk
                   wellPanel(
                     style = "background-color: #202020;",
                     
                     # Dodanie wyboru drużyn i średniego wieku
                     h4("Team Selection"),
                     fluidRow(
                       column(6,
                              selectInput("home_team", "Home team:",
                                          choices = unique(df$Gospodarz),
                                          selected = "Lech Poznań"),
                              numericInput("home_avg_age", "Average age of home team players:",
                                           value = 25, min = 18, max = 40, step = 0.1)
                       ),
                       column(6,
                              selectInput("away_team", "Away Team:",
                                          choices = unique(df$Gość),
                                          selected = "Legia Warszawa"),
                              numericInput("away_avg_age", "Average age of away team players:",
                                           value = 25, min = 18, max = 40, step = 0.1)
                       )
                     ),
                     
                     h4("Home team form (last 3 matches)"),
                     fluidRow(
                       column(6,
                              numericInput("home_goals_scored", "Average goals scored::",
                                           value = 1.5, min = 0, max = 5, step = 0.1),
                              numericInput("home_goals_conceded", "Average goals conceded::",
                                           value = 1.0, min = 0, max = 5, step = 0.1)
                       ),
                       column(6,
                              numericInput("home_losses", "Number of losses:",
                                           value = 2, min = 0, max = 7),
                              numericInput("home_draws", "Number of draws:",
                                           value = 2, min = 0, max = 7)
                       )
                     ),
                     
                     h4("Away team form (last 3 matches)"),
                     fluidRow(
                       column(6,
                              numericInput("away_goals_scored", "Average goals scored:",
                                           value = 1.5, min = 0, max = 5, step = 0.1),
                              numericInput("away_goals_conceded", "Average goals conceded:",
                                           value = 1.0, min = 0, max = 5, step = 0.1)
                       ),
                       column(6,
                              numericInput("away_losses", "Number of losses:",
                                           value = 2, min = 0, max = 7),
                              numericInput("away_draws", "Number of draws:",
                                           value = 2, min = 0, max = 7)
                       )
                     ),
                     
                     h4("Statistics from the last 3 matches"),
                     fluidRow(
                       column(6,
                              numericInput("home_possession", "Home team possession (%):",
                                           value = 50, min = 0, max = 100),
                              numericInput("home_shots_on_target", "Home team shots on target:",
                                           value = 5, min = 0, max = 20),
                              numericInput("home_shots_off_target", "Home team shots off target:",
                                           value = 3, min = 0, max = 20),
                              numericInput("home_corners", "Home team corners:",
                                           value = 5, min = 0, max = 15),
                              numericInput("home_yellow_cards", "Home team yellow cards:",
                                           value = 2, min = 0, max = 10),
                              numericInput("home_red_cards", "Home team red cards:",
                                           value = 0, min = 0, max = 5)
                       ),
                       column(6,
                              numericInput("away_possession", "Away team possession (%):",
                                           value = 50, min = 0, max = 100),
                              numericInput("away_shots_on_target", "Away team shots on target:",
                                           value = 5, min = 0, max = 20),
                              numericInput("away_shots_off_target", "Away team shots off target:",
                                           value = 3, min = 0, max = 20),
                              numericInput("away_corners", "Away team corners:",
                                           value = 5, min = 0, max = 15),
                              numericInput("away_yellow_cards", "Away team yellow cards:",
                                           value = 2, min = 0, max = 10),
                              numericInput("away_red_cards", "Away team red cards:",
                                           value = 0, min = 0, max = 5)
                       )
                     )
                   ),
                   
                   # Panel predykcji
                   div(
                     style = "text-align: center; margin: 20px;",
                     actionButton("predict", "Predict Result",
                                  class = "btn-primary btn-lg")
                   ),
                   wellPanel(
                     style = "background-color: #202020;",
                     h4(textOutput("teams_text"), align = "center"),
                     div(
                       style = "font-size: 24px; text-align: center; margin: 20px; padding: 20px;",
                       textOutput("pred_text")
                     )
                   )
                 )
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Analiza Klubu
  output$najczestsze_mecze <- renderPlotly({
    club <- input$club
    df_club <- df
    
    dff1 <- df_club %>% 
      filter(Gospodarz == club) %>% 
      group_by(Gość) %>% 
      summarise(n = n()) %>% 
      arrange(-n)
    
    dff2 <- df_club %>% 
      filter(Gość == club) %>% 
      group_by(Gospodarz) %>% 
      summarise(n = n()) %>% 
      arrange(-n)
    
    colnames(dff1) <- colnames(dff2) <- c("Club", "n")
    merge_df <- merge(dff1, dff2, by = "Club")
    merge_df$n <- merge_df$n.x + merge_df$n.y
    merge_df <- merge_df %>% arrange(-n) %>% head(8)
    
    ggplotly(
      ggplot(data = merge_df, aes(x = factor(Club, levels = Club), y = n, fill = Club)) +
        geom_bar(stat = "identity") +
        xlab("Club") +
        ylab("Number of Games") +
        scale_fill_manual(values = 1:nrow(merge_df)) +
        guides(fill = FALSE) +
        theme_dark()
    )
  })
  
  output$najczestsze_wygrane <- renderPlotly({
    club <- input$club
    df_club <- df
    
    dff1 <- df_club %>% 
      filter(Gospodarz == club, Wynik == "H") %>% 
      group_by(Gość) %>% 
      summarise(n = n()) %>% 
      arrange(-n)
    
    dff2 <- df_club %>% 
      filter(Gość == club, Wynik == "A") %>% 
      group_by(Gospodarz) %>% 
      summarise(n = n()) %>% 
      arrange(-n)
    
    colnames(dff1) <- colnames(dff2) <- c("Club", "n")
    merge_df <- merge(dff1, dff2, by = "Club")
    merge_df$n <- merge_df$n.x + merge_df$n.y
    merge_df <- merge_df %>% arrange(-n) %>% head(8)
    
    ggplotly(
      ggplot(data = merge_df, aes(x = factor(Club, levels = Club), y = n, fill = Club)) +
        geom_bar(stat = "identity") +
        xlab("Club") +
        ylab("Number of Games") +
        scale_fill_manual(values = 1:nrow(merge_df)) +
        guides(fill = FALSE) +
        theme_dark()
    )
  })
  
  output$najczestsze_remisy <- renderPlotly({
    club <- input$club
    df_club <- df
    
    dff1 <- df_club %>% 
      filter(Gospodarz == club, Wynik == "D") %>% 
      group_by(Gość) %>% 
      summarise(n = n()) %>% 
      arrange(-n)
    
    dff2 <- df_club %>% 
      filter(Gość == club, Wynik == "D") %>% 
      group_by(Gospodarz) %>% 
      summarise(n = n()) %>% 
      arrange(-n)
    
    colnames(dff1) <- colnames(dff2) <- c("Club", "n")
    merge_df <- merge(dff1, dff2, by = "Club")
    merge_df$n <- merge_df$n.x + merge_df$n.y
    merge_df <- merge_df %>% arrange(-n) %>% head(8)
    
    ggplotly(
      ggplot(data = merge_df, aes(x = factor(Club, levels = Club), y = n, fill = Club)) +
        geom_bar(stat = "identity") +
        xlab("Club") +
        ylab("Number of Games") +
        scale_fill_manual(values = 1:nrow(merge_df)) +
        guides(fill = FALSE) +
        theme_dark()
    )
  })
  
  output$najczestsze_porazki <- renderPlotly({
    club <- input$club
    df_club <- df
    
    dff1 <- df_club %>% 
      filter(Gospodarz == club, Wynik == "A") %>% 
      group_by(Gość) %>% 
      summarise(n = n()) %>% 
      arrange(-n)
    
    dff2 <- df_club %>% 
      filter(Gość == club, Wynik == "H") %>% 
      group_by(Gospodarz) %>% 
      summarise(n = n()) %>% 
      arrange(-n)
    
    colnames(dff1) <- colnames(dff2) <- c("Club", "n")
    merge_df <- merge(dff1, dff2, by = "Club")
    merge_df$n <- merge_df$n.x + merge_df$n.y
    merge_df <- merge_df %>% arrange(-n) %>% head(8)
    
    ggplotly(
      ggplot(data = merge_df, aes(x = factor(Club, levels = Club), y = n, fill = Club)) +
        geom_bar(stat = "identity") +
        xlab("Club") +
        ylab("Number of Loses") +
        scale_fill_manual(values = 1:nrow(merge_df)) +
        guides(fill = FALSE) +
        theme_dark()
    )
  })
  
  # Analiza sezonu
  
  output$heatmap_wynikow <- renderPlot({
    season <- input$season
    df_filtered <- df %>% filter(Sezon == season)
    ct <- switch(season,
                 "2012/13" = ContTable_1213,
                 "2013/14" = ContTable_1314,
                 "2014/15" = ContTable_1415,
                 "2015/16" = ContTable_1516,
                 "2016/17" = ContTable_1617,
                 "2017/18" = ContTable_1718,
                 "2018/19" = ContTable_1819,
                 "2019/20" = ContTable_1920,
                 "2020/21" = ContTable_2021,
                 "2021/22" = ContTable_2122,
                 "2022/23" = ContTable_2223,
                 "2023/24" = ContTable_2324)
    ct <- ConvFun(ct)
    levelplotFun(ct)
  })
  
  dataset <- reactive({
    switch(input$season,
           "2012/13" = ContTable_1213,
           "2013/14" = ContTable_1314,
           "2014/15" = ContTable_1415,
           "2015/16" = ContTable_1516,
           "2016/17" = ContTable_1617,
           "2017/18" = ContTable_1718,
           "2018/19" = ContTable_1819,
           "2019/20" = ContTable_1920,
           "2020/21" = ContTable_2021,
           "2021/22" = ContTable_2122,
           "2022/23" = ContTable_2223,
           "2023/24" = ContTable_2324
    )
  })
  
  # mcmc_results <- reactive({
  #   O <- ConvFun(dataset())
  #   N <- nrow(O)
  #   y <- to_adjacency(O, N)
  #   season <- input$season
  #   mcmc(season = season, N = N, y = y, O = O)
  # })
  
  # df_2 <- reactive({
  #   objj <- mcmc_results()
  #   data.frame(miejsce = 1:nrow(objj$table), 
  #              zespół = rownames(objj$table), 
  #              punkty = objj$table[,1])
  # })
  
  # output$table <- renderTable({
  #   # df_2()
  #   objj <- mcmc_results()
  #   data.frame(miejsce = 1:nrow(objj$table), 
  #              zespół = rownames(objj$table), 
  #              punkty = objj$table[,1])
  # })
  
  # output$competetiveness <- renderPlot({
  #   objj <- mcmc_results()
  #   Noisy_True_K <- objj$Noisy_True_K
  #   K_maxmax <- objj$K_maxmax
  #   
  #   plot(Noisy_True_K, col="#00000033",ylab="K", xlab=" ", cex.lab=1.7,
  #        ylim=c(1,K_maxmax+1), yaxt="n")
  # })
  
  output$rozklad_wynikow <- renderPlotly({
    season <- input$season
    df_filtered <- df %>% filter(Sezon == season) 
    
    ggplotly(
      ggplot(data = df_filtered, aes(x = Wynik, fill = Wynik)) +
        geom_bar() +
        ylab("Number of Results") +
        xlab("Result") +
        scale_fill_manual(values = unique(df_filtered$Wynik)) +
        guides(fill = FALSE) +
        theme_dark()
    )
  })
  
  output$rozklad_goli <- renderPlotly({
    season <- input$season
    df_filtered <- df %>% filter(Sezon == season)
    
    ggplotly(
      ggplot(data = df_filtered, aes(x = `Gole Gospodarz` + `Gole Gość`)) +
        geom_histogram(bins = 10, color = "black", fill = "white") +
        xlab("Number of Goals in the Match") +
        theme_dark()
    )
  })
  
  output$gole_sezon <- renderPlotly({
    season <- input$season
    df_filtered <- df %>% filter(Sezon == season)
    
    goals_by_season <- df_filtered %>% 
      group_by(Sezon) %>% 
      summarise(sum = sum(`Gole Gospodarz` + `Gole Gość`))
    
    ggplotly(
      ggplot(data = goals_by_season, aes(x = Sezon, y = sum, fill = Sezon)) +
        geom_bar(stat = "identity") +
        xlab("Season") +
        ylab("Number of Goals") +
        scale_fill_manual(values = 1:nrow(goals_by_season)) +
        guides(fill = FALSE) +
        theme_dark()
    )
  })
  
  output$najskuteczniejsze_zespoly <- renderPlotly({
    season <- input$season
    df_filtered <- df %>% filter(Sezon == season)
    
    A <- df_filtered %>% 
      group_by(Gospodarz) %>% 
      summarise(sum = sum(`Gole Gospodarz`)) %>% 
      arrange(-sum)
    
    B <- df_filtered %>% 
      group_by(Gość) %>% 
      summarise(sum = sum(`Gole Gość`)) %>% 
      arrange(-sum)
    
    colnames(A) <- colnames(B) <- c("Klub", "gole")
    mergeAB <- merge(A, B, by = "Klub")
    mergeAB$gole <- mergeAB$gole.x + mergeAB$gole.y
    goals_by_club <- mergeAB %>% arrange(-gole) %>% head(8)
    
    ggplotly(
      ggplot(data = goals_by_club, aes(x = factor(Klub, levels = Klub), y = gole, fill = Klub)) +
        geom_bar(stat = "identity") +
        xlab("Club") +
        ylab("Number of Goals") +
        scale_fill_manual(values = 1:nrow(goals_by_club)) +
        guides(fill = FALSE) +
        theme_dark() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
    )
  })
  
  output$czerwone_kartki <- renderPlotly({
    season <- input$season
    df_filtered <- df %>% filter(Sezon == season)
    
    A <- df_filtered %>% 
      group_by(Gospodarz) %>% 
      summarise(sum_red = sum(`Czerwone kartki Gospodarz`))
    
    B <- df_filtered %>% 
      group_by(Gość) %>% 
      summarise(sum_red = sum(`Czerwone kartki Gość`))
    
    colnames(A) <- colnames(B) <- c("Club", "red_cards")
    mergeAB <- merge(A, B, by = "Club")
    mergeAB$red_cards <- mergeAB$red_cards.x + mergeAB$red_cards.y
    red_cards_by_club <- mergeAB %>% arrange(-red_cards) %>% head(8)
    
    ggplotly(
      ggplot(data = red_cards_by_club, aes(x = factor(Club, levels = Club), y = red_cards, fill = Club)) +
        geom_bar(stat = "identity") +
        xlab("Club") +
        ylab("Number of Red Cards") +
        scale_fill_manual(values = 1:nrow(red_cards_by_club)) +
        guides(fill = FALSE) +
        theme_dark() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
    )
  })
  
  output$zolte_kartki <- renderPlotly({
    season <- input$season
    df_filtered <- df %>% filter(Sezon == season)
    
    A <- df_filtered %>% 
      group_by(Gospodarz) %>% 
      summarise(sum_yellow = sum(`Żółte kartki Gospodarz`))
    
    B <- df_filtered %>% 
      group_by(Gość) %>% 
      summarise(sum_yellow = sum(`Żółte kartki Gość`))
    
    colnames(A) <- colnames(B) <- c("Club", "yellow_cards")
    mergeAB <- merge(A, B, by = "Club")
    mergeAB$yellow_cards <- mergeAB$yellow_cards.x + mergeAB$yellow_cards.y
    yellow_cards_by_club <- mergeAB %>% arrange(-yellow_cards) %>% head(8)
    
    ggplotly(
      ggplot(data = yellow_cards_by_club, aes(x = factor(Club, levels = Club), y = yellow_cards, fill = Club)) +
        geom_bar(stat = "identity") +
        xlab("Club") +
        ylab("Number of Yellow Cards") +
        scale_fill_manual(values = 1:nrow(yellow_cards_by_club)) +
        guides(fill = FALSE) +
        theme_dark() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
    )
  })
  
  output$mecze_miesiac <- renderPlotly({
    season <- input$season
    df_filtered <- df %>% filter(Sezon == season)
    
    df_date <- df_filtered %>% 
      group_by(month = month(Data)) %>% 
      summarise(n = n()) %>% 
      arrange(-n) %>% 
      head()
    
    df_date$month <- factor(month.name[df_date$month], levels = month.name[df_date$month])
    
    ggplotly(
      ggplot(data = df_date, aes(x = month, y = n, fill = month)) +
        geom_bar(stat = "identity") +
        xlab("Month") +
        ylab("Number of Games") +
        scale_fill_manual(values = 1:nrow(df_date)) +
        guides(fill = FALSE) +
        theme_dark() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  })
  
  output$mecze_godzina <- renderPlotly({
    season <- input$season
    df_filtered <- df %>% filter(Sezon == season)
    
    df_date <- df_filtered %>% 
      group_by(hour = hour(Data)) %>% 
      summarise(n = n()) %>% 
      arrange(-n) %>% 
      head()
    
    df_date$hour <- factor(df_date$hour)
    
    ggplotly(
      ggplot(data = df_date, aes(x = hour, y = n, fill = hour)) +
        geom_bar(stat = "identity") +
        xlab("Time of Game") +
        ylab("Number of Games") +
        scale_fill_manual(values = 1:nrow(df_date)) +
        guides(fill = FALSE) +
        theme_dark()
    )
  })
  
  # Model predykcyjny
  xgb_model <- readRDS("saved_models/xgb_model.rds")
  
  predictionValue <- eventReactive(
    input$predict,
    {
      
      # form_base <- Wynik ~ possession_diff + shots_diff + attack_ratio +
      # forma_diff + defensive_strength_home + defensive_strength_away + shot_efficiency_diff +
      #   discipline_ratio + elo_diff + wiek_diff + league_level_advantage  - 1
      # Przygotowanie danych
      pred_vector <- numeric(10)
      names(pred_vector) <- c(
          "possession_diff",
          "shots_diff",
          "attack_ratio",
          "forma_diff",
          "defensive_strength_home",
          "defensive_strength_away",
          "shot_efficiency_diff",
          "discipline_ratio",
          "elo_diff",
          "wiek_diff"
      )
      
      # Ustawienie pozostałych zmiennych
      pred_vector["possession_diff"] <- input$home_possession - input$away_possession
      
      pred_vector["shots_diff"] <- (input$home_shots_on_target + input$home_shots_off_target) - 
        (input$away_shots_on_target + input$away_shots_off_target)
      
      pred_vector["attack_ratio"] <- (input$home_goals_scored / (input$home_shots_on_target + input$home_shots_off_target)) - 
        (input$away_goals_scored / (input$away_shots_on_target + input$away_shots_off_target))
      
      pred_vector["forma_diff"] <- ((input$away_losses * 3 + input$away_draws * 1) - 
                                      (input$home_losses * 3 + input$home_draws * 1))
      
      pred_vector["defensive_strength_home"] <- input$home_goals_conceded / 
        (input$away_shots_on_target + input$away_shots_off_target)
      
      pred_vector["defensive_strength_away"] <- input$away_goals_conceded / 
        (input$home_shots_on_target + input$home_shots_off_target)
      
      shooting_accuracy_home <- input$home_shots_on_target / 
        (input$home_shots_on_target + input$home_shots_off_target)
      
      shooting_accuracy_away <- input$away_shots_on_target / 
        (input$away_shots_on_target + input$away_shots_off_target)
      
      pred_vector["shot_efficiency_diff"] <- shooting_accuracy_home - 
        shooting_accuracy_away
      
      pred_vector["discipline_ratio"] <- ifelse(
        (input$away_yellow_cards + input$away_red_cards * 2) == 0,
        NA,
        (input$home_yellow_cards + input$home_red_cards * 2) /
          (input$away_yellow_cards + input$away_red_cards * 2)
      )
      pred_vector["elo_diff"] <- elo_df[elo_df$Player == input$home_team, "Rating"] - elo_df[elo_df$Player == input$away_team, "Rating"]
      pred_vector["wiek_diff"] <- input$home_avg_age - input$away_avg_age
      
      input_matrix <- matrix(pred_vector, nrow = 1)
      pred_matrix <- xgb.DMatrix(input_matrix)
      pred_prob <- predict(xgb_model, pred_matrix)
      
      paste(
        sprintf("\nHome Win: %.1f%%", pred_prob[3] * 100),
        sprintf("\nDraw: %.1f%%", pred_prob[2] * 100),
        sprintf("\nAway Win: %.1f%%", pred_prob[1] * 100),
        sep = "\n"
      )
    },
    ignoreNULL = FALSE
  )
  
  output$pred_text <- renderText({
    predictionValue()
  })
}

# Uruchomienie aplikacji
shinyApp(ui = ui, server = server)