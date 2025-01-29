library(shiny)
library(shinythemes)

#load("~/Desktop/AssessingCompetitiveBalance/data/contTables.Rdata")
source("R/MCMC_main.R")

ui <- fluidPage(theme = shinytheme("united"),


  headerPanel('Konkurencyjność sezonu ekstraklasy'),

  sidebarPanel(
    HTML("<h3>Dane wejściowe</h3>"),
    selectInput("season", label = "Sezon:",
                choices = c("sezon 98/99", "sezon 99/00", "sezon 00/01", "sezon 01/02", "sezon 02/03",
                            "sezon 03/04", "sezon 04/05", "sezon 05/06", "sezon 06/07", "sezon 07/08",
                            "sezon 08/09", "sezon 09/10", "sezon 10/11",
                            "sezon 11/12", "sezon 12/13", "sezon 13/14", "sezon 14/15", "sezon 15/16", "sezon 16/17", "sezon 17/18",
                            "sezon 18/19", "sezon 19/20", "sezon 20/21", "sezon 21/22"),
                selected = "sezon 07/08"),
  ),

  mainPanel(
    plotOutput(outputId = "LvlPlot"),
    plotOutput(outputId = "NoisyPlot"),
    tableOutput(outputId = "table")
  )

)

server <- function(input, output){

  dataset <- reactive({
    switch(input$season,
          "sezon 98/99" = ContTable_9899,
          "sezon 99/00" = ContTable_9900,
          "sezon 00/01" = ContTable_0001,
          "sezon 01/02" = ContTable_0102,
          "sezon 02/03" = ContTable_0203,
          "sezon 03/04" = ContTable_0304,
          "sezon 04/05" = ContTable_0405,
          "sezon 05/06" = ContTable_0506,
          "sezon 06/07" = ContTable_0607,
          "sezon 07/08" = ContTable_0708,
          "sezon 08/09" = ContTable_0809,
          "sezon 09/10" = ContTable_0910,
          "sezon 10/11" = ContTable_1011,
          "sezon 12/12" = ContTable_1112,
          "sezon 12/13" = ContTable_1213,
          "sezon 13/14" = ContTable_1314,
          "sezon 14/15" = ContTable_1415,
          "sezon 15/16" = ContTable_1516,
          "sezon 16/17" = ContTable_1617,
          "sezon 17/18" = ContTable_1718,
          "sezon 18/19" = ContTable_1819,
          "sezon 19/20" = ContTable_1920,
          "sezon 20/21" = ContTable_2021,
          "sezon 21/22" = ContTable_2122,
           )
  })

  df <- reactive({
    O <- ConvFun(dataset())
    N <- nrow(O)
    y <- to_adjacency(O, N)
    season <- input$season
    objj <- mcmc(season = season, N = N, y = y, O = O)
    data.frame(miejsce = 1:N, zespół = rownames(objj$table), punkty = objj$table[,1])
  })

  output$LvlPlot <- renderPlot({
    O <- ConvFun(dataset())
    N <- nrow(O)
    y <- to_adjacency(O, N)
    season <- input$season
    objj <- mcmc(season = season, N = N, y = y, O = O)
    Final_O <- objj$Final_O
    O <- objj$O
    season_lab <- input$season
    library("lattice")
    palf <-colorRampPalette(c("green3", "yellow", "red1"))
    levelplot(t(O[nrow(O):1,]),
              col.regions=palf(100), xlab = NULL, ylab = NULL, colorkey = FALSE,
              main =  list(label=paste0("Tabela wyników: ", season_lab), cex=2.4),
              scales = list(list(alternating=1),x=list(cex = 1,rot=45),y=list(cex=0.8)),)

  })

  output$table <- renderTable({
    df()
  })

  output$NoisyPlot <- renderPlot({
    O <- ConvFun(dataset())
    N <- nrow(O)
    y <- to_adjacency(O, N)
    season <- input$season
    objj <- mcmc(season = season, N = N, y = y, O = O)
    Noisy_True_K <- objj$Noisy_True_K
    K_maxmax <- objj$K_maxmax
    plot(Noisy_True_K, col="#00000033",ylab="K", xlab=" ", cex.lab=1.7,
         ylim=c(1,K_maxmax+1), yaxt="n")
  })
}

shinyApp(ui = ui, server = server)
