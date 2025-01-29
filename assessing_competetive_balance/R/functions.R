# Internal functions using during the cleaning and data analysis

cleaningFun <- function(data) {

  data$home <- data$home %>% as.numeric()
  data$away <- data$away %>% as.numeric()
  A <- data %>% group_by(home_teams, away_teams) %>% mutate(n = n()) %>%  filter(n==2) %>% summarise(across(c(home, away), sum))
  B <- data %>% group_by(home_teams, away_teams) %>% mutate(n = n()) %>% filter(n==1)
  cleanedData <- rbind(A, B) %>% select(-n) %>% mutate(result = ifelse(home > away, "H", ifelse(away > home, "A", "D")))
  cleanedData

}

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

  plt <- levelplot(t(data[c(nrow(data):1), ]), scales=list(x=list(rot=45)),
                   col.regions = colorRampPalette(c("green", "yellow", "red"))(100),
                   colorkey=FALSE,
                   xlab = "Away Team", ylab = "Home Team")

  plt

}

#Function for the adjacency matrix from O
to_adjacency<-function(O_matrix, N){
  N_nodes=dim(O_matrix)[1]
  adja_y=array(data=NA, dim=c(N,N,3))
  for(i in 1:N_nodes){
    for(j in 1:N_nodes){
      #do not consider self loops
      if(i!=j){
        # 18/05/22 not considering NAs
        if(is.na(O_matrix[i,j]))
          adja_y[i,j,]=c(0,0,0)
        else{
          #value 1 for W
          if(O_matrix[i,j]==1)
            adja_y[i,j,]=c(1,0,0)
          #Draw
          if(O_matrix[i,j]==2)
            adja_y[i,j,]=c(0,1,0)
          #Loss
          if(O_matrix[i,j]==3)
            adja_y[i,j,]=c(0,0,1)
        }
      }
    }
  }
  return(adja_y)
}
