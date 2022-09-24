
# df <- rbindlist(list(df_11_12, df_12_13, df_13_14, df_14_15, df_15_16, df_16_17,
#                      df_17_18, df_18_19, df_19_20, df_20_21, df_21_22, df_22_23))
df <- do.call("rbind", list(df_11_12, df_12_13, df_13_14, df_14_15, df_15_16, df_16_17,
                            df_17_18, df_18_19, df_19_20, df_20_21, df_21_22, df_22_23))

list_of_clubs <- unique(df$team)

df1 <- data.frame(club = list_of_clubs, season11.12 = 0)

df1$club[12] <- "Podbeskidzie BB"
df1$club[21] <- "BrukBet T."


#12/13
for(i in 1:nrow(df1)){
  if(df1$club[i] %in% df_11_12$team){
    df1$season12.13[i] <- df1$season11.12[i] + 1
  } else{
    df1$season12.13[i] <- df1$season11.12[i]
  }
}

#13/14
for(i in 1:nrow(df1)){
  if(df1$club[i] %in% df_12_13$team){
    df1$season13.14[i] <- df1$season12.13[i] + 1
  } else{
    df1$season13.14[i] <- df1$season12.13[i]
  }
}

#14/15 .
for(i in 1:nrow(df1)){
  if(df1$club[i] %in% df_13_14$team){
    df1$season14.15[i] <- df1$season13.14[i] + 1
  } else{
    df1$season14.15[i] <- df1$season13.14[i]
  }
}

#15/16
for(i in 1:nrow(df1)){
  if(df1$club[i] %in% df_14_15$team){
    df1$season15.16[i] <- df1$season14.15[i] + 1
  } else{
    df1$season15.16[i] <- df1$season14.15[i]
  }
}

#16/17
for(i in 1:nrow(df1)){
  if(df1$club[i] %in% df_15_16$team){
    df1$season16.17[i] <- df1$season15.16[i] + 1
  } else{
    df1$season16.17[i] <- df1$season15.16[i]
  }
}

#17/18
for(i in 1:nrow(df1)){
  if(df1$club[i] %in% df_16_17$team){
    df1$season17.18[i] <- df1$season16.17[i] + 1
  } else{
    df1$season17.18[i] <- df1$season16.17[i]
  }
}

#18/19
for(i in 1:nrow(df1)){
  if(df1$club[i] %in% df_17_18$team){
    df1$season18.19[i] <- df1$season17.18[i] + 1
  } else{
    df1$season18.19[i] <- df1$season17.18[i]
  }
}

#19/20
for(i in 1:nrow(df1)){
  if(df1$club[i] %in% df_18_19$team){
    df1$season19.20[i] <- df1$season18.19[i] + 1
  } else{
    df1$season19.20[i] <- df1$season18.19[i]
  }
}

#20/21
for(i in 1:nrow(df1)){
  if(df1$club[i] %in% df_19_20$team){
    df1$season20.21[i] <- df1$season19.20[i] + 1
  } else{
    df1$season20.21[i] <- df1$season19.20[i]
  }
}

#21/22
for(i in 1:nrow(df1)){
  if(df1$club[i] %in% df_20_21$team){
    df1$season21.22[i] <- df1$season20.21[i] + 1
  } else{
    df1$season21.22[i] <- df1$season20.21[i]
  }
}

#22/23
for(i in 1:nrow(df1)){
  if(df1$club[i] %in% df_21_22$team){
    df1$season22.23[i] <- df1$season21.22[i] + 1
  } else{
    df1$season22.23[i] <- df1$season21.22[i]
  }
}


