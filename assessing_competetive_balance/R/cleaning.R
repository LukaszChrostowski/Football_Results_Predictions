library(rlist)
library(dplyr)
library(tidyr)

ssn01_02 <- cleaningFun(ssn01_02)
ssn13_14 <- cleaningFun(ssn13_14)
ssn14_15 <- cleaningFun(ssn14_15)
ssn15_16 <- cleaningFun(ssn15_16)
ssn16_17 <- cleaningFun(ssn16_17)
ssn17_18 <- cleaningFun(ssn17_18)
ssn18_19 <- cleaningFun(ssn18_19)
ssn19_20 <- cleaningFun(ssn19_20)

A <- list(ssn98_99, ssn99_00,ssn00_01, ssn01_02, ssn02_03, ssn03_04, ssn04_05, ssn05_06, ssn06_07, ssn07_08,
          ssn08_09, ssn09_10, ssn10_11, ssn11_12, ssn12_13, ssn13_14, ssn14_15, ssn15_16,
          ssn16_17, ssn17_18, ssn18_19, ssn19_20, ssn20_21, ssn21_22, ssn22_23, ssn23_24)

B <- list()


for (df in A) {

  final_table <- df %>% select(c(home_teams, away_teams, result))

  final_table <- final_table %>% pivot_wider(names_from = away_teams, values_from = result) %>% as.data.frame()
  col_order <- final_table$home_teams
  rownames(final_table) <- final_table$home_teams
  final_table$home_teams <- NULL
  final_table <- final_table[, col_order]

  B <- list.append(B, final_table)

}

ContTable_9899 <- B[[1]]
ContTable_9900 <- B[[2]]
ContTable_0001 <- B[[3]]
ContTable_0102 <- B[[4]]
ContTable_0203 <- B[[5]]
ContTable_0304 <- B[[6]]
ContTable_0405 <- B[[7]]
ContTable_0506 <- B[[8]]
ContTable_0607 <- B[[9]]
ContTable_0708 <- B[[10]]
ContTable_0809 <- B[[11]]
ContTable_0910 <- B[[12]]
ContTable_1011 <- B[[13]]
ContTable_1112 <- B[[14]]
ContTable_1213 <- B[[15]]
ContTable_1314 <- B[[16]]
ContTable_1415 <- B[[17]]
ContTable_1516 <- B[[18]]
ContTable_1617 <- B[[19]]
ContTable_1718 <- B[[20]]
ContTable_1819 <- B[[21]]
ContTable_1920 <- B[[22]]
ContTable_2021 <- B[[23]]
ContTable_2122 <- B[[24]]
ContTable_2223 <- B[[25]]
ContTable_2324 <- B[[26]]

save(ContTable_9899,
     ContTable_9900,
     ContTable_0001,
     ContTable_0102,
     ContTable_0203,
     ContTable_0304,
     ContTable_0405,
     ContTable_0506,
     ContTable_0607,
     ContTable_0708,
     ContTable_0809,
     ContTable_0910,
     ContTable_1011,
     ContTable_1112,
     ContTable_1213,
     ContTable_1314,
     ContTable_1415,
     ContTable_1516,
     ContTable_1617,
     ContTable_1718,
     ContTable_1819,
     ContTable_1920,
     ContTable_2021,
     ContTable_2122,
     ContTable_2223,
     ContTable_2324,
     file = "contTables.Rdata")

