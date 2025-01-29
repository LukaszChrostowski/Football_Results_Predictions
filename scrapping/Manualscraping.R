# load packages
library(tidyverse)
library(RSelenium)
library(netstat)

# start the server

# rs_driver_object <- rsDriver(browser = 'chrome',
#                              chromever = "106.0.5249.21",
#                              verbose = FALSE,
#                              port = free_port())

rs_driver_object <- rsDriver(browser = "firefox",
                             #verbose = FALSE,
                             #chromever = "131.0.6778.265", # the lastest
                             port = free_port())

# create a client object
remDr <- rs_driver_object$client

# open a browser
remDr$open()

# maximize window
remDr$maxWindowSize()

urls <- list("https://www.wyniki.pl/pko-bp-ekstraklasa-2011-2012/#/zXrc8SIB/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2012-2013/#/4hOhngVE/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2013-2014/#/MFbMedql/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2014-2015/#/CUfFTpEE/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2015-2016/#/86mTH0UC/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2016-2017/#/hYVk7Jqe/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2017-2018/#/bZWyoJnA/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2018-2019/#/dhoVcL5r/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2019-2020/#/v5p2SRke/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2020-2021/#/0YeuAKmU/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2021-2022/#/noYKsAu8/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2022-2023/#/4fofM1vn/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2023-2024/#/EsvRI4zf/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa/#/Qu6CIhxL/table/overall") # current one


### manually ###

#11/12


remDr$navigate(urls[[1]])

#cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
#cookies$clickElement()

Tabela <- remDr$findElement(using = 'class', 'standings_table')
Tabela$clickElement()

miejsce <- remDr$findElements(using = "class", "tableCellRank")
miejsca <- lapply(miejsce, function(x) x$getElementText()) %>% unlist()

Teams <- remDr$findElements(using = "class", "tableCellParticipant__name")
Team <- lapply(Teams, function(x) x$getElementText() %>% unlist()) %>% unlist()

Points <- remDr$findElements(using = "class", "table__cell--points")
Points <- lapply(Points, function(x) x$getElementText()) %>% unlist()
Points <- as.numeric(Points[-1])

df_11_12 <- data.frame(team = Team, place = miejsca, points = Points)

df_11_12$place <- df_11_12$place %>% str_replace_all("[.]", "") %>% as.numeric()
df_11_12$season <- rep("11/12", nrow(df_11_12))

#12/13

remDr$navigate(urls[[2]])

#cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
#cookies$clickElement()

Tabela <- remDr$findElement(using = 'class', 'standings_table')
Tabela$clickElement()

miejsce <- remDr$findElements(using = "class", "tableCellRank")
miejsca <- lapply(miejsce, function(x) x$getElementText()) %>% unlist()

Teams <- remDr$findElements(using = "class", "tableCellParticipant__name")
Team <- lapply(Teams, function(x) x$getElementText()) %>% unlist()


Points <- remDr$findElements(using = "class", "table__cell--points")
Points <- lapply(Points, function(x) x$getElementText()) %>% unlist()
Points <- as.numeric(Points[-1])

df_12_13 <- data.frame(team = Team, place = miejsca, points = Points)

df_12_13$place <- df_12_13$place %>% str_replace_all("[.]", "") %>% as.numeric()
df_12_13$season <- rep("12/13", nrow(df_12_13))

#13/14

remDr$navigate(urls[[3]])

#cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
#cookies$clickElement()

Tabela <- remDr$findElement(using = 'class', 'standings_table')
Tabela$clickElement()

miejsce <- remDr$findElements(using = "class", "tableCellRank")
miejsca <- lapply(miejsce, function(x) x$getElementText()) %>% unlist()

Teams <- remDr$findElements(using = "class", "tableCellParticipant__name")
Team <- lapply(Teams, function(x) x$getElementText()) %>% unlist()

Points <- remDr$findElements(using = "class", "table__cell--points")
Points <- lapply(Points, function(x) x$getElementText()) %>% unlist()
Points <- as.numeric(Points[-1])

df_13_14 <- data.frame(team = Team, place = miejsca, points = Points)

df_13_14$place <- df_13_14$place %>% str_replace_all("[.]", "") %>% as.numeric()
df_13_14$season <- rep("13/14", nrow(df_13_14))

#14/15


remDr$navigate(urls[[4]])

#cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
#cookies$clickElement()

Tabela <- remDr$findElement(using = 'class', 'standings_table')
Tabela$clickElement()

miejsce <- remDr$findElements(using = "class", "tableCellRank")
miejsca <- lapply(miejsce, function(x) x$getElementText()) %>% unlist()

Teams <- remDr$findElements(using = "class", "tableCellParticipant__name")
Team <- lapply(Teams, function(x) x$getElementText()) %>% unlist()


Points <- remDr$findElements(using = "class", "table__cell--points")
Points <- lapply(Points, function(x) x$getElementText()) %>% unlist()
Points <- as.numeric(Points[-1])

df_14_15 <- data.frame(team = Team, place = miejsca, points = Points)

df_14_15$place <- df_14_15$place %>% str_replace_all("[.]", "") %>% as.numeric()
df_14_15$season <- rep("14/15", nrow(df_14_15))

#15/16

remDr$navigate(urls[[5]])

#cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
#cookies$clickElement()

Tabela <- remDr$findElement(using = 'class', 'standings_table')
Tabela$clickElement()

miejsce <- remDr$findElements(using = "class", "tableCellRank")
miejsca <- lapply(miejsce, function(x) x$getElementText()) %>% unlist()

Teams <- remDr$findElements(using = "class", "tableCellParticipant__name")
Team <- lapply(Teams, function(x) x$getElementText()) %>% unlist()


Points <- remDr$findElements(using = "class", "table__cell--points")
Points <- lapply(Points, function(x) x$getElementText()) %>% unlist()
Points <- as.numeric(Points[-1])

df_15_16 <- data.frame(team = Team, place = miejsca, points = Points)

df_15_16$place <- df_15_16$place %>% str_replace_all("[.]", "") %>% as.numeric()
df_15_16$season <- rep("15/16", nrow(df_15_16))

#16/17
remDr$navigate(urls[[6]])

#cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
#cookies$clickElement()

Tabela <- remDr$findElement(using = 'class', 'standings_table')
Tabela$clickElement()

miejsce <- remDr$findElements(using = "class", "tableCellRank")
miejsca <- lapply(miejsce, function(x) x$getElementText()) %>% unlist()

Teams <- remDr$findElements(using = "class", "tableCellParticipant__name")
Team <- lapply(Teams, function(x) x$getElementText()) %>% unlist()


Points <- remDr$findElements(using = "class", "table__cell--points")
Points <- lapply(Points, function(x) x$getElementText()) %>% unlist()
Points <- as.numeric(Points[-1])

df_16_17 <- data.frame(team = Team, place = miejsca, points = Points)

df_16_17$place <- df_16_17$place %>% str_replace_all("[.]", "") %>% as.numeric()
df_16_17$season <- rep("16/17", nrow(df_16_17))
#17/18
remDr$navigate(urls[[7]])

#cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
#cookies$clickElement()

Tabela <- remDr$findElement(using = 'class', 'standings_table')
Tabela$clickElement()

miejsce <- remDr$findElements(using = "class", "tableCellRank")
miejsca <- lapply(miejsce, function(x) x$getElementText()) %>% unlist()

Teams <- remDr$findElements(using = "class", "tableCellParticipant__name")
Team <- lapply(Teams, function(x) x$getElementText()) %>% unlist()


Points <- remDr$findElements(using = "class", "table__cell--points")
Points <- lapply(Points, function(x) x$getElementText()) %>% unlist()
Points <- as.numeric(Points[-1])

df_17_18 <- data.frame(team = Team, place = miejsca, points = Points)

df_17_18$place <- df_17_18$place %>% str_replace_all("[.]", "") %>% as.numeric()
df_17_18$season <- rep("17/18", nrow(df_17_18))

#18/19

remDr$navigate(urls[[8]])

#cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
#cookies$clickElement()

Tabela <- remDr$findElement(using = 'class', 'standings_table')
Tabela$clickElement()

miejsce <- remDr$findElements(using = "class", "tableCellRank")
miejsca <- lapply(miejsce, function(x) x$getElementText()) %>% unlist()

Teams <- remDr$findElements(using = "class", "tableCellParticipant__name")
Team <- lapply(Teams, function(x) x$getElementText()) %>% unlist()


Points <- remDr$findElements(using = "class", "table__cell--points")
Points <- lapply(Points, function(x) x$getElementText()) %>% unlist()
Points <- as.numeric(Points[-1])

df_18_19 <- data.frame(team = Team, place = miejsca, points = Points)

df_18_19$place <- df_18_19$place %>% str_replace_all("[.]", "") %>% as.numeric()
df_18_19$season <- rep("18/19", nrow(df_18_19))
#19/20

remDr$navigate(urls[[9]])

#cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
#cookies$clickElement()

Tabela <- remDr$findElement(using = 'class', 'standings_table')
Tabela$clickElement()

miejsce <- remDr$findElements(using = "class", "tableCellRank")
miejsca <- lapply(miejsce, function(x) x$getElementText()) %>% unlist()

Teams <- remDr$findElements(using = "class", "tableCellParticipant__name")
Team <- lapply(Teams, function(x) x$getElementText()) %>% unlist()


Points <- remDr$findElements(using = "class", "table__cell--points")
Points <- lapply(Points, function(x) x$getElementText()) %>% unlist()
Points <- as.numeric(Points[-1])

df_19_20 <- data.frame(team = Team, place = miejsca, points = Points)

df_19_20$place <- df_19_20$place %>% str_replace_all("[.]", "") %>% as.numeric()
df_19_20$season <- rep("19/20", nrow(df_19_20))

#20/21

remDr$navigate(urls[[10]])

#cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
#cookies$clickElement()

Tabela <- remDr$findElement(using = 'class', 'standings_table')
Tabela$clickElement()

miejsce <- remDr$findElements(using = "class", "tableCellRank")
miejsca <- lapply(miejsce, function(x) x$getElementText()) %>% unlist()

Teams <- remDr$findElements(using = "class", "tableCellParticipant__name")
Team <- lapply(Teams, function(x) x$getElementText()) %>% unlist()

Points <- remDr$findElements(using = "class", "table__cell--points")
Points <- lapply(Points, function(x) x$getElementText()) %>% unlist()
Points <- as.numeric(Points[-1])

df_20_21 <- data.frame(team = Team, place = miejsca, points = Points)

df_20_21$place <- df_20_21$place %>% str_replace_all("[.]", "") %>% as.numeric()
df_20_21$season <- rep("20/21", nrow(df_20_21))


#21/22

remDr$navigate(urls[[11]])

#cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
#cookies$clickElement()

Tabela <- remDr$findElement(using = 'class', 'standings_table')
Tabela$clickElement()

miejsce <- remDr$findElements(using = "class", "tableCellRank")
miejsca <- lapply(miejsce, function(x) x$getElementText()) %>% unlist()

Teams <- remDr$findElements(using = "class", "tableCellParticipant__name")
Team <- lapply(Teams, function(x) x$getElementText()) %>% unlist()

Points <- remDr$findElements(using = "class", "table__cell--points")
Points <- lapply(Points, function(x) x$getElementText()) %>% unlist()
Points <- as.numeric(Points[-1])

df_21_22 <- data.frame(team = Team, place = miejsca, points = Points)

df_21_22$place <- df_21_22$place %>% str_replace_all("[.]", "") %>% as.numeric()
df_21_22$season <- rep("21/22", nrow(df_21_22))

#22/23

remDr$navigate(urls[[12]])

#cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
#cookies$clickElement()

Tabela <- remDr$findElement(using = 'class', 'standings_table')
Tabela$clickElement()

miejsce <- remDr$findElements(using = "class", "tableCellRank")
miejsca <- lapply(miejsce, function(x) x$getElementText()) %>% unlist()

Teams <- remDr$findElements(using = "class", "tableCellParticipant__name")
Team <- lapply(Teams, function(x) x$getElementText()) %>% unlist()

Points <- remDr$findElements(using = "class", "table__cell--points")
Points <- lapply(Points, function(x) x$getElementText()) %>% unlist()
Points <- as.numeric(Points[-1])

df_22_23 <- data.frame(team = Team, place = miejsca, points = Points)

df_22_23$place <- df_22_23$place %>% str_replace_all("[.]", "") %>% as.numeric()
df_22_23$season <- rep("22/23", nrow(df_22_23))

# 23/24

remDr$navigate(urls[[13]])

#cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
#cookies$clickElement()

Tabela <- remDr$findElement(using = 'class', 'standings_table')
Tabela$clickElement()

miejsce <- remDr$findElements(using = "class", "tableCellRank")
miejsca <- lapply(miejsce, function(x) x$getElementText()) %>% unlist()

Teams <- remDr$findElements(using = "class", "tableCellParticipant__name")
Team <- lapply(Teams, function(x) x$getElementText()) %>% unlist()

Points <- remDr$findElements(using = "class", "table__cell--points")
Points <- lapply(Points, function(x) x$getElementText()) %>% unlist()
Points <- as.numeric(Points[-1])

df_23_24 <- data.frame(team = Team, place = miejsca, points = Points)

df_23_24$place <- df_23_24$place %>% str_replace_all("[.]", "") %>% as.numeric()
df_23_24$season <- rep("23/24", nrow(df_23_24))

# 24/25

remDr$navigate(urls[[14]])

#cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
#cookies$clickElement()

Tabela <- remDr$findElement(using = 'class', 'standings_table')
Tabela$clickElement()

miejsce <- remDr$findElements(using = "class", "tableCellRank")
miejsca <- lapply(miejsce, function(x) x$getElementText()) %>% unlist()

Teams <- remDr$findElements(using = "class", "tableCellParticipant__name")
Team <- lapply(Teams, function(x) x$getElementText()) %>% unlist()

Points <- remDr$findElements(using = "class", "table__cell--points")
Points <- lapply(Points, function(x) x$getElementText()) %>% unlist()
Points <- as.numeric(Points[-1])

df_24_25 <- data.frame(team = Team, place = miejsca, points = Points)

df_24_25$place <- df_24_25$place %>% str_replace_all("[.]", "") %>% as.numeric()
df_24_25$season <- rep("24/25", nrow(df_24_25))


# Save multiple objects
save(df_11_12,
     df_12_13,
     df_13_14,
     df_14_15,
     df_15_16,
     df_16_17,
     df_17_18,
     df_18_19,
     df_19_20,
     df_20_21,
     df_21_22,
     df_22_23,
     df_23_24,
     df_24_25,
     file = "standings_tables.RData")

# To load the data again
load("standings_tables.RData")




