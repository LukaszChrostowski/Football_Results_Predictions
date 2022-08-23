# load packages
library(tidyverse)
library(RSelenium)
library(netstat)

# start the server
rs_driver_object <- rsDriver(browser = 'chrome',
                             chromever = '103.0.5060.134',
                             verbose = FALSE,
                             port = free_port())

# create a client object
remDr <- rs_driver_object$client

# open a browser
remDr$open()

# maximize window
remDr$maxWindowSize()

urls <- list("https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2011-2012/",
             "https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2012-2013/",
             "https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2013-2014/",
             "https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2014-2015/",
             "https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2015-2016/",
             "https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2016-2017/",
             "https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2017-2018/",
             "https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2018-2019/",
             "https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2019-2020/",
             "https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2020-2021/",
             "https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2021-2022/",
             "https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2022-2023/")
#11/12

remDr$navigate(urls[[1]])

cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
cookies$clickElement()

Tabela <- remDr$findElement(using = 'class', 'standings_table')
Tabela$clickElement()

miejsce <- remDr$findElements(using = "class", "tableCellRank")
miejsca <- lapply(miejsce, function(x) x$getElementText()) %>% unlist()

Teams <- remDr$findElements(using = "class", "tableCellParticipant__name")
Team <- lapply(Teams, function(x) x$getElementText()) %>% unlist()


df_11_12 <- data.frame(team = Team, place = miejsca)

df_11_12$place <- df_11_12$place %>% str_replace_all("[.]", "") %>% as.numeric()

#12/13
  
  remDr$navigate(urls[[2]])
  
  cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
  cookies$clickElement()
  
  Tabela <- remDr$findElement(using = 'class', 'standings_table')
  Tabela$clickElement()
  
  miejsce <- remDr$findElements(using = "class", "tableCellRank")
  miejsca <- lapply(miejsce, function(x) x$getElementText()) %>% unlist()
  
  Teams <- remDr$findElements(using = "class", "tableCellParticipant__name")
  Team <- lapply(Teams, function(x) x$getElementText()) %>% unlist()


df_12_13 <- data.frame(team = Team, place = miejsca)

df_12_13$place <- df_12_13$place %>% str_replace_all("[.]", "") %>% as.numeric()

#13/14

remDr$navigate(urls[[3]])

cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
cookies$clickElement()

Tabela <- remDr$findElement(using = 'class', 'standings_table')
Tabela$clickElement()

miejsce <- remDr$findElements(using = "class", "tableCellRank")
miejsca <- lapply(miejsce, function(x) x$getElementText()) %>% unlist()

Teams <- remDr$findElements(using = "class", "tableCellParticipant__name")
Team <- lapply(Teams, function(x) x$getElementText()) %>% unlist()

df_13_14 <- data.frame(team = Team, place = miejsca)

df_13_14$place <- df_13_14$place %>% str_replace_all("[.]", "") %>% as.numeric()

#14/15


remDr$navigate(urls[[4]])

cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
cookies$clickElement()

Tabela <- remDr$findElement(using = 'class', 'standings_table')
Tabela$clickElement()

miejsce <- remDr$findElements(using = "class", "tableCellRank")
miejsca <- lapply(miejsce, function(x) x$getElementText()) %>% unlist()

Teams <- remDr$findElements(using = "class", "tableCellParticipant__name")
Team <- lapply(Teams, function(x) x$getElementText()) %>% unlist()


df_14_15 <- data.frame(team = Team, place = miejsca)

df_14_15$place <- df_14_15$place %>% str_replace_all("[.]", "") %>% as.numeric()

#15/16

remDr$navigate(urls[[5]])

cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
cookies$clickElement()

Tabela <- remDr$findElement(using = 'class', 'standings_table')
Tabela$clickElement()

miejsce <- remDr$findElements(using = "class", "tableCellRank")
miejsca <- lapply(miejsce, function(x) x$getElementText()) %>% unlist()

Teams <- remDr$findElements(using = "class", "tableCellParticipant__name")
Team <- lapply(Teams, function(x) x$getElementText()) %>% unlist()


df_15_16 <- data.frame(team = Team, place = miejsca)

df_15_16$place <- df_15_16$place %>% str_replace_all("[.]", "") %>% as.numeric()

#16/17
remDr$navigate(urls[[6]])

cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
cookies$clickElement()

Tabela <- remDr$findElement(using = 'class', 'standings_table')
Tabela$clickElement()

miejsce <- remDr$findElements(using = "class", "tableCellRank")
miejsca <- lapply(miejsce, function(x) x$getElementText()) %>% unlist()

Teams <- remDr$findElements(using = "class", "tableCellParticipant__name")
Team <- lapply(Teams, function(x) x$getElementText()) %>% unlist()


df_16_17 <- data.frame(team = Team, place = miejsca)

df_16_17$place <- df_16_17$place %>% str_replace_all("[.]", "") %>% as.numeric()

#17/18
remDr$navigate(urls[[7]])

cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
cookies$clickElement()

Tabela <- remDr$findElement(using = 'class', 'standings_table')
Tabela$clickElement()

miejsce <- remDr$findElements(using = "class", "tableCellRank")
miejsca <- lapply(miejsce, function(x) x$getElementText()) %>% unlist()

Teams <- remDr$findElements(using = "class", "tableCellParticipant__name")
Team <- lapply(Teams, function(x) x$getElementText()) %>% unlist()


df_17_18 <- data.frame(team = Team, place = miejsca)

df_17_18$place <- df_17_18$place %>% str_replace_all("[.]", "") %>% as.numeric()

#18/19

remDr$navigate(urls[[8]])

cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
cookies$clickElement()

Tabela <- remDr$findElement(using = 'class', 'standings_table')
Tabela$clickElement()

miejsce <- remDr$findElements(using = "class", "tableCellRank")
miejsca <- lapply(miejsce, function(x) x$getElementText()) %>% unlist()

Teams <- remDr$findElements(using = "class", "tableCellParticipant__name")
Team <- lapply(Teams, function(x) x$getElementText()) %>% unlist()


df_18_19 <- data.frame(team = Team, place = miejsca)

df_18_19$place <- df_18_19$place %>% str_replace_all("[.]", "") %>% as.numeric()

#19/20

remDr$navigate(urls[[9]])

cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
cookies$clickElement()

Tabela <- remDr$findElement(using = 'class', 'standings_table')
Tabela$clickElement()

miejsce <- remDr$findElements(using = "class", "tableCellRank")
miejsca <- lapply(miejsce, function(x) x$getElementText()) %>% unlist()

Teams <- remDr$findElements(using = "class", "tableCellParticipant__name")
Team <- lapply(Teams, function(x) x$getElementText()) %>% unlist()


df_19_20 <- data.frame(team = Team, place = miejsca)

df_19_20$place <- df_19_20$place %>% str_replace_all("[.]", "") %>% as.numeric()

#20/21

remDr$navigate(urls[[10]])

cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
cookies$clickElement()

Tabela <- remDr$findElement(using = 'class', 'standings_table')
Tabela$clickElement()

miejsce <- remDr$findElements(using = "class", "tableCellRank")
miejsca <- lapply(miejsce, function(x) x$getElementText()) %>% unlist()

Teams <- remDr$findElements(using = "class", "tableCellParticipant__name")
Team <- lapply(Teams, function(x) x$getElementText()) %>% unlist()


df_20_21 <- data.frame(team = Team, place = miejsca)

df_20_21$place <- df_20_21$place %>% str_replace_all("[.]", "") %>% as.numeric()

#21/22

remDr$navigate(urls[[11]])

cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
cookies$clickElement()

Tabela <- remDr$findElement(using = 'class', 'standings_table')
Tabela$clickElement()

miejsce <- remDr$findElements(using = "class", "tableCellRank")
miejsca <- lapply(miejsce, function(x) x$getElementText()) %>% unlist()

Teams <- remDr$findElements(using = "class", "tableCellParticipant__name")
Team <- lapply(Teams, function(x) x$getElementText()) %>% unlist()


df_21_22 <- data.frame(team = Team, place = miejsca)

df_21_22$place <- df_21_22$place %>% str_replace_all("[.]", "") %>% as.numeric()

#22/23

remDr$navigate(urls[[12]])

cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
cookies$clickElement()

Tabela <- remDr$findElement(using = 'class', 'standings_table')
Tabela$clickElement()

miejsce <- remDr$findElements(using = "class", "tableCellRank")
miejsca <- lapply(miejsce, function(x) x$getElementText()) %>% unlist()

Teams <- remDr$findElements(using = "class", "tableCellParticipant__name")
Team <- lapply(Teams, function(x) x$getElementText()) %>% unlist()


df_22_23 <- data.frame(team = Team, place = miejsca)

df_22_23$place <- df_22_23$place %>% str_replace_all("[.]", "") %>% as.numeric()





