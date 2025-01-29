# load packages
library(tidyverse)
library(RSelenium)
library(netstat)
library(dplyr)

seleniumServer <- rsDriver(browser = "chrome",
                           #verbose = FALSE,
                           port = free_port(),
                           chromever = "106.0.5249.21")
# Client object
remDr <- seleniumServer$client

remDr$maxWindowSize()

# 98/99
url <- "https://www.wyniki.pl/pko-bp-ekstraklasa-1998-1999/wyniki/"

remDr$navigate(url)
# click on cookies info:
cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
cookies$clickElement()
# click on more info:
# get urls to match statistics

#remDr$executeScript("window.scrollTo(0, document.body.scrollHeight);")

more_games <- remDr$findElement(using = "link text", "Pokaż więcej meczów")
more_games$clickElement()
more_games$clickElement()
home <- remDr$findElements(using = "class", "event__score--home")
home <- lapply(home, function(x) x$getElementText() %>% unlist()) %>% unlist() %>% as.numeric()

away <- remDr$findElements(using = "class", "event__score--away")
away <- lapply(away, function(x) x$getElementText() %>% unlist()) %>% unlist() %>% as.numeric()

home_teams <- remDr$findElements(using = "class", "event__participant--home")
home_teams <- lapply(home_teams, function(x) x$getElementText() %>% unlist()) %>% unlist()

away_teams <- remDr$findElements(using = "class", "event__participant--away")
away_teams <- lapply(away_teams, function(x) x$getElementText() %>% unlist()) %>% unlist()

result <- ifelse(home > away, "H", ifelse(away > home, "A", "D"))


ssn98_99 <- data.frame(home = home,
                       away = away,
                       home_teams = home_teams,
                       away_teams = away_teams,
                       result = result)

#99/00

url <- "https://www.wyniki.pl/pko-bp-ekstraklasa-1999-2000/wyniki/"


remDr$maxWindowSize()
remDr$navigate(url)
# click on cookies info:
#cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
#cookies$clickElement()
# click on more info:
# get urls to match statistics

more_games <- remDr$findElement(using = "link text", "Pokaż więcej meczów")
more_games$clickElement()
more_games$clickElement()

home <- remDr$findElements(using = "class", "event__score--home")
home <- lapply(home, function(x) x$getElementText() %>% unlist()) %>% unlist() %>% as.numeric()

away <- remDr$findElements(using = "class", "event__score--away")
away <- lapply(away, function(x) x$getElementText() %>% unlist()) %>% unlist() %>% as.numeric()

home_teams <- remDr$findElements(using = "class", "event__participant--home")
home_teams <- lapply(home_teams, function(x) x$getElementText() %>% unlist()) %>% unlist()

away_teams <- remDr$findElements(using = "class", "event__participant--away")
away_teams <- lapply(away_teams, function(x) x$getElementText() %>% unlist()) %>% unlist()

result <- ifelse(home > away, "H", ifelse(away > home, "A", "D"))


ssn99_00 <- data.frame(home = home,
                       away = away,
                       home_teams = home_teams,
                       away_teams = away_teams,
                       result = result)
#00/01
url <- "https://www.wyniki.pl/pko-bp-ekstraklasa-2000-2001/wyniki/"

remDr$navigate(url)
# click on cookies info:
#cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
#cookies$clickElement()
# click on more info:
# get urls to match statistics
more_games <- remDr$findElement(using = "link text", "Pokaż więcej meczów")
more_games$clickElement()
more_games$clickElement()


home <- remDr$findElements(using = "class", "event__score--home")
home <- lapply(home, function(x) x$getElementText() %>% unlist()) %>% unlist() %>% as.numeric()

away <- remDr$findElements(using = "class", "event__score--away")
away <- lapply(away, function(x) x$getElementText() %>% unlist()) %>% unlist() %>% as.numeric()

home_teams <- remDr$findElements(using = "class", "event__participant--home")
home_teams <- lapply(home_teams, function(x) x$getElementText() %>% unlist()) %>% unlist()

away_teams <- remDr$findElements(using = "class", "event__participant--away")
away_teams <- lapply(away_teams, function(x) x$getElementText() %>% unlist()) %>% unlist()

result <- ifelse(home > away, "H", ifelse(away > home, "A", "D"))


ssn00_01 <- data.frame(home = home,
                       away = away,
                       home_teams = home_teams,
                       away_teams = away_teams,
                       result = result)



# 2001/2002 ####
url <- "https://www.wyniki.pl/pko-bp-ekstraklasa-2001-2002/wyniki/"

remDr$navigate(url)
# click on cookies info:
#cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
#cookies$clickElement()
# click on more info:
# get urls to match statistics
more_games <- remDr$findElement(using = "link text", "Pokaż więcej meczów")
more_games$clickElement()
more_games$clickElement()

home <- remDr$findElements(using = "class", "event__score--home")
home <- lapply(home, function(x) x$getElementText() %>% unlist()) %>% unlist() %>% as.numeric()

away <- remDr$findElements(using = "class", "event__score--away")
away <- lapply(away, function(x) x$getElementText() %>% unlist()) %>% unlist() %>% as.numeric()

home_teams <- remDr$findElements(using = "class", "event__participant--home")
home_teams <- lapply(home_teams, function(x) x$getElementText() %>% unlist()) %>% unlist()

away_teams <- remDr$findElements(using = "class", "event__participant--away")
away_teams <- lapply(away_teams, function(x) x$getElementText() %>% unlist()) %>% unlist()

result <- ifelse(home > away, "H", ifelse(away > home, "A", "D"))


ssn01_02 <- data.frame(home = home,
                       away = away,
                       home_teams = home_teams,
                       away_teams = away_teams,
                       result = result)

#02/03
url <- "https://www.wyniki.pl/pko-bp-ekstraklasa-2002-2003/wyniki/"

remDr$navigate(url)
# click on cookies info:
#cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
#cookies$clickElement()
# click on more info:
# get urls to match statistics
more_games <- remDr$findElement(using = "link text", "Pokaż więcej meczów")
more_games$clickElement()
more_games$clickElement()

home <- remDr$findElements(using = "class", "event__score--home")
home <- lapply(home, function(x) x$getElementText() %>% unlist()) %>% unlist() %>% as.numeric()

away <- remDr$findElements(using = "class", "event__score--away")
away <- lapply(away, function(x) x$getElementText() %>% unlist()) %>% unlist() %>% as.numeric()

home_teams <- remDr$findElements(using = "class", "event__participant--home")
home_teams <- lapply(home_teams, function(x) x$getElementText() %>% unlist()) %>% unlist()

away_teams <- remDr$findElements(using = "class", "event__participant--away")
away_teams <- lapply(away_teams, function(x) x$getElementText() %>% unlist()) %>% unlist()

result <- ifelse(home > away, "H", ifelse(away > home, "A", "D"))


ssn02_03 <- data.frame(home = home,
                       away = away,
                       home_teams = home_teams,
                       away_teams = away_teams,
                       result = result)

#03/04
url <- "https://www.wyniki.pl/pko-bp-ekstraklasa-2003-2004/wyniki/"

remDr$navigate(url)
# click on cookies info:
#cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
#cookies$clickElement()
# click on more info:
# get urls to match statistics
more_games <- remDr$findElement(using = "link text", "Pokaż więcej meczów")
more_games$clickElement()
#more_games$clickElement()

home <- remDr$findElements(using = "class", "event__score--home")
home <- lapply(home, function(x) x$getElementText() %>% unlist()) %>% unlist() %>% as.numeric()

away <- remDr$findElements(using = "class", "event__score--away")
away <- lapply(away, function(x) x$getElementText() %>% unlist()) %>% unlist() %>% as.numeric()

home_teams <- remDr$findElements(using = "class", "event__participant--home")
home_teams <- lapply(home_teams, function(x) x$getElementText() %>% unlist()) %>% unlist()

away_teams <- remDr$findElements(using = "class", "event__participant--away")
away_teams <- lapply(away_teams, function(x) x$getElementText() %>% unlist()) %>% unlist()

result <- ifelse(home > away, "H", ifelse(away > home, "A", "D"))


ssn03_04 <- data.frame(home = home,
                       away = away,
                       home_teams = home_teams,
                       away_teams = away_teams,
                       result = result)

#04/05
url <- "https://www.wyniki.pl/pko-bp-ekstraklasa-2004-2005/wyniki/"

remDr$navigate(url)
# click on cookies info:
#cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
#cookies$clickElement()
# click on more info:
# get urls to match statistics
more_games <- remDr$findElement(using = "link text", "Pokaż więcej meczów")
more_games$clickElement()
#more_games$clickElement()

home <- remDr$findElements(using = "class", "event__score--home")
home <- lapply(home, function(x) x$getElementText() %>% unlist()) %>% unlist() %>% as.numeric()

away <- remDr$findElements(using = "class", "event__score--away")
away <- lapply(away, function(x) x$getElementText() %>% unlist()) %>% unlist() %>% as.numeric()

home_teams <- remDr$findElements(using = "class", "event__participant--home")
home_teams <- lapply(home_teams, function(x) x$getElementText() %>% unlist()) %>% unlist()

away_teams <- remDr$findElements(using = "class", "event__participant--away")
away_teams <- lapply(away_teams, function(x) x$getElementText() %>% unlist()) %>% unlist()

result <- ifelse(home > away, "H", ifelse(away > home, "A", "D"))


ssn04_05 <- data.frame(home = home,
                       away = away,
                       home_teams = home_teams,
                       away_teams = away_teams,
                       result = result)

#05/06
url <- "https://www.wyniki.pl/pko-bp-ekstraklasa-2005-2006/wyniki/"


remDr$navigate(url)
# click on cookies info:
#cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
#cookies$clickElement()
# click on more info:
# get urls to match statistics
more_games <- remDr$findElement(using = "link text", "Pokaż więcej meczów")
more_games$clickElement()
more_games$clickElement()

home <- remDr$findElements(using = "class", "event__score--home")
home <- lapply(home, function(x) x$getElementText() %>% unlist()) %>% unlist() %>% as.numeric()

away <- remDr$findElements(using = "class", "event__score--away")
away <- lapply(away, function(x) x$getElementText() %>% unlist()) %>% unlist() %>% as.numeric()

home_teams <- remDr$findElements(using = "class", "event__participant--home")
home_teams <- lapply(home_teams, function(x) x$getElementText() %>% unlist()) %>% unlist()

away_teams <- remDr$findElements(using = "class", "event__participant--away")
away_teams <- lapply(away_teams, function(x) x$getElementText() %>% unlist()) %>% unlist()

result <- ifelse(home > away, "H", ifelse(away > home, "A", "D"))


ssn05_06 <- data.frame(home = home,
                       away = away,
                       home_teams = home_teams,
                       away_teams = away_teams,
                       result = result)

#06/07
url <- "https://www.wyniki.pl/pko-bp-ekstraklasa-2006-2007/wyniki/"

remDr$navigate(url)
# click on cookies info:
#cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
#cookies$clickElement()
# click on more info:
# get urls to match statistics
more_games <- remDr$findElement(using = "link text", "Pokaż więcej meczów")
more_games$clickElement()
more_games$clickElement()

home <- remDr$findElements(using = "class", "event__score--home")
home <- lapply(home, function(x) x$getElementText() %>% unlist()) %>% unlist() %>% as.numeric()

away <- remDr$findElements(using = "class", "event__score--away")
away <- lapply(away, function(x) x$getElementText() %>% unlist()) %>% unlist() %>% as.numeric()

home_teams <- remDr$findElements(using = "class", "event__participant--home")
home_teams <- lapply(home_teams, function(x) x$getElementText() %>% unlist()) %>% unlist()

away_teams <- remDr$findElements(using = "class", "event__participant--away")
away_teams <- lapply(away_teams, function(x) x$getElementText() %>% unlist()) %>% unlist()

result <- ifelse(home > away, "H", ifelse(away > home, "A", "D"))


ssn06_07 <- data.frame(home = home,
                       away = away,
                       home_teams = home_teams,
                       away_teams = away_teams,
                       result = result)

#07/08
url <- "https://www.wyniki.pl/pko-bp-ekstraklasa-2007-2008/wyniki/"

remDr$navigate(url)
# click on cookies info:
#cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
#cookies$clickElement()
# click on more info:
# get urls to match sta
more_games <- remDr$findElement(using = "link text", "Pokaż więcej meczów")
more_games$clickElement()
more_games$clickElement()


home <- remDr$findElements(using = "class", "event__score--home")
home <- lapply(home, function(x) x$getElementText() %>% unlist()) %>% unlist() %>% as.numeric()

away <- remDr$findElements(using = "class", "event__score--away")
away <- lapply(away, function(x) x$getElementText() %>% unlist()) %>% unlist() %>% as.numeric()

home_teams <- remDr$findElements(using = "class", "event__participant--home")
home_teams <- lapply(home_teams, function(x) x$getElementText() %>% unlist()) %>% unlist()

away_teams <- remDr$findElements(using = "class", "event__participant--away")
away_teams <- lapply(away_teams, function(x) x$getElementText() %>% unlist()) %>% unlist()

result <- ifelse(home > away, "H", ifelse(away > home, "A", "D"))


ssn07_08 <- data.frame(home = home,
                       away = away,
                       home_teams = home_teams,
                       away_teams = away_teams,
                       result = result)

#08/09
url <- "https://www.wyniki.pl/pko-bp-ekstraklasa-2008-2009/wyniki/"


remDr$navigate(url)
# click on cookies info:
#cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
#cookies$clickElement()
# click on more info:
# get urls to match statistics
more_games <- remDr$findElement(using = "link text", "Pokaż więcej meczów")
more_games$clickElement()
more_games$clickElement()

home <- remDr$findElements(using = "class", "event__score--home")
home <- lapply(home, function(x) x$getElementText() %>% unlist()) %>% unlist() %>% as.numeric()

away <- remDr$findElements(using = "class", "event__score--away")
away <- lapply(away, function(x) x$getElementText() %>% unlist()) %>% unlist() %>% as.numeric()

home_teams <- remDr$findElements(using = "class", "event__participant--home")
home_teams <- lapply(home_teams, function(x) x$getElementText() %>% unlist()) %>% unlist()

away_teams <- remDr$findElements(using = "class", "event__participant--away")
away_teams <- lapply(away_teams, function(x) x$getElementText() %>% unlist()) %>% unlist()

result <- ifelse(home > away, "H", ifelse(away > home, "A", "D"))


ssn08_09 <- data.frame(home = home,
                       away = away,
                       home_teams = home_teams,
                       away_teams = away_teams,
                       result = result)

#09/10
url <- "https://www.wyniki.pl/pko-bp-ekstraklasa-2009-2010/wyniki/"


remDr$navigate(url)
# click on cookies info:
#cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
#cookies$clickElement()
# click on more info:
# get urls to match statistics
more_games <- remDr$findElement(using = "link text", "Pokaż więcej meczów")
more_games$clickElement()
more_games$clickElement()

home <- remDr$findElements(using = "class", "event__score--home")
home <- lapply(home, function(x) x$getElementText() %>% unlist()) %>% unlist() %>% as.numeric()

away <- remDr$findElements(using = "class", "event__score--away")
away <- lapply(away, function(x) x$getElementText() %>% unlist()) %>% unlist() %>% as.numeric()

home_teams <- remDr$findElements(using = "class", "event__participant--home")
home_teams <- lapply(home_teams, function(x) x$getElementText() %>% unlist()) %>% unlist()

away_teams <- remDr$findElements(using = "class", "event__participant--away")
away_teams <- lapply(away_teams, function(x) x$getElementText() %>% unlist()) %>% unlist()

result <- ifelse(home > away, "H", ifelse(away > home, "A", "D"))


ssn09_10 <- data.frame(home = home,
                       away = away,
                       home_teams = home_teams,
                       away_teams = away_teams,
                       result = result)
#10/11
url <- "https://www.wyniki.pl/pko-bp-ekstraklasa-2010-2011/wyniki/"


remDr$navigate(url)
# click on cookies info:
#cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
#cookies$clickElement()
# click on more info:
# get urls to match statistics
more_games <- remDr$findElement(using = "link text", "Pokaż więcej meczów")
more_games$clickElement()
more_games$clickElement()

home <- remDr$findElements(using = "class", "event__score--home")
home <- lapply(home, function(x) x$getElementText() %>% unlist()) %>% unlist() %>% as.numeric()

away <- remDr$findElements(using = "class", "event__score--away")
away <- lapply(away, function(x) x$getElementText() %>% unlist()) %>% unlist() %>% as.numeric()

home_teams <- remDr$findElements(using = "class", "event__participant--home")
home_teams <- lapply(home_teams, function(x) x$getElementText() %>% unlist()) %>% unlist()

away_teams <- remDr$findElements(using = "class", "event__participant--away")
away_teams <- lapply(away_teams, function(x) x$getElementText() %>% unlist()) %>% unlist()

result <- ifelse(home > away, "H", ifelse(away > home, "A", "D"))


ssn10_11 <- data.frame(home = home,
                       away = away,
                       home_teams = home_teams,
                       away_teams = away_teams,
                       result = result)
#11/12
url <- "https://www.wyniki.pl/pko-bp-ekstraklasa-2011-2012/wyniki/"

remDr$navigate(url)
# click on cookies info:
#cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
#cookies$clickElement()
# click on more info:
# get urls to match statistics
more_games <- remDr$findElement(using = "link text", "Pokaż więcej meczów")
more_games$clickElement()
more_games$clickElement()

home <- remDr$findElements(using = "class", "event__score--home")
home <- lapply(home, function(x) x$getElementText() %>% unlist()) %>% unlist() %>% as.numeric()

away <- remDr$findElements(using = "class", "event__score--away")
away <- lapply(away, function(x) x$getElementText() %>% unlist()) %>% unlist() %>% as.numeric()

home_teams <- remDr$findElements(using = "class", "event__participant--home")
home_teams <- lapply(home_teams, function(x) x$getElementText() %>% unlist()) %>% unlist()

away_teams <- remDr$findElements(using = "class", "event__participant--away")
away_teams <- lapply(away_teams, function(x) x$getElementText() %>% unlist()) %>% unlist()

result <- ifelse(home > away, "H", ifelse(away > home, "A", "D"))


ssn11_12 <- data.frame(home = home,
                       away = away,
                       home_teams = home_teams,
                       away_teams = away_teams,
                       result = result)

#12/13
url <- "https://www.wyniki.pl/pko-bp-ekstraklasa-2012-2013/wyniki/"

remDr$navigate(url)
# click on cookies info:
#cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
#cookies$clickElement()
# click on more info:
# get urls to match statistics
more_games <- remDr$findElement(using = "link text", "Pokaż więcej meczów")
more_games$clickElement()
more_games$clickElement()

home <- remDr$findElements(using = "class", "event__score--home")
home <- lapply(home, function(x) x$getElementText() %>% unlist()) %>% unlist() %>% as.numeric()

away <- remDr$findElements(using = "class", "event__score--away")
away <- lapply(away, function(x) x$getElementText() %>% unlist()) %>% unlist() %>% as.numeric()

home_teams <- remDr$findElements(using = "class", "event__participant--home")
home_teams <- lapply(home_teams, function(x) x$getElementText() %>% unlist()) %>% unlist()

away_teams <- remDr$findElements(using = "class", "event__participant--away")
away_teams <- lapply(away_teams, function(x) x$getElementText() %>% unlist()) %>% unlist()

result <- ifelse(home > away, "H", ifelse(away > home, "A", "D"))


ssn12_13 <- data.frame(home = home,
                       away = away,
                       home_teams = home_teams,
                       away_teams = away_teams,
                       result = result)

#### saving files

ssn12_13 <- scrapedData2012and2013 %>%  as.data.frame() %>%  select(c(`Gole Gospodarz`, `Gole Gość`, `Gospodarz`, `Gość`, `Wynik`))
ssn13_14 <- scrapedData2013and2014 %>%  as.data.frame() %>%  select(c(`Gole Gospodarz`, `Gole Gość`, `Gospodarz`, `Gość`, `Wynik`))
ssn14_15 <- scrapedData2014and2015 %>%  as.data.frame() %>%  select(c(`Gole Gospodarz`, `Gole Gość`, `Gospodarz`, `Gość`, `Wynik`))
ssn15_16 <- scrapedData2015and2016 %>%  as.data.frame() %>%  select(c(`Gole Gospodarz`, `Gole Gość`, `Gospodarz`, `Gość`, `Wynik`))
ssn16_17 <- scrapedData2016and2017 %>%  as.data.frame() %>%  select(c(`Gole Gospodarz`, `Gole Gość`, `Gospodarz`, `Gość`, `Wynik`))
ssn17_18 <- scrapedData2017and2018 %>%  as.data.frame() %>%  select(c(`Gole Gospodarz`, `Gole Gość`, `Gospodarz`, `Gość`, `Wynik`))
ssn18_19 <- scrapedData2018and2019 %>%  as.data.frame() %>%  select(c(`Gole Gospodarz`, `Gole Gość`, `Gospodarz`, `Gość`, `Wynik`))
ssn19_20 <- scrapedData2019and2020 %>%  as.data.frame() %>%  select(c(`Gole Gospodarz`, `Gole Gość`, `Gospodarz`, `Gość`, `Wynik`))
ssn20_21 <- scrapedData2020and2021 %>%  as.data.frame() %>%  select(c(`Gole Gospodarz`, `Gole Gość`, `Gospodarz`, `Gość`, `Wynik`))
ssn21_22 <- scrapedData2021and2022 %>%  as.data.frame() %>%  select(c(`Gole Gospodarz`, `Gole Gość`, `Gospodarz`, `Gość`, `Wynik`))
ssn22_23 <- scrapedData2022and2023 %>%  as.data.frame() %>%  select(c(`Gole Gospodarz`, `Gole Gość`, `Gospodarz`, `Gość`, `Wynik`))
ssn23_24 <- scrapedData2023and2024 %>%  as.data.frame() %>%  select(c(`Gole Gospodarz`, `Gole Gość`, `Gospodarz`, `Gość`, `Wynik`))
ssn24_25 <- scrapedData2024and2025 %>%  as.data.frame() %>%  select(c(`Gole Gospodarz`, `Gole Gość`, `Gospodarz`, `Gość`, `Wynik`))


colnames(ssn12_13) <- colnames(ssn00_01)
rownames(ssn12_13) <- 1:nrow(ssn12_13)

colnames(ssn13_14) <- colnames(ssn00_01)
rownames(ssn13_14) <- 1:nrow(ssn13_14)

colnames(ssn14_15) <- colnames(ssn00_01)
rownames(ssn14_15) <- 1:nrow(ssn14_15)

colnames(ssn15_16) <- colnames(ssn00_01)
rownames(ssn15_16) <- 1:nrow(ssn15_16)

colnames(ssn16_17) <- colnames(ssn00_01)
rownames(ssn16_17) <- 1:nrow(ssn16_17)

colnames(ssn17_18) <- colnames(ssn00_01)
rownames(ssn17_18) <- 1:nrow(ssn17_18)

colnames(ssn18_19) <- colnames(ssn00_01)
rownames(ssn18_19) <- 1:nrow(ssn18_19)

colnames(ssn19_20) <- colnames(ssn00_01)
rownames(ssn19_20) <- 1:nrow(ssn19_20)

colnames(ssn20_21) <- colnames(ssn00_01)
rownames(ssn20_21) <- 1:nrow(ssn20_21)

colnames(ssn21_22) <- colnames(ssn00_01)
rownames(ssn21_22) <- 1:nrow(ssn21_22)

colnames(ssn22_23) <- colnames(ssn00_01)
rownames(ssn22_23) <- 1:nrow(ssn22_23)

colnames(ssn23_24) <- colnames(ssn00_01)
rownames(ssn23_24) <- 1:nrow(ssn23_24)

colnames(ssn24_25) <- colnames(ssn00_01)
rownames(ssn24_25) <- 1:nrow(ssn24_25)

ssn00_01 <- ssn00_01[-1:-2,]
ssn01_02 <- ssn01_02[-1:-4, ]
ssn02_03 <- ssn02_03[-1:-4, ]
ssn03_04 <- ssn03_04[-1:-2, ]
ssn04_05 <- ssn04_05[-1:-2, ]
ssn05_06 <- ssn05_06[-1:-2, ]


save(ssn98_99,
     ssn99_00,
     ssn00_01,
     ssn01_02,
     ssn02_03,
     ssn03_04,
     ssn04_05,
     ssn05_06,
     ssn06_07,
     ssn07_08,
     ssn08_09,
     ssn09_10,
     ssn10_11,
     ssn11_12,
     ssn12_13,
     ssn13_14,
     ssn14_15,
     ssn15_16,
     ssn16_17,
     ssn17_18,
     ssn18_19,
     ssn19_20,
     ssn20_21,
     ssn21_22,
     ssn22_23,
     ssn_23_24,
     ssn_24_25,
     file = "ssn.Rdata")




