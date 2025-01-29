## Scraping from https://www.wynikinazywo.pl/

#install.packages("RSelenium") you also need to install java for this
#install.packages("netstat") only one function is needed

#### WARNING
#### the site from which data is gathered changed so "wynikanazywo" should
#### probably be changed to just "wyniki" everywhere

# if any for loop breaks (which happens often due to instability of selenium) try:
# for (m in subSiteUrl[(which(subSiteUrl == m)):length(subSiteUrl)]) {

# for now this is now very elegant and likely no correct but will be improved in the future


#### WARNING (AND TODO)
### code updated only for 2022-2023 and onwards
### need to fix prior years due to change of site architecture

library(tidyverse)
library(RSelenium)
library(netstat)

# 2012/2013 ####

url <- "https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2012-2013/wyniki/"

seleniumServer <- rsDriver(browser = "chrome",
                           #verbose = FALSE, 
                           chromever = "105.0.5195.52", # the lastest
                           port = free_port())
# Client object
remDr <- seleniumServer$client

remDr$open()
remDr$maxWindowSize()
remDr$navigate(url)
# click on cookies info:
cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
cookies$clickElement()
# click on more info:
moreInfo <- remDr$findElement(using = "xpath", '//*[@id="live-table"]/div[1]/div/div/a')
moreInfo$clickElement()
moreInfo$clickElement()
# get urls to match statistics
obj <- remDr$findElement(using = "xpath", '//*[@id="live-table"]/div[1]/div/div')
subSiteUrl <- 
  sapply(obj$findChildElements(using = "class name", "event__match"), 
         FUN = function(x) {x$getElementAttribute("id")}) %>%
  unlist()
subSiteUrl <- sapply(subSiteUrl, FUN = function(x) {substr(x, start = 5, stop = 12)})
subSiteUrl <- paste0("https://www.wynikinazywo.pl/mecz/", subSiteUrl, "/#/szczegoly-meczu/statystyki-meczu/0")

# navigate to match statistics:

K <- NULL
matchNames <- NULL
statNames <- c(
  "Posiadanie piłki", # All possible match statistics provided on site
  "Sytuacje bramkowe",
  "Strzały na bramkę",
  "Strzały niecelne",
  "Rzuty rożne",
  "Spalone",
  "Interwencje bramkarzy",
  "Faule",
  "Żółte kartki",
  "Czerwone kartki",
  "Rzuty wolne",
  "Auty bramkowe"
)
for (m in subSiteUrl[1:114]) {# Not all maches in 2012/2013 have match statistics this is for ones with them
  print(m)                    # sometimes selenium breaks because of cookies it is possible to just start the loop again
  remDr$navigate(m)           # begining at which(subSiteUrl == m) no issues should be present
  #if (which(subSiteUrl == m) == 1) {remDr$findElement(using = "id", "onetrust-reject-all-handler")$clickElement()} #this clicks cookies
  
  obj1 <- remDr$findElements(using = "class name", "stat__category")
  if (length(obj1) == 0) {
    obj1 <- remDr$findElements(using = "class name", "stat__category") # sometimes you need to double click
  }
  outcome <- strsplit(x = (obj1[[1]]$getTitle())[[1]], split = "|", fixed = TRUE)[[1]]
  outcome1 <- suppressWarnings((outcome[1] %>% strsplit(split = ""))[[1]] %>% as.numeric())
  matchNames <- c(matchNames, outcome[2] %>% str_trim())
  outcome1 <- outcome1[!is.na(outcome1)]
  valuesHome <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__homeValue")$getElementText()}) %>% unlist()
  names(valuesHome) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  v1 <- rep(NA, length(statNames))
  names(v1) <- statNames
  v1[names(v1) %in% names(valuesHome)][names(valuesHome)] <- valuesHome
  valuesHome <- v1
  valuesAway <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__awayValue")$getElementText()}) %>% unlist()
  names(valuesAway) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  v1 <- rep(NA, length(statNames))
  names(v1) <- statNames
  v1[names(v1) %in% names(valuesAway)][names(valuesAway)] <- valuesAway
  valuesAway <- v1
  participants <- stringr::str_trim(strsplit(outcome[2], split = "-")[[1]], side = "both")
  if (length(participants) == 3) {                                                 # fix for podbeskidzie bielsko biała
    if (which(participants == "B") == 3) {                                         # (podbeskidzie B-B)
      participants <- c(participants[1], paste0(participants[2], participants[3]))
    } else if (which(participants == "B") == 2) {
      participants <- c(paste0(participants[1], participants[2]), participants[3])
    } else {
      stop("Check participants")
    }
  }
  
  if (is.null(K)) {
    K <- c(valuesHome[statNames], valuesAway[statNames],
           outcome1[1], outcome1[2], participants,
           ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
           remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]]) %>%
      data.frame()
  } else {
    K <- data.frame(
      K,
      c(valuesHome[statNames], valuesAway[statNames],
        outcome1[1], outcome1[2], participants,
        ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
        remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]])
    )
  }
}

K <- t(K)
colnames(K) <- c(paste0(statNames, " Gospodarz"), paste0(statNames, " Gość"), "Gole Gospodarz", "Gole Gość", "Gospodarz", "Gość", "Wynik", "Data")
rownames(K) <- matchNames
scrapedData2012and2013 <- K
save(scrapedData2012and2013, file = "data/scrapedData2012and2013.Rdata")

# 2013/2014 ####

url <- "https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2013-2014/wyniki/"

seleniumServer <- rsDriver(browser = "chrome",
                           #verbose = FALSE, 
                           chromever = "105.0.5195.52", # the lastest
                           port = free_port())
# Client object
remDr <- seleniumServer$client

remDr$open()
remDr$maxWindowSize()
remDr$navigate(url)
# click on cookies info:
cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
cookies$clickElement()
# click on more info:
moreInfo <- remDr$findElement(using = "xpath", '//*[@id="live-table"]/div[1]/div/div/a')
moreInfo$clickElement()
moreInfo$clickElement()
# get urls to match statistics
obj <- remDr$findElement(using = "xpath", '//*[@id="live-table"]/div[1]/div/div')
subSiteUrl <- 
  sapply(obj$findChildElements(using = "class name", "event__match"), 
         FUN = function(x) {x$getElementAttribute("id")}) %>%
  unlist()
subSiteUrl <- sapply(subSiteUrl, FUN = function(x) {substr(x, start = 5, stop = 12)})
subSiteUrl <- paste0("https://www.wynikinazywo.pl/mecz/", subSiteUrl, "/#/szczegoly-meczu/statystyki-meczu/0")

# navigate to match statistics:

K <- NULL
matchNames <- NULL
statNames <- c(
  "Posiadanie piłki", # All possible match statistics provided on site for this season 2 new
  "Sytuacje bramkowe",
  "Strzały na bramkę",
  "Strzały niecelne",
  "Strzały zablokowane",
  "Rzuty wolne",
  "Rzuty rożne",
  "Spalone",
  "Wrzuty z autu",
  "Interwencje bramkarzy",
  "Faule","Żółte kartki",
  "Czerwone kartki",
  "Auty bramkowe"
)
# for (m in subSiteUrl) {
#   print(m)  # this determines the statNanes vector
#   remDr$navigate(m)
#   obj1 <- remDr$findElements(using = "class name", "stat__categoryName")
#   K <- c(K, sapply(obj1, FUN = function(x) {x$getElementText()}) %>% unlist())
# }
for (m in subSiteUrl) {
  print(m)                    # sometimes selenium breaks because of cookies it is possible to just start the loop again
  remDr$navigate(m)           # begining at which(subSiteUrl == m) no issues should be present
  #if (which(subSiteUrl == m) == 1) {remDr$findElement(using = "id", "onetrust-reject-all-handler")$clickElement()} #this clicks cookies
  
  obj1 <- remDr$findElements(using = "class name", "stat__category")
  if (length(obj1) == 0) {
    obj1 <- remDr$findElements(using = "class name", "stat__category") # sometimes you need to double click
  }
  outcome <- strsplit(x = (obj1[[1]]$getTitle())[[1]], split = "|", fixed = TRUE)[[1]]
  outcome1 <- suppressWarnings((outcome[1] %>% strsplit(split = ""))[[1]] %>% as.numeric())
  matchNames <- c(matchNames, outcome[2] %>% str_trim())
  outcome1 <- outcome1[!is.na(outcome1)]
  valuesHome <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__homeValue")$getElementText()}) %>% unlist()
  names(valuesHome) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  if (any(!(names(valuesHome) %in% statNames))) {stop("bad names")}
  v1 <- rep(NA, length(statNames))
  names(v1) <- statNames
  v1[names(v1) %in% names(valuesHome)][names(valuesHome)] <- valuesHome
  valuesHome <- v1
  valuesAway <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__awayValue")$getElementText()}) %>% unlist()
  names(valuesAway) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  v1 <- rep(NA, length(statNames))
  names(v1) <- statNames
  v1[names(v1) %in% names(valuesAway)][names(valuesAway)] <- valuesAway
  valuesAway <- v1
  participants <- stringr::str_trim(strsplit(outcome[2], split = "-")[[1]], side = "both")
  if (length(participants) == 3) {                                                 # fix for podbeskidzie bielsko biała
    if (which(participants == "B") == 3) {                                         # (podbeskidzie B-B)
      participants <- c(participants[1], paste0(participants[2], participants[3]))
    } else if (which(participants == "B") == 2) {
      participants <- c(paste0(participants[1], participants[2]), participants[3])
    } else {
      stop("Check participants")
    }
  }
  
  if (is.null(K)) {
    K <- c(valuesHome[statNames], valuesAway[statNames],
           outcome1[1], outcome1[2], participants,
           ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
           remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]]) %>%
      data.frame()
  } else {
    K <- data.frame(
      K,
      c(valuesHome[statNames], valuesAway[statNames],
        outcome1[1], outcome1[2], participants,
        ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
        remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]])
    )
  }
}

K <- t(K)
colnames(K) <- c(paste0(statNames, " Gospodarz"), paste0(statNames, " Gość"), "Gole Gospodarz", "Gole Gość", "Gospodarz", "Gość", "Wynik", "Data")
rownames(K) <- matchNames
scrapedData2013and2014 <- K
save(scrapedData2013and2014, file = "data/scrapedData2013and2014.Rdata")


# 2014/2015 ####

url <- "https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2014-2015/wyniki/"

seleniumServer <- rsDriver(browser = "chrome",
                           #verbose = FALSE, 
                           chromever = "105.0.5195.52", # the lastest
                           port = free_port())
# Client object
remDr <- seleniumServer$client

remDr$open()
remDr$maxWindowSize()
remDr$navigate(url)
# click on cookies info:
cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
cookies$clickElement()
# click on more info:
moreInfo <- remDr$findElement(using = "xpath", '//*[@id="live-table"]/div[1]/div/div/a')
moreInfo$clickElement()
moreInfo$clickElement()
# get urls to match statistics
obj <- remDr$findElement(using = "xpath", '//*[@id="live-table"]/div[1]/div/div')
subSiteUrl <- 
  sapply(obj$findChildElements(using = "class name", "event__match"), FUN = function(x) {x$getElementAttribute("id")}) %>%
  unlist()
subSiteUrl <- sapply(subSiteUrl, FUN = function(x) {substr(x, start = 5, stop = 12)})
subSiteUrl <- paste0("https://www.wynikinazywo.pl/mecz/", subSiteUrl, "/#/szczegoly-meczu/statystyki-meczu/0")

# navigate to match statistics:

K <- NULL
matchNames <- NULL
statNames <- c(
  "Posiadanie piłki",
  "Sytuacje bramkowe",
  "Strzały na bramkę",
  "Strzały niecelne",
  "Strzały zablokowane",
  "Rzuty wolne",
  "Rzuty rożne",
  "Spalone",
  "Wrzuty z autu",
  "Interwencje bramkarzy",
  "Faule",
  "Żółte kartki",
  "Czerwone kartki"
)
# for (m in subSiteUrl) {
#   print(m)
#   remDr$navigate(m)
#   obj1 <- remDr$findElements(using = "class name", "stat__categoryName")
#   K <- c(K, sapply(obj1, FUN = function(x) {x$getElementText()}) %>% unlist())
# }
for (m in subSiteUrl) {
  print(m)                    # sometimes selenium breaks because of cookies it is possible to just start the loop again
  remDr$navigate(m)           # begining at which(subSiteUrl == m) no issues should be present
  #if (which(subSiteUrl == m) == 1) {remDr$findElement(using = "id", "onetrust-reject-all-handler")$clickElement()} #this clicks cookies
  
  obj1 <- remDr$findElements(using = "class name", "stat__category")
  if (length(obj1) == 0) {
    obj1 <- remDr$findElements(using = "class name", "stat__category") # sometimes you need to double click
  }
  outcome <- strsplit(x = (obj1[[1]]$getTitle())[[1]], split = "|", fixed = TRUE)[[1]]
  outcome1 <- suppressWarnings((outcome[1] %>% strsplit(split = ""))[[1]] %>% as.numeric())
  matchNames <- c(matchNames, outcome[2] %>% str_trim())
  outcome1 <- outcome1[!is.na(outcome1)]
  valuesHome <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__homeValue")$getElementText()}) %>% unlist()
  names(valuesHome) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  if (any(!(names(valuesHome) %in% statNames))) {stop("bad names")}
  v1 <- rep(NA, length(statNames))
  names(v1) <- statNames
  v1[names(v1) %in% names(valuesHome)][names(valuesHome)] <- valuesHome
  valuesHome <- v1
  valuesAway <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__awayValue")$getElementText()}) %>% unlist()
  names(valuesAway) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  v1 <- rep(NA, length(statNames))
  names(v1) <- statNames
  v1[names(v1) %in% names(valuesAway)][names(valuesAway)] <- valuesAway
  valuesAway <- v1
  participants <- stringr::str_trim(strsplit(outcome[2], split = "-")[[1]], side = "both")
  if (length(participants) == 3) {                                                 # fix for podbeskidzie bielsko biała
    if (which(participants == "B") == 3) {                                         # (podbeskidzie B-B)
      participants <- c(participants[1], paste0(participants[2], participants[3]))
    } else if (which(participants == "B") == 2) {
      participants <- c(paste0(participants[1], participants[2]), participants[3])
    } else {
      stop("Check participants")
    }
  }
  
  if (is.null(K)) {
    K <- c(valuesHome[statNames], valuesAway[statNames],
           outcome1[1], outcome1[2], participants,
           ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
           remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]]) %>%
      data.frame()
  } else {
    K <- data.frame(
      K,
      c(valuesHome[statNames], valuesAway[statNames],
        outcome1[1], outcome1[2], participants,
        ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
        remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]])
    )
  }
}

K <- t(K)
colnames(K) <- c(paste0(statNames, " Gospodarz"), paste0(statNames, " Gość"), "Gole Gospodarz", "Gole Gość", "Gospodarz", "Gość", "Wynik", "Data")
rownames(K) <- matchNames
scrapedData2014and2015 <- K
save(scrapedData2014and2015, file = "data/scrapedData2014and2015.Rdata")

# 2015/2016 ####

url <- "https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2015-2016/wyniki/"

seleniumServer <- rsDriver(browser = "chrome",
                           #verbose = FALSE, 
                           chromever = "105.0.5195.52", # the lastest
                           port = free_port())
# Client object
remDr <- seleniumServer$client

remDr$open()
remDr$maxWindowSize()
remDr$navigate(url)
# click on cookies info:
cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
cookies$clickElement()
# click on more info:
moreInfo <- remDr$findElement(using = "xpath", '//*[@id="live-table"]/div[1]/div/div/a')
moreInfo$clickElement()
moreInfo$clickElement()
# get urls to match statistics
obj <- remDr$findElement(using = "xpath", '//*[@id="live-table"]/div[1]/div/div')
subSiteUrl <- 
  sapply(obj$findChildElements(using = "class name", "event__match"), FUN = function(x) {x$getElementAttribute("id")}) %>%
  unlist()
subSiteUrl <- sapply(subSiteUrl, FUN = function(x) {substr(x, start = 5, stop = 12)})
subSiteUrl <- paste0("https://www.wynikinazywo.pl/mecz/", subSiteUrl, "/#/szczegoly-meczu/statystyki-meczu/0")

# navigate to match statistics:

K <- NULL
matchNames <- NULL
statNames <- c(
  "Posiadanie piłki",
  "Sytuacje bramkowe",
  "Strzały na bramkę",
  "Strzały niecelne",
  "Strzały zablokowane",
  "Rzuty wolne",
  "Rzuty rożne",
  "Spalone",
  "Wrzuty z autu",
  "Interwencje bramkarzy",
  "Faule",
  "Żółte kartki",
  "Czerwone kartki"
)
# for (m in subSiteUrl) {
#   print(m)
#   remDr$navigate(m)
#   obj1 <- remDr$findElements(using = "class name", "stat__categoryName")
#   K <- c(K, sapply(obj1, FUN = function(x) {x$getElementText()}) %>% unlist())
# }
for (m in subSiteUrl) {
  print(m)                    # sometimes selenium breaks because of cookies it is possible to just start the loop again
  remDr$navigate(m)           # begining at which(subSiteUrl == m) no issues should be present
  #if (which(subSiteUrl == m) == 1) {remDr$findElement(using = "id", "onetrust-reject-all-handler")$clickElement()} #this clicks cookies
  
  obj1 <- remDr$findElements(using = "class name", "stat__category")
  if (length(obj1) == 0) {
    obj1 <- remDr$findElements(using = "class name", "stat__category") # sometimes you need to double click
  }
  outcome <- strsplit(x = (obj1[[1]]$getTitle())[[1]], split = "|", fixed = TRUE)[[1]]
  outcome1 <- suppressWarnings((outcome[1] %>% strsplit(split = ""))[[1]] %>% as.numeric())
  matchNames <- c(matchNames, outcome[2] %>% str_trim())
  outcome1 <- outcome1[!is.na(outcome1)]
  valuesHome <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__homeValue")$getElementText()}) %>% unlist()
  names(valuesHome) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  if (any(!(names(valuesHome) %in% statNames))) {stop("bad names")}
  v1 <- rep(NA, length(statNames))
  names(v1) <- statNames
  v1[names(v1) %in% names(valuesHome)][names(valuesHome)] <- valuesHome
  valuesHome <- v1
  valuesAway <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__awayValue")$getElementText()}) %>% unlist()
  names(valuesAway) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  v1 <- rep(NA, length(statNames))
  names(v1) <- statNames
  v1[names(v1) %in% names(valuesAway)][names(valuesAway)] <- valuesAway
  valuesAway <- v1
  participants <- stringr::str_trim(strsplit(outcome[2], split = "-")[[1]], side = "both")
  if (length(participants) == 3) {                                                 # fix for podbeskidzie bielsko biała and Bruk-Bet Termalica Nieciecza
    if ("B" %in% participants) {                                                   #              (podbeskidzie B-B)               (Bruk-Bet T.)
      if (which(participants == "B") == 3) {                                
        participants <- c(participants[1], paste0(participants[2], participants[3]))
      } else if (which(participants == "B") == 2) {
        participants <- c(paste0(participants[1], participants[2]), participants[3])
      } else {
        stop("Check participants")
      }
    } else if ("Bruk" %in% participants) {
      if (which(participants == "Bet T.") == 3) {                                         
        participants <- c(participants[1], paste0(participants[2], participants[3]))
      } else if (which(participants == "Bet T.") == 2) {
        participants <- c(paste0(participants[1], participants[2]), participants[3])
      } else {
        stop("Check participants")
      }
    } else {
      stop("Check participants")
    }
  } else if (length(participants) == 4) {
    if (all(c("Bruk", "B") %in% participants)) {
      participants <- c(paste0(participants[1], participants[2]), participants[3:4])
      participants <- c(participants[1], paste0(participants[2], participants[3]))
    } else {
      stop("Check participants")
    }
  }
  
  if (is.null(K)) {
    K <- c(valuesHome[statNames], valuesAway[statNames],
           outcome1[1], outcome1[2], participants,
           ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
           remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]]) %>%
      data.frame()
  } else {
    K <- data.frame(
      K,
      c(valuesHome[statNames], valuesAway[statNames],
        outcome1[1], outcome1[2], participants,
        ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
        remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]])
    )
  }
}

K <- t(K)
colnames(K) <- c(paste0(statNames, " Gospodarz"), paste0(statNames, " Gość"), "Gole Gospodarz", "Gole Gość", "Gospodarz", "Gość", "Wynik", "Data")
rownames(K) <- matchNames
scrapedData2015and2016 <- K
save(scrapedData2015and2016, file = "data/scrapedData2015and2016.Rdata")

# 2016/2017 ####
url <- "https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2016-2017/wyniki/"

seleniumServer <- rsDriver(browser = "chrome",
                           #verbose = FALSE, 
                           chromever = "105.0.5195.52", # the lastest
                           port = free_port())
# Client object
remDr <- seleniumServer$client

remDr$open()
remDr$maxWindowSize()
remDr$navigate(url)
# click on cookies info:
cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
cookies$clickElement()
# click on more info:
moreInfo <- remDr$findElement(using = "xpath", '//*[@id="live-table"]/div[1]/div/div/a')
moreInfo$clickElement()
Sys.sleep(.5)
moreInfo$clickElement()
# get urls to match statistics
obj <- remDr$findElement(using = "xpath", '//*[@id="live-table"]/div[1]/div/div')
subSiteUrl <- 
  sapply(obj$findChildElements(using = "class name", "event__match"), FUN = function(x) {x$getElementAttribute("id")}) %>%
  unlist()
subSiteUrl <- sapply(subSiteUrl, FUN = function(x) {substr(x, start = 5, stop = 12)})
subSiteUrl <- paste0("https://www.wynikinazywo.pl/mecz/", subSiteUrl, "/#/szczegoly-meczu/statystyki-meczu/0")

# navigate to match statistics:

K <- NULL
matchNames <- NULL
statNames <- c(
  "Posiadanie piłki",
  "Sytuacje bramkowe",
  "Strzały na bramkę",
  "Strzały niecelne",
  "Strzały zablokowane",
  "Rzuty wolne",
  "Rzuty rożne",
  "Spalone",
  "Wrzuty z autu",
  "Interwencje bramkarzy",
  "Faule",
  "Żółte kartki",
  "Czerwone kartki"
)
# for (m in subSiteUrl) {
#   print(m)
#   remDr$navigate(m)
#   obj1 <- remDr$findElements(using = "class name", "stat__categoryName")
#   K <- c(K, sapply(obj1, FUN = function(x) {x$getElementText()}) %>% unlist())
# }
for (m in subSiteUrl) {
  print(m)                    # sometimes selenium breaks because of cookies it is possible to just start the loop again
  remDr$navigate(m)           # begining at which(subSiteUrl == m) no issues should be present
  #if (which(subSiteUrl == m) == 1) {remDr$findElement(using = "id", "onetrust-reject-all-handler")$clickElement()} #this clicks cookies
  
  Sys.sleep(.05) # this makes it so that the site always has the time to compile javascript code
  obj1 <- remDr$findElements(using = "class name", "stat__category")
  if (length(obj1) == 0) {
    obj1 <- remDr$findElements(using = "class name", "stat__category") # sometimes you need to double click
  }
  outcome <- strsplit(x = (obj1[[1]]$getTitle())[[1]], split = "|", fixed = TRUE)[[1]]
  outcome1 <- suppressWarnings((outcome[1] %>% strsplit(split = ""))[[1]] %>% as.numeric())
  matchNames <- c(matchNames, outcome[2] %>% str_trim())
  outcome1 <- outcome1[!is.na(outcome1)]
  valuesHome <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__homeValue")$getElementText()}) %>% unlist()
  names(valuesHome) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  if (any(!(names(valuesHome) %in% statNames))) {stop("bad names")}
  v1 <- rep(NA, length(statNames))
  names(v1) <- statNames
  v1[names(v1) %in% names(valuesHome)][names(valuesHome)] <- valuesHome
  valuesHome <- v1
  valuesAway <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__awayValue")$getElementText()}) %>% unlist()
  names(valuesAway) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  v1 <- rep(NA, length(statNames))
  names(v1) <- statNames
  v1[names(v1) %in% names(valuesAway)][names(valuesAway)] <- valuesAway
  valuesAway <- v1
  participants <- stringr::str_trim(strsplit(outcome[2], split = "-")[[1]], side = "both")
  if (length(participants) == 3) {                                                 # fix for podbeskidzie bielsko biała and Bruk-Bet Termalica Nieciecza
    if ("B" %in% participants) {                                                   #              (podbeskidzie B-B)               (Bruk-Bet T.)
      if (which(participants == "B") == 3) {                                
        participants <- c(participants[1], paste0(participants[2], participants[3]))
      } else if (which(participants == "B") == 2) {
        participants <- c(paste0(participants[1], participants[2]), participants[3])
      } else {
        stop("Check participants")
      }
    } else if ("Bruk" %in% participants) {
      if (which(participants == "Bet T.") == 3) {                                         
        participants <- c(participants[1], paste0(participants[2], participants[3]))
      } else if (which(participants == "Bet T.") == 2) {
        participants <- c(paste0(participants[1], participants[2]), participants[3])
      } else {
        stop("Check participants")
      }
    } else {
      stop("Check participants")
    }
  } else if (length(participants) == 4) {
    if (all(c("Bruk", "B") %in% participants)) {
      participants <- c(paste0(participants[1], participants[2]), participants[3:4])
      participants <- c(participants[1], paste0(participants[2], participants[3]))
    } else {
      stop("Check participants")
    }
  }
  
  if (is.null(K)) {
    K <- c(valuesHome[statNames], valuesAway[statNames],
           outcome1[1], outcome1[2], participants,
           ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
           remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]]) %>%
      data.frame()
  } else {
    K <- data.frame(
      K,
      c(valuesHome[statNames], valuesAway[statNames],
        outcome1[1], outcome1[2], participants,
        ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
        remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]])
    )
  }
}

K <- t(K)
colnames(K) <- c(paste0(statNames, " Gospodarz"), paste0(statNames, " Gość"), "Gole Gospodarz", "Gole Gość", "Gospodarz", "Gość", "Wynik", "Data")
rownames(K) <- matchNames
scrapedData2016and2017 <- K
save(scrapedData2016and2017, file = "data/scrapedData2016and2017.Rdata")

# 2017/2018 ####
url <- "https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2017-2018/wyniki/"

seleniumServer <- rsDriver(browser = "chrome",
                           #verbose = FALSE, 
                           chromever = "105.0.5195.52", # the lastest
                           port = free_port())
# Client object
remDr <- seleniumServer$client

remDr$open()
remDr$maxWindowSize()
remDr$navigate(url)
# click on cookies info:
cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
cookies$clickElement()
# click on more info:
moreInfo <- remDr$findElement(using = "xpath", '//*[@id="live-table"]/div[1]/div/div/a')
moreInfo$clickElement()
moreInfo$clickElement() # may possibly cause an error if 
# get urls to match statistics
obj <- remDr$findElement(using = "xpath", '//*[@id="live-table"]/div[1]/div/div')
subSiteUrl <- 
  sapply(obj$findChildElements(using = "class name", "event__match"), FUN = function(x) {x$getElementAttribute("id")}) %>%
  unlist()
subSiteUrl <- sapply(subSiteUrl, FUN = function(x) {substr(x, start = 5, stop = 12)})
subSiteUrl <- paste0("https://www.wynikinazywo.pl/mecz/", subSiteUrl, "/#/szczegoly-meczu/statystyki-meczu/0")

# navigate to match statistics:

K <- NULL
matchNames <- NULL
statNames <- c(
  "Posiadanie piłki",
  "Sytuacje bramkowe",
  "Strzały na bramkę",
  "Strzały niecelne",
  "Rzuty rożne",
  "Spalone",
  "Interwencje bramkarzy",
  "Faule","Żółte kartki",
  "Ataki",
  "Niebezpieczne ataki",
  "Czerwone kartki",
  "Wrzuty z autu",
  "Rzuty wolne"  
)
# for (m in subSiteUrl) {
#   print(m)
#   remDr$navigate(m)
#   obj1 <- remDr$findElements(using = "class name", "stat__categoryName")
#   K <- c(K, sapply(obj1, FUN = function(x) {x$getElementText()}) %>% unlist())
# }
for (m in subSiteUrl) {
  print(m)                    # sometimes selenium breaks because of cookies it is possible to just start the loop again
  remDr$navigate(m)           # begining at which(subSiteUrl == m) no issues should be present
  #if (which(subSiteUrl == m) == 1) {remDr$findElement(using = "id", "onetrust-reject-all-handler")$clickElement()} #this clicks cookies
  
  Sys.sleep(.05) # this makes it so that the site always has the time to compile javascript code
  obj1 <- remDr$findElements(using = "class name", "stat__category")
  if (length(obj1) == 0) {
    obj1 <- remDr$findElements(using = "class name", "stat__category") # sometimes you need to double click
  }
  outcome <- strsplit(x = (obj1[[1]]$getTitle())[[1]], split = "|", fixed = TRUE)[[1]]
  outcome1 <- suppressWarnings((outcome[1] %>% strsplit(split = ""))[[1]] %>% as.numeric())
  matchNames <- c(matchNames, outcome[2] %>% str_trim())
  outcome1 <- outcome1[!is.na(outcome1)]
  valuesHome <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__homeValue")$getElementText()}) %>% unlist()
  names(valuesHome) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  if (any(!(names(valuesHome) %in% statNames))) {stop("bad names")}
  v1 <- rep(NA, length(statNames))
  names(v1) <- statNames
  v1[names(v1) %in% names(valuesHome)][names(valuesHome)] <- valuesHome
  valuesHome <- v1
  valuesAway <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__awayValue")$getElementText()}) %>% unlist()
  names(valuesAway) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  v1 <- rep(NA, length(statNames))
  names(v1) <- statNames
  v1[names(v1) %in% names(valuesAway)][names(valuesAway)] <- valuesAway
  valuesAway <- v1
  participants <- stringr::str_trim(strsplit(outcome[2], split = "-")[[1]], side = "both")
  if (length(participants) == 3) {                                                 # fix for podbeskidzie bielsko biała and Bruk-Bet Termalica Nieciecza
    if ("B" %in% participants) {                                                   #              (podbeskidzie B-B)               (Bruk-Bet T.)
      if (which(participants == "B") == 3) {                                
        participants <- c(participants[1], paste0(participants[2], participants[3]))
      } else if (which(participants == "B") == 2) {
        participants <- c(paste0(participants[1], participants[2]), participants[3])
      } else {
        stop("Check participants")
      }
    } else if ("Bruk" %in% participants) {
      if (which(participants == "Bet T.") == 3) {                                         
        participants <- c(participants[1], paste0(participants[2], participants[3]))
      } else if (which(participants == "Bet T.") == 2) {
        participants <- c(paste0(participants[1], participants[2]), participants[3])
      } else {
        stop("Check participants")
      }
    } else {
      stop("Check participants")
    }
  } else if (length(participants) == 4) {
    if (all(c("Bruk", "B") %in% participants)) {
      participants <- c(paste0(participants[1], participants[2]), participants[3:4])
      participants <- c(participants[1], paste0(participants[2], participants[3]))
    } else {
      stop("Check participants")
    }
  }
  
  if (is.null(K)) {
    K <- c(valuesHome[statNames], valuesAway[statNames],
           outcome1[1], outcome1[2], participants,
           ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
           remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]]) %>%
      data.frame()
  } else {
    K <- data.frame(
      K,
      c(valuesHome[statNames], valuesAway[statNames],
        outcome1[1], outcome1[2], participants,
        ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
        remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]])
    )
  }
}

K <- t(K)
colnames(K) <- c(paste0(statNames, " Gospodarz"), paste0(statNames, " Gość"), "Gole Gospodarz", "Gole Gość", "Gospodarz", "Gość", "Wynik", "Data")
rownames(K) <- matchNames
scrapedData2017and2018 <- K
save(scrapedData2017and2018, file = "data/scrapedData2017and2018.Rdata")

# 2018/2019 ####

url <- "https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2018-2019/wyniki/"

seleniumServer <- rsDriver(browser = "chrome",
                           #verbose = FALSE, 
                           chromever = "105.0.5195.52", # the lastest
                           port = free_port())
# Client object
remDr <- seleniumServer$client

remDr$open()
remDr$maxWindowSize()
remDr$navigate(url)
# click on cookies info:
cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
cookies$clickElement()
# click on more info:
moreInfo <- remDr$findElement(using = "xpath", '//*[@id="live-table"]/div[1]/div/div/a')
moreInfo$clickElement()
Sys.sleep(.5)
moreInfo$clickElement()
# get urls to match statistics
obj <- remDr$findElement(using = "xpath", '//*[@id="live-table"]/div[1]/div/div')
subSiteUrl <- 
  sapply(obj$findChildElements(using = "class name", "event__match"), FUN = function(x) {x$getElementAttribute("id")}) %>%
  unlist()
subSiteUrl <- sapply(subSiteUrl, FUN = function(x) {substr(x, start = 5, stop = 12)})
subSiteUrl <- paste0("https://www.wynikinazywo.pl/mecz/", subSiteUrl, "/#/szczegoly-meczu/statystyki-meczu/0")

# navigate to match statistics:

K <- NULL
matchNames <- NULL
statNames <- c(
  "Posiadanie piłki",
  "Sytuacje bramkowe",
  "Strzały na bramkę",
  "Strzały niecelne",
  "Rzuty rożne",
  "Spalone",
  "Interwencje bramkarzy",
  "Faule",
  "Żółte kartki",
  "Podania",
  "Ataki",
  "Niebezpieczne ataki",
  "Czerwone kartki",
  "Rzuty wolne"      
)
# for (m in subSiteUrl) {
#   print(m)
#   remDr$navigate(m)
#   obj1 <- remDr$findElements(using = "class name", "stat__categoryName")
#   K <- c(K, sapply(obj1, FUN = function(x) {x$getElementText()}) %>% unlist())
# }
for (m in subSiteUrl) {
  print(m)                    # sometimes selenium breaks because of cookies it is possible to just start the loop again
  remDr$navigate(m)           # begining at which(subSiteUrl == m) no issues should be present
  #if (which(subSiteUrl == m) == 1) {remDr$findElement(using = "id", "onetrust-reject-all-handler")$clickElement()} #this clicks cookies
  
  Sys.sleep(.05) # this makes it so that the site always has the time to compile javascript code
  obj1 <- remDr$findElements(using = "class name", "stat__category")
  if (length(obj1) == 0) {
    obj1 <- remDr$findElements(using = "class name", "stat__category") # sometimes you need to double click
  }
  outcome <- strsplit(x = (obj1[[1]]$getTitle())[[1]], split = "|", fixed = TRUE)[[1]]
  outcome1 <- suppressWarnings((outcome[1] %>% strsplit(split = ""))[[1]] %>% as.numeric())
  matchNames <- c(matchNames, outcome[2] %>% str_trim())
  outcome1 <- outcome1[!is.na(outcome1)]
  valuesHome <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__homeValue")$getElementText()}) %>% unlist()
  names(valuesHome) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  if (any(!(names(valuesHome) %in% statNames))) {stop("bad names")}
  v1 <- rep(NA, length(statNames))
  names(v1) <- statNames
  v1[names(v1) %in% names(valuesHome)][names(valuesHome)] <- valuesHome
  valuesHome <- v1
  valuesAway <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__awayValue")$getElementText()}) %>% unlist()
  names(valuesAway) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  v1 <- rep(NA, length(statNames))
  names(v1) <- statNames
  v1[names(v1) %in% names(valuesAway)][names(valuesAway)] <- valuesAway
  valuesAway <- v1
  participants <- stringr::str_trim(strsplit(outcome[2], split = "-")[[1]], side = "both")
  if (length(participants) == 3) {                                                 # fix for podbeskidzie bielsko biała and Bruk-Bet Termalica Nieciecza
    if ("B" %in% participants) {                                                   #              (podbeskidzie B-B)               (Bruk-Bet T.)
      if (which(participants == "B") == 3) {                                
        participants <- c(participants[1], paste0(participants[2], participants[3]))
      } else if (which(participants == "B") == 2) {
        participants <- c(paste0(participants[1], participants[2]), participants[3])
      } else {
        stop("Check participants")
      }
    } else if ("Bruk" %in% participants) {
      if (which(participants == "Bet T.") == 3) {                                         
        participants <- c(participants[1], paste0(participants[2], participants[3]))
      } else if (which(participants == "Bet T.") == 2) {
        participants <- c(paste0(participants[1], participants[2]), participants[3])
      } else {
        stop("Check participants")
      }
    } else {
      stop("Check participants")
    }
  } else if (length(participants) == 4) {
    if (all(c("Bruk", "B") %in% participants)) {
      participants <- c(paste0(participants[1], participants[2]), participants[3:4])
      participants <- c(participants[1], paste0(participants[2], participants[3]))
    } else {
      stop("Check participants")
    }
  }
  
  if (is.null(K)) {
    K <- c(valuesHome[statNames], valuesAway[statNames],
           outcome1[1], outcome1[2], participants,
           ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
           remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]]) %>%
      data.frame()
  } else {
    K <- data.frame(
      K,
      c(valuesHome[statNames], valuesAway[statNames],
        outcome1[1], outcome1[2], participants,
        ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
        remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]])
    )
  }
}

K <- t(K)
colnames(K) <- c(paste0(statNames, " Gospodarz"), paste0(statNames, " Gość"), "Gole Gospodarz", "Gole Gość", "Gospodarz", "Gość", "Wynik", "Data")
rownames(K) <- matchNames
scrapedData2018and2019 <- K
save(scrapedData2018and2019, file = "data/scrapedData2018and2019.Rdata")

# 2019/2020 ####

url <- "https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2019-2020/wyniki/"

seleniumServer <- rsDriver(browser = "chrome",
                           #verbose = FALSE, 
                           chromever = "105.0.5195.52", # the lastest
                           port = free_port())
# Client object
remDr <- seleniumServer$client

remDr$open()
remDr$maxWindowSize()
remDr$navigate(url)
# click on cookies info:
cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
cookies$clickElement()
# click on more info:
moreInfo <- remDr$findElement(using = "xpath", '//*[@id="live-table"]/div[1]/div/div/a')
moreInfo$clickElement()
Sys.sleep(.5)
moreInfo$clickElement() # may possibly cause an error if 
# get urls to match statistics
obj <- remDr$findElement(using = "xpath", '//*[@id="live-table"]/div[1]/div/div')
subSiteUrl <- 
  sapply(obj$findChildElements(using = "class name", "event__match"), FUN = function(x) {x$getElementAttribute("id")}) %>%
  unlist()
subSiteUrl <- sapply(subSiteUrl, FUN = function(x) {substr(x, start = 5, stop = 12)})
subSiteUrl <- paste0("https://www.wynikinazywo.pl/mecz/", subSiteUrl, "/#/szczegoly-meczu/statystyki-meczu/0")

# navigate to match statistics:

K <- NULL
matchNames <- NULL
statNames <- c(
  "Posiadanie piłki",
  "Sytuacje bramkowe",
  "Strzały na bramkę",
  "Strzały niecelne",
  "Rzuty rożne",
  "Spalone",
  "Interwencje bramkarzy",
  "Faule",
  "Podania celne",
  "Ataki",
  "Niebezpieczne ataki",
  "Żółte kartki",
  "Czerwone kartki",
  "Rzuty wolne",
  "Wrzuty z autu",
  "Podania"
)
# for (m in subSiteUrl) {
#   print(m)
#   remDr$navigate(m)
#   obj1 <- remDr$findElements(using = "class name", "stat__categoryName")
#   K <- c(K, sapply(obj1, FUN = function(x) {x$getElementText()}) %>% unlist())
# }
for (m in subSiteUrl) {
  print(m)                    # sometimes selenium breaks because of cookies it is possible to just start the loop again
  remDr$navigate(m)           # begining at which(subSiteUrl == m) no issues should be present
  #if (which(subSiteUrl == m) == 1) {remDr$findElement(using = "id", "onetrust-reject-all-handler")$clickElement()} #this clicks cookies
  
  Sys.sleep(.05) # this makes it so that the site always has the time to compile javascript code
  obj1 <- remDr$findElements(using = "class name", "stat__category")
  if (length(obj1) == 0) {
    obj1 <- remDr$findElements(using = "class name", "stat__category") # sometimes you need to double click
  }
  outcome <- strsplit(x = (obj1[[1]]$getTitle())[[1]], split = "|", fixed = TRUE)[[1]]
  outcome1 <- suppressWarnings((outcome[1] %>% strsplit(split = ""))[[1]] %>% as.numeric())
  matchNames <- c(matchNames, outcome[2] %>% str_trim())
  outcome1 <- outcome1[!is.na(outcome1)]
  valuesHome <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__homeValue")$getElementText()}) %>% unlist()
  names(valuesHome) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  if (any(!(names(valuesHome) %in% statNames))) {stop("bad names")}
  v1 <- rep(NA, length(statNames))
  names(v1) <- statNames
  v1[names(v1) %in% names(valuesHome)][names(valuesHome)] <- valuesHome
  valuesHome <- v1
  valuesAway <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__awayValue")$getElementText()}) %>% unlist()
  names(valuesAway) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  v1 <- rep(NA, length(statNames))
  names(v1) <- statNames
  v1[names(v1) %in% names(valuesAway)][names(valuesAway)] <- valuesAway
  valuesAway <- v1
  participants <- stringr::str_trim(strsplit(outcome[2], split = "-")[[1]], side = "both")
  if (length(participants) == 3) {                                                 # fix for podbeskidzie bielsko biała and Bruk-Bet Termalica Nieciecza
    if ("B" %in% participants) {                                                   #              (podbeskidzie B-B)               (Bruk-Bet T.)
      if (which(participants == "B") == 3) {                                
        participants <- c(participants[1], paste0(participants[2], participants[3]))
      } else if (which(participants == "B") == 2) {
        participants <- c(paste0(participants[1], participants[2]), participants[3])
      } else {
        stop("Check participants")
      }
    } else if ("Bruk" %in% participants) {
      if (which(participants == "Bet T.") == 3) {                                         
        participants <- c(participants[1], paste0(participants[2], participants[3]))
      } else if (which(participants == "Bet T.") == 2) {
        participants <- c(paste0(participants[1], participants[2]), participants[3])
      } else {
        stop("Check participants")
      }
    } else {
      stop("Check participants")
    }
  } else if (length(participants) == 4) {
    if (all(c("Bruk", "B") %in% participants)) {
      participants <- c(paste0(participants[1], participants[2]), participants[3:4])
      participants <- c(participants[1], paste0(participants[2], participants[3]))
    } else {
      stop("Check participants")
    }
  }
  
  if (is.null(K)) {
    K <- c(valuesHome[statNames], valuesAway[statNames],
           outcome1[1], outcome1[2], participants,
           ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
           remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]]) %>%
      data.frame()
  } else {
    K <- data.frame(
      K,
      c(valuesHome[statNames], valuesAway[statNames],
        outcome1[1], outcome1[2], participants,
        ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
        remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]])
    )
  }
}

K <- t(K)
colnames(K) <- c(paste0(statNames, " Gospodarz"), paste0(statNames, " Gość"), "Gole Gospodarz", "Gole Gość", "Gospodarz", "Gość", "Wynik", "Data")
rownames(K) <- matchNames
scrapedData2019and2020 <- K
save(scrapedData2019and2020, file = "data/scrapedData2019and2020.Rdata")

# 2020/2021 ####
url <- "https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2020-2021/wyniki/"

seleniumServer <- rsDriver(browser = "chrome",
                           #verbose = FALSE, 
                           chromever = "105.0.5195.52", # the lastest
                           port = free_port())
# Client object
remDr <- seleniumServer$client

remDr$open()
remDr$maxWindowSize()
remDr$navigate(url)
# click on cookies info:
cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
cookies$clickElement()
# click on more info:
moreInfo <- remDr$findElement(using = "xpath", '//*[@id="live-table"]/div[1]/div/div/a')
moreInfo$clickElement()
Sys.sleep(.5)
moreInfo$clickElement()
# get urls to match statistics
obj <- remDr$findElement(using = "xpath", '//*[@id="live-table"]/div[1]/div/div')
subSiteUrl <- 
  sapply(obj$findChildElements(using = "class name", "event__match"), FUN = function(x) {x$getElementAttribute("id")}) %>%
  unlist()
subSiteUrl <- sapply(subSiteUrl, FUN = function(x) {substr(x, start = 5, stop = 12)})
subSiteUrl <- paste0("https://www.wynikinazywo.pl/mecz/", subSiteUrl, "/#/szczegoly-meczu/statystyki-meczu/0")

# navigate to match statistics:

K <- NULL
matchNames <- NULL
statNames <- c(
  "Posiadanie piłki",
  "Sytuacje bramkowe",
  "Strzały na bramkę",
  "Strzały niecelne",
  "Rzuty rożne",
  "Spalone",
  "Interwencje bramkarzy",
  "Faule",
  "Podania celne",
  "Ataki",
  "Niebezpieczne ataki",
  "Żółte kartki",
  "Czerwone kartki",
  "Strzały zablokowane"
)
# for (m in subSiteUrl) {
#   print(m)
#   remDr$navigate(m)
#   obj1 <- remDr$findElements(using = "class name", "stat__categoryName")
#   K <- c(K, sapply(obj1, FUN = function(x) {x$getElementText()}) %>% unlist())
# }
for (m in subSiteUrl) {# Not all maches in 2012/2013 have match statistics this is for ones with them
  print(m)                    # sometimes selenium breaks because of cookies it is possible to just start the loop again
  remDr$navigate(m)           # begining at which(subSiteUrl == m) no issues should be present
  #if (which(subSiteUrl == m) == 1) {remDr$findElement(using = "id", "onetrust-reject-all-handler")$clickElement()} #this clicks cookies
  
  Sys.sleep(.05) # this makes it so that the site always has the time to compile javascript code
  obj1 <- remDr$findElements(using = "class name", "stat__category")
  if (length(obj1) == 0) {
    obj1 <- remDr$findElements(using = "class name", "stat__category") # sometimes you need to double click
  }
  outcome <- strsplit(x = (obj1[[1]]$getTitle())[[1]], split = "|", fixed = TRUE)[[1]]
  outcome1 <- suppressWarnings((outcome[1] %>% strsplit(split = ""))[[1]] %>% as.numeric())
  matchNames <- c(matchNames, outcome[2] %>% str_trim())
  outcome1 <- outcome1[!is.na(outcome1)]
  valuesHome <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__homeValue")$getElementText()}) %>% unlist()
  names(valuesHome) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  if (any(!(names(valuesHome) %in% statNames))) {stop("bad names")}
  v1 <- rep(NA, length(statNames))
  names(v1) <- statNames
  v1[names(v1) %in% names(valuesHome)][names(valuesHome)] <- valuesHome
  valuesHome <- v1
  valuesAway <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__awayValue")$getElementText()}) %>% unlist()
  names(valuesAway) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  v1 <- rep(NA, length(statNames))
  names(v1) <- statNames
  v1[names(v1) %in% names(valuesAway)][names(valuesAway)] <- valuesAway
  valuesAway <- v1
  participants <- stringr::str_trim(strsplit(outcome[2], split = "-")[[1]], side = "both")
  if (length(participants) == 3) {                                                 # fix for podbeskidzie bielsko biała and Bruk-Bet Termalica Nieciecza
    if ("B" %in% participants) {                                                   #              (podbeskidzie B-B)               (Bruk-Bet T.)
      if (which(participants == "B") == 3) {                                
        participants <- c(participants[1], paste0(participants[2], participants[3]))
      } else if (which(participants == "B") == 2) {
        participants <- c(paste0(participants[1], participants[2]), participants[3])
      } else {
        stop("Check participants")
      }
    } else if ("Bruk" %in% participants) {
      if (which(participants == "Bet T.") == 3) {                                         
        participants <- c(participants[1], paste0(participants[2], participants[3]))
      } else if (which(participants == "Bet T.") == 2) {
        participants <- c(paste0(participants[1], participants[2]), participants[3])
      } else {
        stop("Check participants")
      }
    } else {
      stop("Check participants")
    }
  } else if (length(participants) == 4) {
    if (all(c("Bruk", "B") %in% participants)) {
      participants <- c(paste0(participants[1], participants[2]), participants[3:4])
      participants <- c(participants[1], paste0(participants[2], participants[3]))
    } else {
      stop("Check participants")
    }
  }
  
  if (is.null(K)) {
    K <- c(valuesHome[statNames], valuesAway[statNames],
           outcome1[1], outcome1[2], participants,
           ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
           remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]]) %>%
      data.frame()
  } else {
    K <- data.frame(
      K,
      c(valuesHome[statNames], valuesAway[statNames],
        outcome1[1], outcome1[2], participants,
        ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
        remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]])
    )
  }
}

K <- t(K)
colnames(K) <- c(paste0(statNames, " Gospodarz"), paste0(statNames, " Gość"), "Gole Gospodarz", "Gole Gość", "Gospodarz", "Gość", "Wynik", "Data")
rownames(K) <- matchNames
scrapedData2020and2021 <- K
save(scrapedData2020and2021, file = "data/scrapedData2020and2021.Rdata")

# 2021/2022 ####
url <- "https://www.wyniki.pl/pko-bp-ekstraklasa-2021-2022/wyniki"

seleniumServer <- rsDriver(browser = "chrome",
                           #verbose = FALSE, 
                           chromever = "105.0.5195.52", # the lastest
                           port = free_port())
# Client object
remDr <- seleniumServer$client

remDr$open()
remDr$maxWindowSize()
remDr$navigate(url)
# click on cookies info:
cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
cookies$clickElement()
# click on more info:
moreInfo <- remDr$findElement(using = "xpath", '//*[@id="live-table"]/div[1]/div/div/a')
moreInfo$clickElement()
moreInfo$clickElement()
moreInfo$clickElement()
# get urls to match statistics
obj <- remDr$findElement(using = "xpath", '//*[@id="live-table"]/div[1]/div/div')
subSiteUrl <- 
  sapply(obj$findChildElements(using = "class name", "event__match"), FUN = function(x) {x$getElementAttribute("id")}) %>%
  unlist()
subSiteUrl <- sapply(subSiteUrl, FUN = function(x) {substr(x, start = 5, stop = 12)})
subSiteUrl <- paste0("https://www.wynikinazywo.pl/mecz/", subSiteUrl, "/#/szczegoly-meczu/statystyki-meczu/0")

# navigate to match statistics:

K <- NULL
matchNames <- NULL
statNames <- c(
  "Posiadanie piłki",
  "Sytuacje bramkowe",
  "Strzały na bramkę",
  "Strzały niecelne",
  "Strzały zablokowane",
  "Rzuty wolne",
  "Rzuty rożne",
  "Spalone",
  "Wrzuty z autu",
  "Interwencje bramkarzy",
  "Faule",
  "Żółte kartki",
  "Podania",
  "Podania celne",
  "Bloki",
  "Ataki",
  "Niebezpieczne ataki",
  "Czerwone kartki",
  "Skuteczność podań"        
)
# for (m in subSiteUrl) {
#   print(m)
#   remDr$navigate(m)
#   obj1 <- remDr$findElements(using = "class name", "stat__categoryName")
#   K <- c(K, sapply(obj1, FUN = function(x) {x$getElementText()}) %>% unlist())
# }
for (m in subSiteUrl) {
  print(m)                    # sometimes selenium breaks because of cookies it is possible to just start the loop again
  remDr$navigate(m)           # begining at which(subSiteUrl == m) no issues should be present
  #if (which(subSiteUrl == m) == 1) {remDr$findElement(using = "id", "onetrust-reject-all-handler")$clickElement()} #this clicks cookies
  
  Sys.sleep(.05) # this makes it so that the site always has the time to compile javascript code
  obj1 <- remDr$findElements(using = "class name", "stat__category")
  if (length(obj1) == 0) {
    obj1 <- remDr$findElements(using = "class name", "stat__category") # sometimes you need to double click
  }
  outcome <- strsplit(x = (obj1[[1]]$getTitle())[[1]], split = "|", fixed = TRUE)[[1]]
  outcome1 <- suppressWarnings((outcome[1] %>% strsplit(split = ""))[[1]] %>% as.numeric())
  matchNames <- c(matchNames, outcome[2] %>% str_trim())
  outcome1 <- outcome1[!is.na(outcome1)]
  valuesHome <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__homeValue")$getElementText()}) %>% unlist()
  names(valuesHome) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  if (any(!(names(valuesHome) %in% statNames))) {stop("bad names")}
  v1 <- rep(NA, length(statNames))
  names(v1) <- statNames
  v1[names(v1) %in% names(valuesHome)][names(valuesHome)] <- valuesHome
  valuesHome <- v1
  valuesAway <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__awayValue")$getElementText()}) %>% unlist()
  names(valuesAway) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  v1 <- rep(NA, length(statNames))
  names(v1) <- statNames
  v1[names(v1) %in% names(valuesAway)][names(valuesAway)] <- valuesAway
  valuesAway <- v1
  participants <- stringr::str_trim(strsplit(outcome[2], split = "-")[[1]], side = "both")
  if (length(participants) == 3) {                                                 # fix for podbeskidzie bielsko biała and Bruk-Bet Termalica Nieciecza
    if ("B" %in% participants) {                                                   #              (podbeskidzie B-B)               (Bruk-Bet T.)
      if (which(participants == "B") == 3) {                                
        participants <- c(participants[1], paste0(participants[2], participants[3]))
      } else if (which(participants == "B") == 2) {
        participants <- c(paste0(participants[1], participants[2]), participants[3])
      } else {
        stop("Check participants")
      }
    } else if ("Bruk" %in% participants) {
      if (which(participants == "Bet T.") == 3) {                                         
        participants <- c(participants[1], paste0(participants[2], participants[3]))
      } else if (which(participants == "Bet T.") == 2) {
        participants <- c(paste0(participants[1], participants[2]), participants[3])
      } else {
        stop("Check participants")
      }
    } else {
      stop("Check participants")
    }
  } else if (length(participants) == 4) {
    if (all(c("Bruk", "B") %in% participants)) {
      participants <- c(paste0(participants[1], participants[2]), participants[3:4])
      participants <- c(participants[1], paste0(participants[2], participants[3]))
    } else {
      stop("Check participants")
    }
  }
  
  if (is.null(K)) {
    K <- c(valuesHome[statNames], valuesAway[statNames],
           outcome1[1], outcome1[2], participants,
           ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
           remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]]) %>%
      data.frame()
  } else {
    K <- data.frame(
      K,
      c(valuesHome[statNames], valuesAway[statNames],
        outcome1[1], outcome1[2], participants,
        ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
        remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]])
    )
  }
}

K <- t(K)
colnames(K) <- c(paste0(statNames, " Gospodarz"), paste0(statNames, " Gość"), "Gole Gospodarz", "Gole Gość", "Gospodarz", "Gość", "Wynik", "Data")
rownames(K) <- matchNames
scrapedData2021and2022 <- K
save(scrapedData2021and2022, file = "data/scrapedData2021and2022.Rdata")

# 2022/2023 ####
url <- "https://www.wyniki.pl/pko-bp-ekstraklasa-2022-2023/wyniki/"

seleniumServer <- rsDriver(browser = "firefox",
                           #verbose = FALSE,
                           port = free_port(),
                           chromever = "107.0.5304.18")
# Client object
remDr <- seleniumServer$client

remDr$maxWindowSize()
remDr$navigate(url)
# click on cookies info:
cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
cookies$clickElement()
# click on more info:
moreInfo <- remDr$findElement(using = "xpath", '//*[@id="live-table"]/div[1]/div/div/a')
moreInfo$clickElement()
moreInfo$clickElement() # may possibly cause an error if 
# get urls to match statistics
obj <- remDr$findElement(using = "xpath", '//*[@id="live-table"]/div[1]/div/div')
subSiteUrl <- 
  sapply(obj$findChildElements(using = "class name", "event__match"), FUN = function(x) {x$getElementAttribute("id")}) %>%
  unlist()
subSiteUrl <- sapply(subSiteUrl, FUN = function(x) {substr(x, start = 5, stop = 12)})
subSiteUrl <- paste0("https://www.wyniki.pl/mecz/", subSiteUrl, "/#/szczegoly-meczu/statystyki-meczu/0")

# navigate to match statistics:

K <- NULL
matchNames <- NULL
statNames <- c(
  "Posiadanie piłki",
  "Sytuacje bramkowe",
  "Strzały na bramkę",
  "Strzały niecelne",
  "Strzały zablokowane",
  "Rzuty wolne",
  "Rzuty rożne",
  "Spalone",
  "Wrzuty z autu",
  "Interwencje bramkarzy",
  "Faule",
  "Żółte kartki",
  "Podania",
  "Podania celne",
  "Bloki",
  "Ataki",
  "Niebezpieczne ataki",
  "Czerwone kartki"      
)
# for (m in subSiteUrl) {
#   print(m)
#   remDr$navigate(m)
#   obj1 <- remDr$findElements(using = "class name", "stat__categoryName")
#   K <- c(K, sapply(obj1, FUN = function(x) {x$getElementText()}) %>% unlist())
# }
for (m in subSiteUrl) {
#for (m in subSiteUrl[which(subSiteUrl == m):length(subSiteUrl)]) {
  print(m)                    # sometimes selenium breaks because of cookies it is possible to just start the loop again
  remDr$navigate(m)           # begining at which(subSiteUrl == m) no issues should be present
  #if (which(subSiteUrl == m) == 1) {remDr$findElement(using = "id", "onetrust-reject-all-handler")$clickElement()}
  Sys.sleep(1.2)
  obj1 <- remDr$findElements(using = "class name", "section")
  while (!length(obj1)) {
    obj1 <- remDr$findElements(using = "class name", "section")
  }
  
  # for (k in 1:length(obj1)) {
  #   obj1[[k]] <- obj1[[k]]$findChildElement(#using = "class name", "_category_lq1k0_16")
  #     using = "xpath", "/html/body/div[1]/div/div[9]/div[2]")
  # }
  outcome <- strsplit(x = (obj1[[1]]$getTitle())[[1]], split = "|", fixed = TRUE)[[1]]
  outcome1 <- suppressWarnings((outcome[1] %>% strsplit(split = ""))[[1]] %>% as.numeric())
  matchNames <- c(matchNames, outcome[2] %>% str_trim())
  outcome1 <- outcome1[!is.na(outcome1)]
  xxx <- sapply(obj1[[1]]$findChildElements(using = "class name", "_row_lq1k0_9"), FUN = function(x) {(x$getElementText() %>% str_split(patter = "\n") %>% unlist())})
  if (!length(xxx)) {
    matchNames <- matchNames[1:(length(matchNames)-1)]
  }
  valuesHome <- xxx[1,]
  names(valuesHome) <- xxx[2,]
  if (any(!(names(valuesHome) %in% statNames))) {stop("bad names")}
  v1 <- rep(NA, length(statNames))
  names(v1) <- statNames
  v1[names(v1) %in% names(valuesHome)][names(valuesHome)] <- valuesHome
  valuesHome <- v1
  valuesAway <- xxx[3,]
  names(valuesAway) <- xxx[2,]
  v1 <- rep(NA, length(statNames))
  names(v1) <- statNames
  v1[names(v1) %in% names(valuesAway)][names(valuesAway)] <- valuesAway
  valuesAway <- v1
  participants <- stringr::str_trim(strsplit(outcome[2], split = "-")[[1]], side = "both")
  if (length(participants) == 3) {                                                 # fix for podbeskidzie bielsko biała and Bruk-Bet Termalica Nieciecza
    if ("B" %in% participants) {                                                   #              (podbeskidzie B-B)               (Bruk-Bet T.)
      if (which(participants == "B") == 3) {                                
        participants <- c(participants[1], paste0(participants[2], participants[3]))
      } else if (which(participants == "B") == 2) {
        participants <- c(paste0(participants[1], participants[2]), participants[3])
      } else {
        stop("Check participants")
      }
    } else if ("Bruk" %in% participants) {
      if (which(participants == "Bet T.") == 3) {                                         
        participants <- c(participants[1], paste0(participants[2], participants[3]))
      } else if (which(participants == "Bet T.") == 2) {
        participants <- c(paste0(participants[1], participants[2]), participants[3])
      } else {
        stop("Check participants")
      }
    } else {
      stop("Check participants")
    }
  } else if (length(participants) == 4) {
    if (all(c("Bruk", "B") %in% participants)) {
      participants <- c(paste0(participants[1], participants[2]), participants[3:4])
      participants <- c(participants[1], paste0(participants[2], participants[3]))
    } else {
      stop("Check participants")
    }
  }
  
  if (is.null(K)) {
    K <- c(valuesHome[statNames], valuesAway[statNames],
           outcome1[1], outcome1[2], participants,
           ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
           remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]]) %>%
      data.frame()
  } else {
    K <- data.frame(
      K,
      c(valuesHome[statNames], valuesAway[statNames],
        outcome1[1], outcome1[2], participants,
        ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
        remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]])
    )
  }
}

K <- t(K)
colnames(K) <- c(paste0(statNames, " Gospodarz"), paste0(statNames, " Gość"), "Gole Gospodarz", "Gole Gość", "Gospodarz", "Gość", "Wynik", "Data")
rownames(K) <- matchNames
scrapedData2022and2023 <- K
save(scrapedData2022and2023, file = "data/scrapedData2022and2023.Rdata")


# 2023/2024 ####
url <- "https://www.wyniki.pl/pko-bp-ekstraklasa-2023-2024/wyniki/"

seleniumServer <- rsDriver(browser = "firefox",
                           #verbose = FALSE, 
                           #chromever = "131.0.6778.265", # the lastest
                           port = free_port())
# Client object
remDr <- seleniumServer$client

remDr$maxWindowSize()
remDr$navigate(url)
# click on cookies info:
cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
cookies$clickElement()
# click on more info:
moreInfo <- remDr$findElement(using = "xpath", '//*[@id="live-table"]/div[1]/div/div/a')
moreInfo$clickElement()
moreInfo$clickElement() # may possibly cause an error if 
# get urls to match statistics
obj <- remDr$findElement(using = "xpath", '//*[@id="live-table"]/div[1]/div/div')
subSiteUrl <- 
  sapply(obj$findChildElements(using = "class name", "event__match"), FUN = function(x) {x$getElementAttribute("id")}) %>%
  unlist()
subSiteUrl <- sapply(subSiteUrl, FUN = function(x) {substr(x, start = 5, stop = 12)})
subSiteUrl <- paste0("https://www.wyniki.pl/mecz/", subSiteUrl, "/#/szczegoly-meczu/statystyki-meczu/0")

# navigate to match statistics:

K <- NULL
matchNames <- NULL
statNames <- c(
  "Posiadanie piłki",
  "Sytuacje bramkowe",
  "Strzały na bramkę",
  "Strzały niecelne",
  "Strzały zablokowane",
  "Rzuty wolne",
  "Rzuty rożne",
  "Spalone",
  "Wrzuty z autu",
  "Interwencje bramkarzy",
  "Faule",
  "Żółte kartki",
  "Podania",
  "Podania celne",
  "Bloki",
  "Ataki",
  "Niebezpieczne ataki",
  "Czerwone kartki",
  "Wykonane dośrodkowania",
  "Pokonany dystans (km)",
  "Oczekiwane bramki (xG)"
)
# for (m in subSiteUrl) {
#   print(m)
#   remDr$navigate(m)
#   obj1 <- remDr$findElements(using = "class name", "stat__categoryName")
#   K <- c(K, sapply(obj1, FUN = function(x) {x$getElementText()}) %>% unlist())
# }
for (m in subSiteUrl) {
#for (m in subSiteUrl[which(subSiteUrl == m):length(subSiteUrl)]) {
  print(m)                    # sometimes selenium breaks because of cookies it is possible to just start the loop again
  remDr$navigate(m)           # begining at which(subSiteUrl == m) no issues should be present
  #if (which(subSiteUrl == m) == 1) {remDr$findElement(using = "id", "onetrust-reject-all-handler")$clickElement()} #this clicks cookies
  Sys.sleep(2.5)
  obj1 <- remDr$findElements(using = "class name", "section")
  while (!length(obj1)) {
    obj1 <- remDr$findElements(using = "class name", "section")
  }
  
  # for (k in 1:length(obj1)) {
  #   obj1[[k]] <- obj1[[k]]$findChildElement(#using = "class name", "_category_lq1k0_16")
  #     using = "xpath", "/html/body/div[1]/div/div[9]/div[2]")
  # }
  outcome <- strsplit(x = (obj1[[1]]$getTitle())[[1]], split = "|", fixed = TRUE)[[1]]
  outcome1 <- suppressWarnings((outcome[1] %>% strsplit(split = ""))[[1]] %>% as.numeric())
  outcome1 <- outcome1[!is.na(outcome1)]
  xxx <- sapply(
    obj1[[1]]$findChildElements(using = "class name", "wcl-row_OFViZ"), #_row_n1rcj_9"
    FUN = function(x) {(x$getElementText() %>% str_split(patter = "\n") %>% unlist())}
  )
  if (!length(xxx)) {
    matchNames <- matchNames[1:(length(matchNames)-1)]
  }
  valuesHome <- xxx[1,]
  names(valuesHome) <- xxx[2,]
  #if (any(!(names(valuesHome) %in% statNames))) {stop("bad names")}
  v1 <- rep(NA, length(statNames))
  names(v1) <- statNames
  v1[names(v1) %in% names(valuesHome)][names(valuesHome)] <- valuesHome
  valuesHome <- v1
  valuesAway <- xxx[3,]
  names(valuesAway) <- xxx[2,]
  v1 <- rep(NA, length(statNames))
  names(v1) <- statNames
  v1[names(v1) %in% names(valuesAway)][names(valuesAway)] <- valuesAway
  valuesAway <- v1
  participants <- stringr::str_trim(strsplit(outcome[2], split = "-")[[1]], side = "both")
  if (length(participants) == 3) {                                                 # fix for podbeskidzie bielsko biała and Bruk-Bet Termalica Nieciecza
    if ("B" %in% participants) {                                                   #              (podbeskidzie B-B)               (Bruk-Bet T.)
      if (which(participants == "B") == 3) {                                
        participants <- c(participants[1], paste0(participants[2], participants[3]))
      } else if (which(participants == "B") == 2) {
        participants <- c(paste0(participants[1], participants[2]), participants[3])
      } else {
        stop("Check participants")
      }
    } else if ("Bruk" %in% participants) {
      if (which(participants == "Bet T.") == 3) {                                         
        participants <- c(participants[1], paste0(participants[2], participants[3]))
      } else if (which(participants == "Bet T.") == 2) {
        participants <- c(paste0(participants[1], participants[2]), participants[3])
      } else {
        stop("Check participants")
      }
    } else {
      stop("Check participants")
    }
  } else if (length(participants) == 4) {
    if (all(c("Bruk", "B") %in% participants)) {
      participants <- c(paste0(participants[1], participants[2]), participants[3:4])
      participants <- c(participants[1], paste0(participants[2], participants[3]))
    } else {
      stop("Check participants")
    }
  }
  
  matchNames <- c(matchNames, outcome[2] %>% str_trim())
  if (is.null(K)) {
    K <- c(valuesHome[statNames], valuesAway[statNames],
           outcome1[1], outcome1[2], participants,
           ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
           remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]]) %>%
      data.frame()
  } else {
    K <- data.frame(
      K,
      c(valuesHome[statNames], valuesAway[statNames],
        outcome1[1], outcome1[2], participants,
        ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
        remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]])
    )
  }
}

K <- t(K)
colnames(K) <- c(paste0(statNames, " Gospodarz"), paste0(statNames, " Gość"), "Gole Gospodarz", "Gole Gość", "Gospodarz", "Gość", "Wynik", "Data")
rownames(K) <- matchNames
scrapedData2023and2024 <- K
save(scrapedData2023and2024, file = "data/scrapedData2023and2024.Rdata")

# 2024/2025 ####
url <- "https://www.wyniki.pl/pko-bp-ekstraklasa-2024-2025/wyniki/"

seleniumServer <- rsDriver(browser = "firefox",
                           #verbose = FALSE, 
                           #chromever = "131.0.6778.265", # the lastest
                           port = free_port())
# Client object
remDr <- seleniumServer$client

remDr$maxWindowSize()
remDr$navigate(url)
# click on cookies info:
cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
cookies$clickElement()
# click on more info:
moreInfo <- remDr$findElement(using = "xpath", '//*[@id="live-table"]/div[1]/div/div/a')
moreInfo$clickElement()
#moreInfo$clickElement() # may possibly cause an error if 
# get urls to match statistics
obj <- remDr$findElement(using = "xpath", '//*[@id="live-table"]/div[1]/div/div')
subSiteUrl <- 
  sapply(obj$findChildElements(using = "class name", "event__match"), FUN = function(x) {x$getElementAttribute("id")}) %>%
  unlist()
subSiteUrl <- sapply(subSiteUrl, FUN = function(x) {substr(x, start = 5, stop = 12)})
subSiteUrl <- paste0("https://www.wyniki.pl/mecz/", subSiteUrl, "/#/szczegoly-meczu/statystyki-meczu/0")

# navigate to match statistics:

K <- NULL
matchNames <- NULL
statNames <- c(
  "Posiadanie piłki",
  "Sytuacje bramkowe",
  "Strzały na bramkę",
  "Strzały niecelne",
  "Strzały zablokowane",
  "Rzuty wolne",
  "Rzuty rożne",
  "Spalone",
  "Wrzuty z autu",
  "Interwencje bramkarzy",
  "Faule",
  "Żółte kartki",
  "Podania",
  "Podania celne",
  "Bloki",
  "Ataki",
  "Niebezpieczne ataki",
  "Czerwone kartki",
  "Wykonane dośrodkowania",
  "Pokonany dystans (km)",
  "Oczekiwane bramki (xG)"
)
# for (m in subSiteUrl) {
#   print(m)
#   remDr$navigate(m)
#   obj1 <- remDr$findElements(using = "class name", "stat__categoryName")
#   K <- c(K, sapply(obj1, FUN = function(x) {x$getElementText()}) %>% unlist())
# }
for (m in subSiteUrl) {
  #for (m in subSiteUrl[which(subSiteUrl == m):length(subSiteUrl)]) {
  print(m)                    # sometimes selenium breaks because of cookies it is possible to just start the loop again
  remDr$navigate(m)           # begining at which(subSiteUrl == m) no issues should be present
  #if (which(subSiteUrl == m) == 1) {remDr$findElement(using = "id", "onetrust-reject-all-handler")$clickElement()} #this clicks cookies
  Sys.sleep(2.5)
  obj1 <- remDr$findElements(using = "class name", "section")
  while (!length(obj1)) {
    obj1 <- remDr$findElements(using = "class name", "section")
  }
  
  # for (k in 1:length(obj1)) {
  #   obj1[[k]] <- obj1[[k]]$findChildElement(#using = "class name", "_category_lq1k0_16")
  #     using = "xpath", "/html/body/div[1]/div/div[9]/div[2]")
  # }
  outcome <- strsplit(x = (obj1[[1]]$getTitle())[[1]], split = "|", fixed = TRUE)[[1]]
  outcome1 <- suppressWarnings((outcome[1] %>% strsplit(split = ""))[[1]] %>% as.numeric())
  outcome1 <- outcome1[!is.na(outcome1)]
  xxx <- sapply(
    obj1[[1]]$findChildElements(using = "class name", "wcl-row_OFViZ"), #_row_n1rcj_9"
    FUN = function(x) {(x$getElementText() %>% str_split(patter = "\n") %>% unlist())}
  )
  if (!length(xxx)) {
    matchNames <- matchNames[1:(length(matchNames)-1)]
  }
  valuesHome <- xxx[1,]
  names(valuesHome) <- xxx[2,]
  #if (any(!(names(valuesHome) %in% statNames))) {stop("bad names")}
  v1 <- rep(NA, length(statNames))
  names(v1) <- statNames
  v1[names(v1) %in% names(valuesHome)][names(valuesHome)] <- valuesHome
  valuesHome <- v1
  valuesAway <- xxx[3,]
  names(valuesAway) <- xxx[2,]
  v1 <- rep(NA, length(statNames))
  names(v1) <- statNames
  v1[names(v1) %in% names(valuesAway)][names(valuesAway)] <- valuesAway
  valuesAway <- v1
  participants <- stringr::str_trim(strsplit(outcome[2], split = "-")[[1]], side = "both")
  if (length(participants) == 3) {                                                 # fix for podbeskidzie bielsko biała and Bruk-Bet Termalica Nieciecza
    if ("B" %in% participants) {                                                   #              (podbeskidzie B-B)               (Bruk-Bet T.)
      if (which(participants == "B") == 3) {                                
        participants <- c(participants[1], paste0(participants[2], participants[3]))
      } else if (which(participants == "B") == 2) {
        participants <- c(paste0(participants[1], participants[2]), participants[3])
      } else {
        stop("Check participants")
      }
    } else if ("Bruk" %in% participants) {
      if (which(participants == "Bet T.") == 3) {                                         
        participants <- c(participants[1], paste0(participants[2], participants[3]))
      } else if (which(participants == "Bet T.") == 2) {
        participants <- c(paste0(participants[1], participants[2]), participants[3])
      } else {
        stop("Check participants")
      }
    } else {
      stop("Check participants")
    }
  } else if (length(participants) == 4) {
    if (all(c("Bruk", "B") %in% participants)) {
      participants <- c(paste0(participants[1], participants[2]), participants[3:4])
      participants <- c(participants[1], paste0(participants[2], participants[3]))
    } else {
      stop("Check participants")
    }
  }
  
  matchNames <- c(matchNames, outcome[2] %>% str_trim())
  if (is.null(K)) {
    K <- c(valuesHome[statNames], valuesAway[statNames],
           outcome1[1], outcome1[2], participants,
           ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
           remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]]) %>%
      data.frame()
  } else {
    K <- data.frame(
      K,
      c(valuesHome[statNames], valuesAway[statNames],
        outcome1[1], outcome1[2], participants,
        ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
        remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]])
    )
  }
}

K <- t(K)
colnames(K) <- c(paste0(statNames, " Gospodarz"), paste0(statNames, " Gość"), "Gole Gospodarz", "Gole Gość", "Gospodarz", "Gość", "Wynik", "Data")
rownames(K) <- matchNames
scrapedData2024and2025 <- K
save(scrapedData2024and2025, file = "data/scrapedData2024and2025.Rdata")
