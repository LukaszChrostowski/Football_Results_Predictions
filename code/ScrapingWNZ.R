## Scraping from https://www.wynikinazywo.pl/

#install.packages("RSelenium") you also need to install java for this
#install.packages("netstat") only one function is needed

# TODO::
## - make this script fancy by making it into gigantic loop
## - determine the source of equivalence of obj1 and list()

# for now this is now very elegant and likely no correct but will be improved in the future

library(tidyverse)
library(RSelenium)
library(netstat)

# 2012/2013 ####

url <- "https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2012-2013/wyniki/"

seleniumServer <- rsDriver(browser = "chrome",
                           #verbose = FALSE, 
                           chromever = "103.0.5060.134", # the lastest
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
  print(m)                    # sometimes selenium breajs because of cookies it is possible to just start the loop again
  remDr$navigate(m)           # beggining at which(subSiteUrl == m) no issues should be present
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
  v1 <- rep(0, length(statNames))
  names(v1) <- statNames
  v1[names(v1) %in% names(valuesHome)] <- valuesHome
  valuesHome <- v1
  valuesAway <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__awayValue")$getElementText()}) %>% unlist()
  names(valuesAway) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  v1 <- rep(0, length(statNames))
  names(v1) <- statNames
  v1[names(v1) %in% names(valuesAway)] <- valuesAway
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
    K <- c(valuesHome, valuesAway,
           outcome1[1], outcome1[2], participants,
           ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
           remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]]) %>%
      data.frame()
  } else {
    K <- data.frame(
      K,
      c(valuesHome, valuesAway,
        outcome1[1], outcome1[2], participants,
        ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
        remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]])
    )
  }
}

K <- t(K)
colnames(K) <- c(paste0(statNames, " Gospodarz"), paste0(statNames, " Gość"), "Gole Gospodarz", "Gole Gość", "Gospodarz", "Gość", "Wynik", "Data")
rownames(K) <- matchNames

# 2013/2014 ####

url <- "https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2013-2014/wyniki/"

seleniumServer <- rsDriver(browser = "chrome",
                           #verbose = FALSE, 
                           chromever = "103.0.5060.134", # the lastest
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
for (m in subSiteUrl) {# Not all maches in 2012/2013 have match statistics this is for ones with them
  print(m)                    # sometimes selenium breajs because of cookies it is possible to just start the loop again
  remDr$navigate(m)           # beggining at which(subSiteUrl == m) no issues should be present
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
  v1 <- rep(0, length(statNames))
  names(v1) <- statNames
  v1[names(v1) %in% names(valuesHome)] <- valuesHome
  valuesHome <- v1
  valuesAway <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__awayValue")$getElementText()}) %>% unlist()
  names(valuesAway) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  v1 <- rep(0, length(statNames))
  names(v1) <- statNames
  v1[names(v1) %in% names(valuesAway)] <- valuesAway
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
    K <- c(valuesHome, valuesAway,
           outcome1[1], outcome1[2], participants,
           ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
           remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]]) %>%
      data.frame()
  } else {
    K <- data.frame(
      K,
      c(valuesHome, valuesAway,
        outcome1[1], outcome1[2], participants,
        ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
        remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]])
    )
  }
}

K <- t(K)
colnames(K) <- c(paste0(statNames, " Gospodarz"), paste0(statNames, " Gość"), "Gole Gospodarz", "Gole Gość", "Gospodarz", "Gość", "Wynik", "Data")
rownames(K) <- matchNames


# 2014/2015 ####

url <- "https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2014-2015/wyniki/"

seleniumServer <- rsDriver(browser = "chrome",
                           #verbose = FALSE, 
                           chromever = "103.0.5060.134", # the lastest
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
for (m in subSiteUrl) {# Not all maches in 2012/2013 have match statistics this is for ones with them
  print(m)                    # sometimes selenium breajs because of cookies it is possible to just start the loop again
  remDr$navigate(m)           # beggining at which(subSiteUrl == m) no issues should be present
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
  valuesHome <- ifelse(statNames %in% names(valuesHome), valuesHome, 0)
  valuesAway <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__awayValue")$getElementText()}) %>% unlist()
  names(valuesAway) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  valuesAway <- ifelse(statNames %in% names(valuesAway), valuesAway, 0)
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
    K <- c(valuesHome, valuesAway,
           outcome1[1], outcome1[2], participants,
           ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
           remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]]) %>%
      data.frame()
  } else {
    K <- data.frame(
      K,
      c(valuesHome, valuesAway,
        outcome1[1], outcome1[2], participants,
        ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
        remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]])
    )
  }
}

K <- t(K)
colnames(K) <- c(paste0(statNames, " Gospodarz"), paste0(statNames, " Gość"), "Gole Gospodarz", "Gole Gość", "Gospodarz", "Gość", "Wynik", "Data")
rownames(K) <- matchNames

# 2015/2016 ####

url <- "https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2015-2016/wyniki/"

seleniumServer <- rsDriver(browser = "chrome",
                           #verbose = FALSE, 
                           chromever = "103.0.5060.134", # the lastest
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
for (m in subSiteUrl) {# Not all maches in 2012/2013 have match statistics this is for ones with them
  print(m)                    # sometimes selenium breajs because of cookies it is possible to just start the loop again
  remDr$navigate(m)           # beggining at which(subSiteUrl == m) no issues should be present
  #if (which(subSiteUrl == m) == 1) {remDr$findElement(using = "id", "onetrust-reject-all-handler")$clickElement()} #this clicks cookies
  
  Sys.sleep(.05) # this makes it so that the site always has the time to compile javascript code
  obj1 <- remDr$findElements(using = "class name", "stat__category")
  outcome <- strsplit(x = (obj1[[1]]$getTitle())[[1]], split = "|", fixed = TRUE)[[1]]
  outcome1 <- suppressWarnings((outcome[1] %>% strsplit(split = ""))[[1]] %>% as.numeric())
  matchNames <- c(matchNames, outcome[2] %>% str_trim())
  outcome1 <- outcome1[!is.na(outcome1)]
  valuesHome <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__homeValue")$getElementText()}) %>% unlist()
  names(valuesHome) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  valuesHome <- ifelse(statNames %in% names(valuesHome), valuesHome, 0)
  valuesAway <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__awayValue")$getElementText()}) %>% unlist()
  names(valuesAway) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  valuesAway <- ifelse(statNames %in% names(valuesAway), valuesAway, 0)
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
    K <- c(valuesHome, valuesAway,
           outcome1[1], outcome1[2], participants,
           ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
           remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]]) %>%
      data.frame()
  } else {
    K <- data.frame(
      K,
      c(valuesHome, valuesAway,
        outcome1[1], outcome1[2], participants,
        ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
        remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]])
    )
  }
}

K <- t(K)
colnames(K) <- c(paste0(statNames, " Gospodarz"), paste0(statNames, " Gość"), "Gole Gospodarz", "Gole Gość", "Gospodarz", "Gość", "Wynik", "Data")
rownames(K) <- matchNames

# 2016/2017 ####
url <- "https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2016-2017/wyniki/"

seleniumServer <- rsDriver(browser = "chrome",
                           #verbose = FALSE, 
                           chromever = "103.0.5060.134", # the lastest
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
for (m in subSiteUrl) {# Not all maches in 2012/2013 have match statistics this is for ones with them
  print(m)                    # sometimes selenium breajs because of cookies it is possible to just start the loop again
  remDr$navigate(m)           # beggining at which(subSiteUrl == m) no issues should be present
  #if (which(subSiteUrl == m) == 1) {remDr$findElement(using = "id", "onetrust-reject-all-handler")$clickElement()} #this clicks cookies
  
  Sys.sleep(.05) # this makes it so that the site always has the time to compile javascript code
  obj1 <- remDr$findElements(using = "class name", "stat__category")
  outcome <- strsplit(x = (obj1[[1]]$getTitle())[[1]], split = "|", fixed = TRUE)[[1]]
  outcome1 <- suppressWarnings((outcome[1] %>% strsplit(split = ""))[[1]] %>% as.numeric())
  matchNames <- c(matchNames, outcome[2] %>% str_trim())
  outcome1 <- outcome1[!is.na(outcome1)]
  valuesHome <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__homeValue")$getElementText()}) %>% unlist()
  names(valuesHome) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  valuesHome <- ifelse(statNames %in% names(valuesHome), valuesHome, 0)
  valuesAway <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__awayValue")$getElementText()}) %>% unlist()
  names(valuesAway) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  valuesAway <- ifelse(statNames %in% names(valuesAway), valuesAway, 0)
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
    K <- c(valuesHome, valuesAway,
           outcome1[1], outcome1[2], participants,
           ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
           remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]]) %>%
      data.frame()
  } else {
    K <- data.frame(
      K,
      c(valuesHome, valuesAway,
        outcome1[1], outcome1[2], participants,
        ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
        remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]])
    )
  }
}

K <- t(K)
colnames(K) <- c(paste0(statNames, " Gospodarz"), paste0(statNames, " Gość"), "Gole Gospodarz", "Gole Gość", "Gospodarz", "Gość", "Wynik", "Data")
rownames(K) <- matchNames

# 2017/2018 ####
url <- "https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2017-2018/wyniki/"

seleniumServer <- rsDriver(browser = "chrome",
                           #verbose = FALSE, 
                           chromever = "103.0.5060.134", # the lastest
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
for (m in subSiteUrl) {# Not all maches in 2012/2013 have match statistics this is for ones with them
  print(m)                    # sometimes selenium breajs because of cookies it is possible to just start the loop again
  remDr$navigate(m)           # beggining at which(subSiteUrl == m) no issues should be present
  #if (which(subSiteUrl == m) == 1) {remDr$findElement(using = "id", "onetrust-reject-all-handler")$clickElement()} #this clicks cookies
  
  Sys.sleep(.05) # this makes it so that the site always has the time to compile javascript code
  obj1 <- remDr$findElements(using = "class name", "stat__category")
  outcome <- strsplit(x = (obj1[[1]]$getTitle())[[1]], split = "|", fixed = TRUE)[[1]]
  outcome1 <- suppressWarnings((outcome[1] %>% strsplit(split = ""))[[1]] %>% as.numeric())
  matchNames <- c(matchNames, outcome[2] %>% str_trim())
  outcome1 <- outcome1[!is.na(outcome1)]
  valuesHome <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__homeValue")$getElementText()}) %>% unlist()
  names(valuesHome) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  valuesHome <- ifelse(statNames %in% names(valuesHome), valuesHome, 0)
  valuesAway <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__awayValue")$getElementText()}) %>% unlist()
  names(valuesAway) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  valuesAway <- ifelse(statNames %in% names(valuesAway), valuesAway, 0)
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
    K <- c(valuesHome, valuesAway,
           outcome1[1], outcome1[2], participants,
           ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
           remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]]) %>%
      data.frame()
  } else {
    K <- data.frame(
      K,
      c(valuesHome, valuesAway,
        outcome1[1], outcome1[2], participants,
        ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
        remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]])
    )
  }
}

K <- t(K)
colnames(K) <- c(paste0(statNames, " Gospodarz"), paste0(statNames, " Gość"), "Gole Gospodarz", "Gole Gość", "Gospodarz", "Gość", "Wynik", "Data")
rownames(K) <- matchNames

# 2018/2019####

url <- "https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2018-2019/wyniki/"

seleniumServer <- rsDriver(browser = "chrome",
                           #verbose = FALSE, 
                           chromever = "103.0.5060.134", # the lastest
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
for (m in subSiteUrl) {# Not all maches in 2012/2013 have match statistics this is for ones with them
  print(m)                    # sometimes selenium breajs because of cookies it is possible to just start the loop again
  remDr$navigate(m)           # beggining at which(subSiteUrl == m) no issues should be present
  #if (which(subSiteUrl == m) == 1) {remDr$findElement(using = "id", "onetrust-reject-all-handler")$clickElement()} #this clicks cookies
  
  Sys.sleep(.05) # this makes it so that the site always has the time to compile javascript code
  obj1 <- remDr$findElements(using = "class name", "stat__category")
  outcome <- strsplit(x = (obj1[[1]]$getTitle())[[1]], split = "|", fixed = TRUE)[[1]]
  outcome1 <- suppressWarnings((outcome[1] %>% strsplit(split = ""))[[1]] %>% as.numeric())
  matchNames <- c(matchNames, outcome[2] %>% str_trim())
  outcome1 <- outcome1[!is.na(outcome1)]
  valuesHome <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__homeValue")$getElementText()}) %>% unlist()
  names(valuesHome) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  valuesHome <- ifelse(statNames %in% names(valuesHome), valuesHome, 0)
  valuesAway <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__awayValue")$getElementText()}) %>% unlist()
  names(valuesAway) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  valuesAway <- ifelse(statNames %in% names(valuesAway), valuesAway, 0)
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
    K <- c(valuesHome, valuesAway,
           outcome1[1], outcome1[2], participants,
           ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
           remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]]) %>%
      data.frame()
  } else {
    K <- data.frame(
      K,
      c(valuesHome, valuesAway,
        outcome1[1], outcome1[2], participants,
        ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
        remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]])
    )
  }
}

K <- t(K)
colnames(K) <- c(paste0(statNames, " Gospodarz"), paste0(statNames, " Gość"), "Gole Gospodarz", "Gole Gość", "Gospodarz", "Gość", "Wynik", "Data")
rownames(K) <- matchNames


# 2019/2020####

url <- "https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2019-2020/wyniki/"

seleniumServer <- rsDriver(browser = "chrome",
                           #verbose = FALSE, 
                           chromever = "103.0.5060.134", # the lastest
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
  "Czerwone kartki"         
)
# for (m in subSiteUrl) {
#   print(m)
#   remDr$navigate(m)
#   obj1 <- remDr$findElements(using = "class name", "stat__categoryName")
#   K <- c(K, sapply(obj1, FUN = function(x) {x$getElementText()}) %>% unlist())
# }
for (m in subSiteUrl) {# Not all maches in 2012/2013 have match statistics this is for ones with them
  print(m)                    # sometimes selenium breajs because of cookies it is possible to just start the loop again
  remDr$navigate(m)           # beggining at which(subSiteUrl == m) no issues should be present
  #if (which(subSiteUrl == m) == 1) {remDr$findElement(using = "id", "onetrust-reject-all-handler")$clickElement()} #this clicks cookies
  
  Sys.sleep(.05) # this makes it so that the site always has the time to compile javascript code
  obj1 <- remDr$findElements(using = "class name", "stat__category")
  outcome <- strsplit(x = (obj1[[1]]$getTitle())[[1]], split = "|", fixed = TRUE)[[1]]
  outcome1 <- suppressWarnings((outcome[1] %>% strsplit(split = ""))[[1]] %>% as.numeric())
  matchNames <- c(matchNames, outcome[2] %>% str_trim())
  outcome1 <- outcome1[!is.na(outcome1)]
  valuesHome <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__homeValue")$getElementText()}) %>% unlist()
  names(valuesHome) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  valuesHome <- ifelse(statNames %in% names(valuesHome), valuesHome, 0)
  valuesAway <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__awayValue")$getElementText()}) %>% unlist()
  names(valuesAway) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  valuesAway <- ifelse(statNames %in% names(valuesAway), valuesAway, 0)
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
    K <- c(valuesHome, valuesAway,
           outcome1[1], outcome1[2], participants,
           ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
           remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]]) %>%
      data.frame()
  } else {
    K <- data.frame(
      K,
      c(valuesHome, valuesAway,
        outcome1[1], outcome1[2], participants,
        ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
        remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]])
    )
  }
}

K <- t(K)
colnames(K) <- c(paste0(statNames, " Gospodarz"), paste0(statNames, " Gość"), "Gole Gospodarz", "Gole Gość", "Gospodarz", "Gość", "Wynik", "Data")
rownames(K) <- matchNames

# 2020/2021####
url <- "https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2020-2021/wyniki/"

seleniumServer <- rsDriver(browser = "chrome",
                           #verbose = FALSE, 
                           chromever = "103.0.5060.134", # the lastest
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
  "Czerwone kartki"         
)
# for (m in subSiteUrl) {
#   print(m)
#   remDr$navigate(m)
#   obj1 <- remDr$findElements(using = "class name", "stat__categoryName")
#   K <- c(K, sapply(obj1, FUN = function(x) {x$getElementText()}) %>% unlist())
# }
for (m in subSiteUrl) {# Not all maches in 2012/2013 have match statistics this is for ones with them
  print(m)                    # sometimes selenium breajs because of cookies it is possible to just start the loop again
  remDr$navigate(m)           # beggining at which(subSiteUrl == m) no issues should be present
  #if (which(subSiteUrl == m) == 1) {remDr$findElement(using = "id", "onetrust-reject-all-handler")$clickElement()} #this clicks cookies
  
  Sys.sleep(.05) # this makes it so that the site always has the time to compile javascript code
  obj1 <- remDr$findElements(using = "class name", "stat__category")
  outcome <- strsplit(x = (obj1[[1]]$getTitle())[[1]], split = "|", fixed = TRUE)[[1]]
  outcome1 <- suppressWarnings((outcome[1] %>% strsplit(split = ""))[[1]] %>% as.numeric())
  matchNames <- c(matchNames, outcome[2] %>% str_trim())
  outcome1 <- outcome1[!is.na(outcome1)]
  valuesHome <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__homeValue")$getElementText()}) %>% unlist()
  names(valuesHome) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  valuesHome <- ifelse(statNames %in% names(valuesHome), valuesHome, 0)
  valuesAway <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__awayValue")$getElementText()}) %>% unlist()
  names(valuesAway) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  valuesAway <- ifelse(statNames %in% names(valuesAway), valuesAway, 0)
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
    K <- c(valuesHome, valuesAway,
           outcome1[1], outcome1[2], participants,
           ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
           remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]]) %>%
      data.frame()
  } else {
    K <- data.frame(
      K,
      c(valuesHome, valuesAway,
        outcome1[1], outcome1[2], participants,
        ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
        remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]])
    )
  }
}

K <- t(K)
colnames(K) <- c(paste0(statNames, " Gospodarz"), paste0(statNames, " Gość"), "Gole Gospodarz", "Gole Gość", "Gospodarz", "Gość", "Wynik", "Data")
rownames(K) <- matchNames
# 2021/2022 ####
url <- "https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2021-2022/wyniki/"

seleniumServer <- rsDriver(browser = "chrome",
                           #verbose = FALSE, 
                           chromever = "103.0.5060.134", # the lastest
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
for (m in subSiteUrl) {# Not all maches in 2012/2013 have match statistics this is for ones with them
  print(m)                    # sometimes selenium breajs because of cookies it is possible to just start the loop again
  remDr$navigate(m)           # beggining at which(subSiteUrl == m) no issues should be present
  #if (which(subSiteUrl == m) == 1) {remDr$findElement(using = "id", "onetrust-reject-all-handler")$clickElement()} #this clicks cookies
  
  Sys.sleep(.05) # this makes it so that the site always has the time to compile javascript code
  obj1 <- remDr$findElements(using = "class name", "stat__category")
  outcome <- strsplit(x = (obj1[[1]]$getTitle())[[1]], split = "|", fixed = TRUE)[[1]]
  outcome1 <- suppressWarnings((outcome[1] %>% strsplit(split = ""))[[1]] %>% as.numeric())
  matchNames <- c(matchNames, outcome[2] %>% str_trim())
  outcome1 <- outcome1[!is.na(outcome1)]
  valuesHome <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__homeValue")$getElementText()}) %>% unlist()
  names(valuesHome) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  valuesHome <- ifelse(statNames %in% names(valuesHome), valuesHome, 0)
  valuesAway <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__awayValue")$getElementText()}) %>% unlist()
  names(valuesAway) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  valuesAway <- ifelse(statNames %in% names(valuesAway), valuesAway, 0)
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
    K <- c(valuesHome, valuesAway,
           outcome1[1], outcome1[2], participants,
           ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
           remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]]) %>%
      data.frame()
  } else {
    K <- data.frame(
      K,
      c(valuesHome, valuesAway,
        outcome1[1], outcome1[2], participants,
        ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
        remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]])
    )
  }
}

K <- t(K)
colnames(K) <- c(paste0(statNames, " Gospodarz"), paste0(statNames, " Gość"), "Gole Gospodarz", "Gole Gość", "Gospodarz", "Gość", "Wynik", "Data")
rownames(K) <- matchNames

# 2022/2023 ####
url <- "https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2022-2023/wyniki/"

seleniumServer <- rsDriver(browser = "chrome",
                           #verbose = FALSE, 
                           chromever = "103.0.5060.134", # the lastest
                           port = free_port())
# Client object
remDr <- seleniumServer$client

remDr$open()
remDr$maxWindowSize()
remDr$navigate(url)
# click on cookies info:
cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
cookies$clickElement()
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
  "Czerwone kartki"      
)
# for (m in subSiteUrl) {
#   print(m)
#   remDr$navigate(m)
#   obj1 <- remDr$findElements(using = "class name", "stat__categoryName")
#   K <- c(K, sapply(obj1, FUN = function(x) {x$getElementText()}) %>% unlist())
# }
for (m in subSiteUrl) {# Not all maches in 2012/2013 have match statistics this is for ones with them
  print(m)                    # sometimes selenium breajs because of cookies it is possible to just start the loop again
  remDr$navigate(m)           # beggining at which(subSiteUrl == m) no issues should be present
  #if (which(subSiteUrl == m) == 1) {remDr$findElement(using = "id", "onetrust-reject-all-handler")$clickElement()} #this clicks cookies
  
  Sys.sleep(.05) # this makes it so that the site always has the time to compile javascript code
  obj1 <- remDr$findElements(using = "class name", "stat__category")
  outcome <- strsplit(x = (obj1[[1]]$getTitle())[[1]], split = "|", fixed = TRUE)[[1]]
  outcome1 <- suppressWarnings((outcome[1] %>% strsplit(split = ""))[[1]] %>% as.numeric())
  matchNames <- c(matchNames, outcome[2] %>% str_trim())
  outcome1 <- outcome1[!is.na(outcome1)]
  valuesHome <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__homeValue")$getElementText()}) %>% unlist()
  names(valuesHome) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  valuesHome <- ifelse(statNames %in% names(valuesHome), valuesHome, 0)
  valuesAway <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__awayValue")$getElementText()}) %>% unlist()
  names(valuesAway) <- sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist()
  valuesAway <- ifelse(statNames %in% names(valuesAway), valuesAway, 0)
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
    K <- c(valuesHome, valuesAway,
           outcome1[1], outcome1[2], participants,
           ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
           remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]]) %>%
      data.frame()
  } else {
    K <- data.frame(
      K,
      c(valuesHome, valuesAway,
        outcome1[1], outcome1[2], participants,
        ifelse(outcome1[1] > outcome1[2], "H", ifelse(outcome1[2] > outcome1[1], "A", "D")),
        remDr$findElement(using = "class name", "duelParticipant__startTime")$getElementText()[[1]])
    )
  }
}

K <- t(K)
colnames(K) <- c(paste0(statNames, " Gospodarz"), paste0(statNames, " Gość"), "Gole Gospodarz", "Gole Gość", "Gospodarz", "Gość", "Wynik", "Data")
rownames(K) <- matchNames
