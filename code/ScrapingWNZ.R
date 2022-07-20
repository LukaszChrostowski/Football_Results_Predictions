## Scraping from https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2012-2013/wyniki/

#install.packages("RSelenium") you also need to install java for this
#install.packages("netstat") only one function is needed

library(tidyverse)
library(RSelenium)
library(netstat)

url <- "https://www.wynikinazywo.pl/pko-bp-ekstraklasa-2012-2013/wyniki/"

seleniumServer <- rsDriver(browser = "chrome",
                           #verbose = FALSE, 
                           chromever = "103.0.5060.134", # the lastest
                           port = free_port())
# Cilint object
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

remDr$navigate(subSiteUrl[1])

obj1 <- remDr$findElements(using = "class name", "stat__category")

data.frame(
  home = sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__homeValue")$getElementText()}) %>% unlist(),
  name = sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__categoryName")$getElementText()}) %>% unlist(),
  away = sapply(obj1, FUN = function(x) {x$findChildElement(using = "class name", "stat__awayValue")$getElementText()}) %>% unlist()
)

# Tomorrow I'll do this for all elements in 2012 and hopefully other seasons won't be too difficult