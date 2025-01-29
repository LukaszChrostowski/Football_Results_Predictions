# load packages
library(tidyverse)
library(RSelenium)
library(netstat)

# start the server
rs_driver_object <- rsDriver(browser = 'chrome',
                             chromever = '105.0.5195.52',
                             verbose = FALSE,
                             port = free_port())

# create a client object
remDr <- rs_driver_object$client

# open a browser
remDr$open()

# maximize window
remDr$maxWindowSize()



urls <- list("https://www.flashscore.pl/pilka-nozna/polska/pko-bp-ekstraklasa-2012-2013/wyniki/",
             "https://www.flashscore.pl/pilka-nozna/polska/pko-bp-ekstraklasa-2013-2014/wyniki/",
             "https://www.flashscore.pl/pilka-nozna/polska/pko-bp-ekstraklasa-2014-2015/wyniki/",
             "https://www.flashscore.pl/pilka-nozna/polska/pko-bp-ekstraklasa-2015-2016/wyniki/",
             "https://www.flashscore.pl/pilka-nozna/polska/pko-bp-ekstraklasa-2016-2017/wyniki/",
             "https://www.flashscore.pl/pilka-nozna/polska/pko-bp-ekstraklasa-2017-2018/wyniki/",
             "https://www.flashscore.pl/pilka-nozna/polska/pko-bp-ekstraklasa-2018-2019/wyniki/",
             "https://www.flashscore.pl/pilka-nozna/polska/pko-bp-ekstraklasa-2019-2020/wyniki/",
             "https://www.flashscore.pl/pilka-nozna/polska/pko-bp-ekstraklasa-2020-2021/wyniki/",
             "https://www.flashscore.pl/pilka-nozna/polska/pko-bp-ekstraklasa-2021-2022/wyniki/",
             "https://www.flashscore.pl/pilka-nozna/polska/pko-bp-ekstraklasa-2022-2023/wyniki/")

walkovers_number <- vector(mode = "numeric", length = length(urls))


for(i in 1:length(walkovers_number)){
  
  remDr$navigate(urls[[i]])
  
  
  walkovers <- remDr$findElements(using = "class name", "event__stage")
  
  
  all_walkovers <- lapply(walkovers, function(x) x$getElementText()) %>% unlist()


  walkovers_number[i] <- length(all_walkovers)  
  
}

Sezon <- c("2012/13", "2013/14", "2014/15", "2015/16", "2016/17",
           "2017/18" ,"2018/19", "2019/20", "2020/21", "2021/22",
           "2022/23")

walkowers_df <- data.frame(Sezon = Sezon, walkovers_number = walkovers_number)
